{-# LANGUAGE OverloadedStrings #-}

module CodeGen.CGMain where

import CoreAST.CoreExpr
import CoreAST.Types
import CoreAST.Kind
import CoreAST.Var
import CoreAST.TScheme
import CoreAST.Literal
import CoreAST.DataCon as DC

import CodeGen.JVMVersion
import CodeGen.TypeToJType
import CodeGen.JTypes
import CodeGen.JInterfaces
import CodeGen.CGMonad
import CodeGen.CGLit
import CodeGen.CodeGen
import CodeGen.CGHelper

import Codec.JVM
import Codec.JVM.Opcode (athrow)
import Codec.JVM.Const (cstring)

import Control.Monad (liftM)
import Data.Text (Text,unpack)
import Data.String (fromString)
import Data.Char (ord)
import Data.Int (Int32)
import Data.Monoid ((<>))
import Data.Foldable (foldrM)

import Data.String.Utils (startswith)


-- Returns the var that has been looked up and the path to get the variable.
lookupVar :: [ScopeVar] -> -- Path end
             Text       -> -- Var name
             [Scope]    -> -- Scope
             (ScopeVar,[ScopeVar])
lookupVar path name [] = error $ "Not found var " ++ show name ++ " on path " ++ show (reverse path)
lookupVar path name ((Scope sv scope):ss) = if not (null search) then (head search, reverse (head search : path')) else lookupVar path' name ss
  where path' = sv : path
        search = filter (\(ScopeVar n _) -> n == name) scope

buildPath :: [ScopeVar] -> Code
buildPath []               = error "No path"
buildPath [(ScopeVar n t)] = getfield (mkFieldRef n n t)
buildPath [(ScopeVar n1 t1),(ScopeVar n2 t2)] = getfield (mkFieldRef n1 n2 t2)
buildPath ((ScopeVar n1 t1):sv2@(ScopeVar n2 t2):svs) = getfield (mkFieldRef n1 n2 t2) <> buildPath (sv2:svs)


cgEnv :: CoreExprDefs -> CG ()
cgEnv c =
  do let sv = (ScopeVar envName (obj envName))
     scopeMap <- mapM (\(ExprDef b _) -> scopeMap b) c
     setScope [Scope sv scopeMap]
     envMap <- mapM cgEnvMap c
     mapM_ mkEnvSuppliers envMap
     mkEnvClassFile $ map (\(a,b,_) -> (a,b)) envMap

scopeMap :: Binder -> CG ScopeVar
scopeMap b =
  do let bn = fromString $ varName b
     return (ScopeVar bn supplierInterfaceType)

mkEnvSuppliers :: (Text,Code,CoreExpr) -> CG ()
mkEnvSuppliers (text,code,e) =
  do let envField = mkFieldDef [Public] envName envType
         ctr = mkConstructorDef text jobjectC [envType] (gload jobject 0
                                                      <> gload jobject 1
                                                      <> putfield (mkFieldRef text envName (obj envName)))
     c <- unwrapMany jobject (greturn jobject)
     let getMethodCode = code <> c
         get = mkMethodDef text [Public] supplierName [] (ret jobject) getMethodCode
     logClass text $ mkClassFileV lamAccessors text Nothing [supplierInterfaceName] [envField] [ctr,get]


mkEnvClassFile :: [(Text,Code)] -> CG ()
mkEnvClassFile topLevel =
  do let fields  = map (\(t,_) -> mkFieldDef [Public] t supplierInterfaceType) topLevel
         ctr     = mkConstructorDef envName jobjectC []
                      (mconcat ( map
                        (\(t,c) ->
                                gload jobject 0
                             <> new (obj t)
                             <> dup (obj t)
                             <> gload jobject 0
                             <> invokespecial (mkMethodRef t "<init>" [envType] void)
                             <> putfield (mkFieldRef envName t supplierInterfaceType )) topLevel))
     let after = unsafePeformIO
              <> invokevirtual (mkMethodRef "java/io/PrintStream" "println" [jobject] void)
              <> vreturn
     c <- unwrapMany jobject after
     let mCode =   (  getstatic (mkFieldRef "java/lang/System" "out" (obj "java/io/PrintStream"))
                    <> new (obj envName)
                    <> dup (obj envName)
                    <> invokespecial (mkMethodRef envName "<init>" [] void)
                    <> getfield (mkFieldRef envName mainName supplierInterfaceType)
                    <> c)
         mainFunction = mkMethodDef envName [Public,Static] "main" [] void mCode
     logClass envName $ mkClassFileV lamAccessors envName Nothing [] fields [ctr,mainFunction]

unsafePeformIO :: Code
unsafePeformIO = gconv jobject ioJType
              <> invokeinterface (mkMethodRef ioName "unsafePerformIO" [] (ret jobject))

cgEnvMap :: CoreExprDef -> CG (Text,Code,CoreExpr)
cgEnvMap (ExprDef b e) =
  do let (MkVar { varType = TScheme _ t }) = b
         name = (fromString . varName) b
         sv = ScopeVar name (obj name)
         env = ScopeVar envName envType
     scope <- getScope
     updateScope (Scope sv [env])
     (code,_) <- cgExpr e
     setScope scope
     return (name,code,e)

isRestricted :: CoreExpr -> Bool
isRestricted (Lam (MkVar _ _) _) = False
isRestricted (Lam (MkTVar _ ) e) = isRestricted e
isRestricted _                   = True

isThunkName :: JType -> Bool
isThunkName (ObjectType (IClassName t))  = startswith "THUNK" (unpack t)
isThunkName _ = False

isApp :: CoreExpr -> Bool
isApp (Lam (MkTVar _) e) = isApp e
isApp (App e (Type _)) = isApp e
isApp (App _ _) = True
isApp _         = False

unwrapMany :: FieldType -> Code -> CG Code
unwrapMany ft code =
  do fInt <- getFreshInt
     let label = mkLabel fInt
     return (startLabel label
         <> dup ft
         <> ginstanceof supplierInterfaceType
         <> ifeq code (invokeSupplier <> goto label))



cgExpr :: CodeGen CoreExpr

cgExpr (Lit lit) = cgLit lit

-- creates class
-- public class NAME(n) implements Function
--   ty(t) arg;
--   Parent p;
--   NAME(n) (Parent p)
--     this.p = p
--
--   protected ty(e) (ty(b) arg)
--     this.arg = arg
--     return [[e]];
--
-- returns a new instance of the above class

cgExpr l@(Lam (MkVar { varName = n, varType = TScheme [] bt } ) e) =
  do (pname,ptype) <- getParent
     let fnName = fromString n
         fnType = obj fnName
         tyB = jobject
         sc = (ScopeVar fnName fnType)
         inner = (ScopeVar fnName tyB)
     updateScope (Scope sc [inner])
     (body, (tyE,n)) <- cgExpr e
     b <- unwrapMany tyE mempty
     let body'    = body <> b
         lamClass = mkLambdaClass fnName pname tyB tyE ptype body'
     logClass fnName lamClass
     let retCode = newDup fnType
                <> gload ptype 0
                <> gconv jobject ptype
                <> invokespecial (mkMethodRef fnName "<init>" [ptype] void)
     return (retCode,(fnType,0))

cgExpr (Let (ExprDef b e1) e2) =
  do scope <- getScope
     ret <- cgExpr (App (Lam b e2) e1)
     setScope scope
     return ret


cgExpr (Lam (MkTVar _) e) = cgExpr e

cgExpr (App e (Type _)) = cgExpr e

cgExpr (Type _) = error "Cannot generate bytecode for type"

cgExpr (App e1 e2) =
  do thunkName <- newFunName "THUNK"
     (pname,ptype) <- getParent
     let scopeVar = ScopeVar thunkName (obj thunkName)
     updateScope (Scope scopeVar [])
     s1 <- getScope
     (c1,(jt1,n1)) <- cgExpr e1
     setScope s1
     (c2,(jt2,n2)) <- cgExpr e2
     tc <- (unwrapMany jt1 (c2 <> invokeFunction))
     let thunkCode = c1 <> tc
         thunk = mkThunk thunkName (obj thunkName) pname ptype thunkCode
     logClass thunkName thunk
     let retCode = (newDup (obj thunkName)
                <> gload ptype 0
                <> gconv jobject ptype
                <> invokespecial (mkMethodRef thunkName "<init>" [ptype] void))
     --
     return (retCode,(obj thunkName,1))


cgExpr (Case e t alts) =
  do scope <- getScope
     (code,(obj,n)) <- cgExpr e
     setScope scope
     altsCode <- foldrM caseMap raiseMatchError alts
     return (code <> altsCode,(toJType t,1))

cgExpr (Var i) =
  do let v = (fromString i)
     lookupResult <- getPreDefinedFunction v
     case lookupResult of
       Just x   -> cgBuildIn x
       Nothing -> cgNormalVar v

cgNormalVar :: CodeGen Text
cgNormalVar i = do
  (s@(ScopeVar n ft),path) <- liftM (lookupVar [] i) getScope
  scope <- getScope
  return (gload jobject 0 <> buildPath path,(ft,0))

cgBuildIn :: CodeGen (Text,Bool)
cgBuildIn (fnName,False) =
  do let fnType  = obj fnName
         retCode = (new fnType
                <> dup fnType
                <> invokespecial (mkMethodRef fnName "<init>" [] void))
     return (retCode,(fnType,0))

cgBuildIn (fnName,True) =
  do let fnType  = obj fnName
     return (newDup objThunkType
          <> newDup fnType
          <> invokespecial (mkMethodRef fnName "<init>" [] void)
          <> invokespecial objThunkConstructor,(supplierInterfaceType,1))

caseMap :: (Alt Binder) -> Code -> CG Code
caseMap alts code =
  do scope <- getScope
     c     <- cgAlt alts code
     setScope scope
     return c


raiseMatchError :: Code
raiseMatchError =
     invokevirtual (mkMethodRef "java/lang/Object" "toString" [] (ret $ obj "java/lang/String"))
  <> new runTimeExcpType
  <> dup_x1 jstring runTimeExcpType
  <> swap runTimeExcpType jstring
  <> invokespecial (mkMethodRef runTimeException "<init>" [stringType] void)
  <> gthrow runTimeExcpType


-- This doesn't work need to apply arg
cgAlt :: Alt Binder -> Code -> CG Code
cgAlt (DEFAULT, binders, e) otherBranch =
  do if length binders /= 1 then fail "binder length not 1 for default" else return ()
     let arg = head binders
         binderType = (obj . fromString . varName) arg
     (code,(jt,t)) <- cgExpr (Lam arg e)
     let code' = code
              <> swap binderType supplierInterfaceType
              <> invokeFunction
     return code'

cgAlt (LitAlt l,b,e) otherBranch = cgLitAlt (l,b,e) otherBranch

-- Need to put in the if instance of stuff!

cgAlt (DataAlt dc, binders, e) otherBranch =
  do let (MkDataCon { dName = name, tName = iface, DC.fields = f, conType = t }) = dc
         binderLen = length binders
     (pname,ptype) <- getParent
     envName <- newFunName $ (fromString name) `mappend` (fromString $ concatMap varName binders)
     let envType = obj envName
         envFields = zip (map fromString $ map varName binders) (repeat supplierInterfaceType)
         sc = (ScopeVar envName envType)
         inner = map (\v -> ScopeVar (fromString $ varName v) supplierInterfaceType) binders
     updateScope (Scope sc inner)
     (c,(jt,t)) <- cgExpr e
     let argsCode = foldr (\(n,t) acc ->
                           dup jobject
                        <> gconv jobject (obj $ fromString name)
                        <> getfield (mkFieldRef (fromString name) (fromString $ name ++ (show n)) t)
                        <> swap t jobject
                        <> acc)
                      (pop (obj (fromString name))) (zip [0..] (map snd envFields))
         thisBranch  =
                new envType
             <> dup_x1 jobject envType
             <> swap envType jobject
             <> argsCode
             <> gload ptype 0
             <> gconv jobject ptype
             <> invokespecial (mkMethodRef envName "<init>" (replicate binderLen supplierInterfaceType ++ [ptype]) void)
             <> invokevirtual (mkMethodRef envName runMethod [] (ret jobject))
             <> greturn jobject
     let code = dup jobject
             <> ginstanceof (obj $ fromString name)
             <> ifeq otherBranch thisBranch
     c' <- unwrapMany jobject code
     return c'
  where tyOf (TScheme _ t) = t

mkScopeClass :: Text           ->  -- Name
                [(Text,JType)] ->  -- Fields
                Code           ->  -- Body
                ClassFile

mkScopeClass name f code = mkClassFileV lamAccessors name Nothing [] fields methods
  where
    fields = map (\(n,t) -> mkFieldDef [Protected] n t) f
    classType = obj name
    methods = [mkConstructorDef name jobjectC (map snd f)
                  (foldr (\((n,nt),num) acc ->
                     dup classType
                  <> gload nt num
                  <> putfield (mkFieldRef name n nt)
                  <> acc) mempty (zip f [1..])),
                  mkMethodDef name [Public] runMethod [] (ret jobject) (
                    code
                  <> greturn jobject)
              ]

runMethod = "__run__"

cgLitAlt :: (Literal,[Binder],CoreExpr) -> Code -> CG Code
cgLitAlt (LitInt i,bs,e) otherBranch =
  cgGLit (fromInteger i) integerFieldName jIntType jint jIntValue e otherBranch

cgLitAlt (LitChar c,bs,e) otherBranch =
  cgGLit (fromIntegral $ ord c) charFieldName jCharType jchar jCharValue e otherBranch

cgGLit :: Int32     ->  -- value
          Text      ->  -- lit boxed name
          FieldType ->  -- lit boxed type
          FieldType ->  -- prim type
          Text      ->  -- getter name
          CoreExpr  ->  -- branch expr
          Code      ->  -- other branch
          CG Code

cgGLit value boxedName boxedType primType getterName expr otherBranch =
  do (thisBranch,(jt,t)) <- cgExpr expr
     let code = dup jobject
           <> gconv jobject boxedType
           <> invokevirtual (mkMethodRef boxedName getterName [] (ret primType))
           <> iconst primType value
           <> if_icmpeq thisBranch otherBranch
     unwrapMany jobject code

mkName s i = fromString s `mappend` (fromString . show) i



