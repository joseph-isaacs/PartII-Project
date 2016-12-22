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
import Data.Text (Text)
import Data.String (fromString)
import Data.Char (ord)
import Data.Int (Int32)
import Data.Monoid ((<>))
import Data.Foldable (foldrM)


-- Returns the var that has been looked up and the path to get the variable.
lookupVar :: [ScopeVar] -> -- Path end
             Text       -> -- Var name
             [Scope]    -> -- Scope
             (ScopeVar,[ScopeVar])
lookupVar path name [] = error $ "Not found var " ++ show name ++ " on path " ++ show (reverse path)
lookupVar path name ((Scope sv scope):ss) = if not (null search) then (head search, reverse (head search : path')) else lookupVar path' name ss
  where path' = sv : path
        search = filter (\(ScopeVar n _ _) -> n == name) scope

buildPath :: [ScopeVar] -> Code
buildPath []               = error "No path"
buildPath [(ScopeVar n t _)] = getfield (mkFieldRef n n t)
buildPath [(ScopeVar n1 t1 _),(ScopeVar n2 t2 _)] = getfield (mkFieldRef n1 n2 t2)
buildPath ((ScopeVar n1 t1 _):sv2@(ScopeVar n2 t2 _):svs) = getfield (mkFieldRef n1 n2 t2) <> buildPath (sv2:svs)

envName :: Text
envName = "Env"

cgEnv :: CoreExprDefs -> CG ()
cgEnv c =
  do let sv = (ScopeVar envName (obj envName) (TGen 0))
     scopeMap <- mapM (\(ExprDef b _) -> scopeMap b) c
     setScope [Scope sv scopeMap]
     envMap <- mapM cgEnvMap c
     mapM_ mkEnvSuppliers envMap
     mkEnvClassFile $ map (\(a,b,_) -> (a,b)) envMap

scopeMap :: Binder -> CG ScopeVar
scopeMap b =
  do let (MkVar { varName = n, varType = TScheme _ t }) = b
         bn = fromString n
     return (ScopeVar bn supplierInterfaceType t)

mkEnvSuppliers :: (Text,Code,Bool) -> CG ()
mkEnvSuppliers (text,code,restricted) = logClass text $
  mkClassFileV lamAccessors text Nothing [supplierInterfaceName] [envField] [ctr,get]
  where
    envField = mkFieldDef [Public] envName (obj envName)
    ctr = mkConstructorDef text jobjectC [jobject] (gload jobject 0
                                                 <> gload jobject 1
                                                 <> gconv jobject (obj envName)
                                                 <> putfield (mkFieldRef text envName (obj envName)))
    get = mkMethodDef text [Public] supplierName [] (ret jobject) (
              code
           <> (if restricted then invokeSupplier else mempty)
           <> greturn jobject)


mkEnvClassFile :: [(Text,Code)] -> CG ()
mkEnvClassFile topLevel = logClass envName $
  mkClassFileV lamAccessors envName Nothing [] fields [ctr,mainFunction]
  where
    fields  = map (\(t,_) -> mkFieldDef [Public] t supplierInterfaceType) topLevel
    ctr     = mkConstructorDef envName jobjectC []
              (mconcat ( map
                (\(t,c) ->
                        gload jobject 0
                     <> new (obj t)
                     <> dup (obj t)
                     <> gload jobject 0
                     <> invokespecial (mkMethodRef t "<init>" [jobject] void)
                     <> putfield (mkFieldRef envName t supplierInterfaceType )) topLevel))
    mainFunction = mkMethodDef envName [Public,Static] "main" [jarray jstring] void
                    (  getstatic (mkFieldRef "java/lang/System" "out" (obj "java/io/PrintStream"))
                    <> new (obj envName)
                    <> dup (obj envName)
                    <> invokespecial (mkMethodRef envName "<init>" [] void)
                    <> getfield (mkFieldRef envName mainName supplierInterfaceType)
                    <> invokeSupplier
                    <> unsafePeformIO
                    <> invokevirtual (mkMethodRef "java/io/PrintStream" "println" [jobject] void)
                    <> vreturn)

unsafePeformIO :: Code
unsafePeformIO = gconv jobject ioJType
              <> invokeinterface (mkMethodRef ioName "unsafePerformIO" [] (ret jobject))

cgEnvMap :: CoreExprDef -> CG (Text,Code,Bool)
cgEnvMap (ExprDef b e) =
  do let (MkVar { varType = TScheme _ t }) = b
         name = (fromString . varName) b
         sv = ScopeVar name (obj name) t
         env = ScopeVar envName (obj envName) (TGen 0)
     scope <- getScope
     updateScope (Scope sv [env])
     (code,_) <- cgExpr e
     setScope scope
     return (name,code,isRestricted e)

isRestricted :: CoreExpr -> Bool
isRestricted (Lam (MkVar _ _) _) = False
isRestricted (Lam (MkTVar _ ) e) = isRestricted e
isRestricted _                   = True

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
  do (pname,ptype,pctype) <- getParent
     let fnName = fromString n
         fnType = obj fnName
         tyB = supplierInterfaceType
         sc = (ScopeVar fnName fnType bt)
         inner = (ScopeVar fnName tyB bt)
     updateScope (Scope sc [inner])
     (body, (tyE,t)) <- cgExpr e
     let lamClass = mkLambdaClass fnName pname tyB tyE ptype body
     logClass fnName lamClass
     let retCode = newDup fnType
                <> gload ptype 0
                <> gconv jobject ptype
                <> invokespecial (mkMethodRef fnName "<init>" [ptype] void)
     return (retCode,(fnType,bt `fn` t))

cgExpr (Let (ExprDef b e1) e2) =
  do scope <- getScope
     ret <- cgExpr (App (Lam b e2) e1)
     setScope scope
     return ret


cgExpr (Lam (MkTVar _) e) = cgExpr e

cgExpr (App e (Type _)) = cgExpr e

cgExpr (Type _) = error "Cannot generate bytecode for type"

cgExpr (App e1 e2) =
  do s1 <- getScope
     (c1,(jt1,t1)) <- cgExpr e1
     setScope s1
     (c2,(jt2,t2)) <- cgExpr e2
     let (TAp (TAp tArrow _) retT) = t1
     return (c1
          <> (if jt1 == supplierInterfaceType then  invokeSupplier else mempty)
          <> c2
          <> invokeFunction
          ,(jobject,retT))

cgExpr (Case e t alts) =
  do scope <- getScope
     (code,(obj,t)) <- cgExpr e
     setScope scope
     altsCode <- foldrM caseMap raiseMatchError alts
     return (code <> altsCode,(toJType t,t))

cgExpr (Var i) =
  do let v = (fromString i)
     lookupResult <- getPreDefinedFunction v
     case lookupResult of
       Just x   -> cgBuildIn x
       Nothing -> cgNormalVar v

cgNormalVar :: CodeGen Text
cgNormalVar i = do
  (s@(ScopeVar n ft t),path) <- liftM (lookupVar [] i) getScope
  scope <- getScope
  return (gload jobject 0 <> buildPath path,(ft,t))

cgBuildIn :: CodeGen (Text,Bool)
cgBuildIn (fnName,False) =
  do let fnType  = obj fnName
         retCode = (new fnType
                <> dup fnType
                <> invokespecial (mkMethodRef fnName "<init>" [] void))
     return (retCode,(fnType,TGen 0))

cgBuildIn (fnName,True) =
  do let fnType  = obj fnName
     return (newDup objThunkType
          <> newDup fnType
          <> invokespecial (mkMethodRef fnName "<init>" [] void)
          <> invokespecial objThunkConstructor,(supplierInterfaceType,TGen 0))

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
  -- <> gldc stringType (cstring "match failed exception")
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
     (pname,ptype,pctype) <- getParent
     envName <- newFunName $ (fromString name) `mappend` (fromString $ concatMap varName binders)
     let envType = obj envName
         envFields = zip (map fromString $ map varName binders) (repeat supplierInterfaceType)
         sc = (ScopeVar envName envType (TVar $ Tyvar "e" Star))
         inner = map (\v -> ScopeVar (fromString $ varName v) supplierInterfaceType (tyOf $ varType v)) binders
     updateScope (Scope sc inner)
     (c,(jt,t)) <- cgExpr e
     logClass envName (mkScopeClass envName (envFields ++ [(pname,ptype)]) c)
     let argsCode = foldr (\(n,t) acc ->
                           dup jobject
                        <> invokeSupplier
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
         code = dup jobject
             <> invokeSupplier
             <> ginstanceof (obj $ fromString name)
             <> ifeq otherBranch thisBranch
     return code
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
     return $ dup jobject
           <> invokeSupplier
           <> gconv jobject boxedType
           <> invokevirtual (mkMethodRef boxedName getterName [] (ret primType))
           <> iconst primType value
           <> if_icmpeq thisBranch otherBranch

mkName s i = fromString s `mappend` (fromString . show) i



