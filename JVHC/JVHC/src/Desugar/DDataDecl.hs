module Desugar.DDataDecl where

import Infer.KAssumption
import qualified Infer.Assumption as A
import Infer.Scheme
import Parsing.ParsingAST
import CoreAST.Types
import CoreAST.TScheme
import qualified CoreAST.DataCon as DC
import CoreAST.Kind

import Desugar.DExpr

import Data.List((\\))

import Infer.KIMain
import Infer.KIMonad


dDataDecls :: Monad m => [TDataDecl] -> m [DataType]
dDataDecls dds = do mapM (kTypeToDType kassump) tks
   where
     kassump    = map (makeKind.getSType) dds
     dd         = map dDataDecl  dds
     tks        = runKI $ kiDataDecls kassump dd
     dataAssump = map mkAssump tks
     getSType (TDatadecl s _) = s
     mkAssump (TK { typeName = name, tKind = kind}) = name :>: kind


dDataDecl :: TDataDecl -> DataDecl
dDataDecl (TDatadecl st constrs) = DD st constrs

kTypeToDType :: Monad m => [KAssumption] -> TypeKinds -> m DataType
kTypeToDType ka tk@(TK { typeName = name, tKind = kind, varKinds = var, dd = dataDecl}) =
  do let (DD st ctrs) = dataDecl
     ctr' <- mapM (dConstr ka tk) ctrs
     let (ctr,dcs) = unzip ctr'
         tcon = DC.MkTyCon { DC.mtyCon = Tycon name kind, DC.constrs = dcs }
     return $ DT { dTName = name, dKind = kind, dConstrs = ctr, tCon = tcon}


-- takes a list of all the custom data types, a list of all variables in the current custom data type
-- and a single constructor and returns that assumption of the type for this constructor
dConstr :: Monad m => [KAssumption] -> TypeKinds -> Constr -> m (A.Assumption,DC.DataCon)
dConstr cons (TK { varKinds = vars, typeName = tName}) (TConstr id types) =
  do dTypes <- mapM (dConstrType cons vars) types
     dataK <- find tName cons
     let tyvars  = map (\(i:>:ks) -> Tyvar i ks) vars
         dTypes' = map removeKVarsFromType dTypes
         ctrType = makeConstrType (Tycon tName (removeVars dataK),tyvars) dTypes'
         dCon    = DC.MkDataCon { DC.dName = id, DC.tName = tName, DC.fields = dTypes', DC.conType = TScheme [] ctrType}
     return ((A.:>:) id (quantify tyvars ctrType), dCon)

removeKVarsFromType :: Type -> Type
removeKVarsFromType (TAp t1 t2) = TAp (removeKVarsFromType t1) (removeKVarsFromType t2)
removeKVarsFromType (TVar (Tyvar i k)) = TVar (Tyvar i (removeVars k))
removeKVarsFromType (TCon (Tycon i k)) = TCon (Tycon i (removeVars k))
removeKVarsFromType x = x


makeConstrType :: (Tycon,[Tyvar]) -> [Type] -> Type
makeConstrType (tcs,ass) ts = foldr fn ret ts
  where ret  = foldl (\a acc -> TAp a acc) (TCon $ tcs) ass'
        ass' = map (TVar) ass

dConstrType :: Monad m => [KAssumption] -> [KAssumption] -> AType -> m Type
dConstrType _    vars (TTyVar v) =
  do k <- find v vars
     return $ TVar (Tyvar v k)
dConstrType cons _    (TGTyCon c) =
  do k <- find c cons
     return $ TCon (Tycon c k)

dConstrType cons vars (TATypeAp t1 t2) =
  do t1' <- dConstrType cons vars t1
     t2' <- dConstrType cons vars t2
     return $ TAp t1' t2'

dConstrType cons vars (TATypeArrow t1 t2) =
  do t1' <- dConstrType cons vars t1
     t2' <- dConstrType cons vars t2
     return $ (t1' `fn` t2')


makeKind :: SimpleType -> KAssumption
makeKind (TSimpleType id vars) = id :>: kvars
  where kvars = foldr (\x k -> Kfun (KVar $ Kvar x) k) Star vars


class FreeTypeNames a where
  ftn :: a -> [GTCon]

instance FreeTypeNames a => FreeTypeNames [a] where
  ftn = concatMap ftn

instance FreeTypeNames AType where
  ftn (TGTyCon gc)        = [gc]
  ftn (TATypeAp t1 t2)    = ftn t1 ++ ftn t2
  ftn (TATypeArrow t1 t2) = ftn t1 ++ ftn t2
  ftn _                   = []

instance FreeTypeNames Constr where
  ftn (TConstr _ ats) = ftn ats

instance FreeTypeNames TDataDecl where
  ftn (TDatadecl (TSimpleType tc _) ctrs) = ftn ctrs \\ [tc]
