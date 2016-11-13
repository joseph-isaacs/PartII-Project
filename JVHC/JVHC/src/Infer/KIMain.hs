module Infer.KIMain where

import Infer.Id
import Infer.KIMonad
import Infer.KSubst
import Infer.KAssumption

import CoreAST.Kind
import CoreAST.Types
import CoreAST.DataType

import Parsing.ParsingAST


import Control.Arrow

data DataDecl = DD SimpleType [Constr]
  deriving (Eq,Show)

data TypeKinds = TK { typeName :: String, tKind :: Kind,  varKinds :: [KAssumption], dd :: DataDecl }
  deriving Show

kiDataDecls :: KInfer [DataDecl] [TypeKinds]
kiDataDecls as dd =
  do mapM (kiDataDecl as) dd
     s <- getKSubst
     let len = length s
         s'  = applyN apStOverSt len s
     return $ map (findTypeKind s') dd

findTypeKind :: KSubst -> DataDecl -> TypeKinds
findTypeKind ks d@(DD s@(TSimpleType name _)  _) = TK { typeName = name, tKind = k, varKinds = varK , dd = d}
  where
    varK = findKinds ks s
    varK' = map (\ (_:>:k) -> k) varK
    k = foldr (\var acc -> Kfun var acc) Star varK'


findKinds :: KSubst -> SimpleType -> [KAssumption]
findKinds ks (TSimpleType _ vars) = map (findKind ks) vars

findKind :: KSubst -> TVar -> KAssumption
findKind ks v = v :>: removeVars (applyK ks ( maybe Star id (lookup var ks)))
  where var = Kvar v


kiDataDecl :: KInfer DataDecl ()
kiDataDecl as (DD _ constr) =
  do mapM (kiConstr as) constr
     return ()

apStOverSt :: KSubst -> KSubst -> KSubst
apStOverSt s = map (id *** (applyK s))

applyN :: (a -> a -> a) -> Int -> a -> a
applyN f n s | n > 0 = applyN f (n-1) (f s s)
           | otherwise = s

decls = ["A" :>: Kfun (KVar $ Kvar "a") Star, "B" :>: Kfun (KVar $ Kvar "b") Star, "X" :>: Kfun (KVar $ Kvar "c") (Kfun (KVar $ Kvar "d") Star)]
dds = [DD (TSimpleType "A" ["b"])  [TConstr "X" [TATypeAp  (TGTyCon "B") (TTyVar "a")]], DD (TSimpleType "B" ["b"]) [TConstr "Y" [TATypeAp (TGTyCon "A") (TTyVar "b")]], DD (TSimpleType "X" ["c"]) [TConstr "V" [TATypeAp (TTyVar "c") (TTyVar "d")]]]

kiConstr :: KInfer Constr ()
kiConstr as (TConstr id types) =
  do kinds <- mapM (kiType as) types
     mapM (unify Star) kinds
     return ()


kiType :: KInfer AType Kind
kiType as (TTyVar v) = return (KVar $ Kvar v)
kiType as (TGTyCon i) = find i as
kiType as (TATypeAp k1 k2) = do
  k1' <- kiType as k1
  k2' <- kiType as k2
  v <- newKVar
  unify k1' (Kfun k2' v)
  return v

