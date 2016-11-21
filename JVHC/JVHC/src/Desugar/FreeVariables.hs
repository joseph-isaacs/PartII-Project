{-# LANGUAGE FlexibleInstances #-}

module Desugar.FreeVariables where

import CoreAST.Types

import Desugar.DExpr

import Infer.TIMain
import Infer.TIPat
import Infer.Id

class FreeVariables a where
  fv :: a -> [String]
  binder :: a -> [String]


instance FreeVariables Impl where
  fv (id,alt) = removeAll [id] (fv alt)
  binder (id,alt) = [id]

instance FreeVariables Expl where
  fv (id,_,alt) = removeAll [id] (fv alt)
  binder (id,_,_) = [id]

instance FreeVariables Expr where
  fv (Var id)   = [id]
  fv (Ap e e')  = fv e ++ fv e'
  fv (Lam id e) = filter (/=id) (fv e)
  fv (Let bg e) = removeAll (binder bg) (fv e)
  fv (Case e pe) = fv e ++ concatMap (\(p,e) -> removeAll (fv p) (fv e)) pe
  fv e          = []

  binder b      = []

removeAll :: Eq a => [a] -> [a] -> [a]
removeAll toRem = filter (\x -> not (elem x toRem))

instance FreeVariables Pat where
  fv p = []

  binder b = []

instance FreeVariables BindGroup where
  fv (expl,impl) = concatMap fv expl ++ concatMap (concatMap fv) impl


  binder (expl,impl) = concatMap binder expl ++ concatMap (concatMap binder) impl

--instance (FreeVariables a, FreeVariables b) => FreeVariables (a,b) where
--  fv (a,b) = fv a ++ fv b

--instance FreeVariables a => FreeVariables [a] where
--  fv = concatMap fv
