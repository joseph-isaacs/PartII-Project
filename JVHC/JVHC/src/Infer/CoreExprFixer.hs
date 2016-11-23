{-# LANGUAGE FlexibleInstances #-}

module Infer.CoreExprFixer where

import CoreAST.CoreExpr

import CoreAST.Var

import CoreAST.Types
import CoreAST.Kind

import Infer.CoreExprSubst
import Infer.Subst

class SystemF a where
  -- [Tycon] are the already bound variables.
  addLambda :: [Tyvar] -> a -> a
  addTvApp  :: a -> a


instance SystemF Var where
  addLambda bind v = undefined
  addTvApp v = undefined

instance SystemF (Bind Var) where
  addLambda bind (NonRec a e) = (NonRec a (addLambda bind e))
  addLambda bind (Rec bs)     = Rec $ map (\(v,e) -> (v, addLambda bind e)) bs
  addTvApp  r = undefined

instance SystemF [Alt Var] where
  addLambda bind = map (\(ac,bs,e) -> (ac,bs, addLambda bind e))
  addTvApp  b = undefined

instance SystemF CoreExpr where
  addLambda bound (App e e') = App (addLambda bound e) (addLambda bound e')
  addLambda bound (Lam v e) = foldr driver lam (filter (`notElem` bound) tvars)
    where driver tvar acc = Lam (MkTVar { tvarName = tvar }) acc
          tvars = (tv v)
          lam  = Lam v (addLambda tvars e)

  addLambda bound (Let b e) = (Let b' e')
    where b' = addLambda bound b
          e' = addLambda bound e

  addLambda bound (Case e t al) = (Case e' t al')
    where e'  = addLambda bound e
          al' = addLambda bound al

  addLambda _ e = e

  addTvApp e = e






--addTypeApplications :: CoreExprDefs -> CoreExpr
--addTypeApplications defs (App e1 e2) =
--  case e1 of
--    Var id -> lookup id defs
--    Lam x   -> undefined

--getFirstFreeType :: CoreExprDefs -> CoreExpr -> Maybe Type
--getFirstFreeType el (Var id) = map (getFirstFreeType el) (lookup id el)
--getFirstFreeType el s = Nothing
