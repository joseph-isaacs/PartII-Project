module Infer.CoreExprFixer where

import CoreAST.CoreExpr

import CoreAST.Var

import CoreAST.Types
import CoreAST.Kind

import Infer.CoreExprSubst
import Infer.Subst



addBigLambda :: CoreExpr -> CoreExpr
addBigLambda lam@(Lam v e) = foldr driver lam (tv v)
  where driver tvar acc = Lam (MkTVar { tvarName = tvar }) acc

addBigLambda e         = e



--addTypeApplications :: CoreExprDefs -> CoreExpr
--addTypeApplications defs (App e1 e2) =
--  case e1 of
--    Var id -> lookup id defs
--    Lam x   -> undefined

--getFirstFreeType :: CoreExprDefs -> CoreExpr -> Maybe Type
--getFirstFreeType el (Var id) = map (getFirstFreeType el) (lookup id el)
--getFirstFreeType el s = Nothing
