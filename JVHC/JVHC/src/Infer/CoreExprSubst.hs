module Infer.CoreExprSubst where

import CoreAST.Types
import CoreAST.CoreExpr
import CoreAST.Var
import CoreAST.TScheme
import CoreAST.DataCon
import CoreAST.Literal

import Infer.Subst

instance Types b => Types (ExprDef b) where
  apply s (ExprDef b e) = ExprDef (apply s b) (apply s e)
  tv s = []

instance Types TScheme where
  apply s (TScheme b t) = TScheme b (apply s t)
  tv (TScheme b t) = tv t

instance Types Var where
  apply s (MkVar { varName = n, varType = t }) =
                            MkVar { varName = n, varType = apply s t }
  apply s (MkTVar { tvarName = t }) = MkTVar { tvarName = apply s t }

  tv (MkVar {varType = t})   = tv t
  tv (MkTVar {tvarName = t}) = tv t

instance Types b => Types (Expr b) where
  apply s (App e1 e2)     = App (apply s e1) (apply s e2)
  apply s (Lam b   e)     = Lam (apply s b) (apply s e)
  apply s (Let bind e)    = Let (apply s bind) (apply s e)
  apply s (Case e t alts) = Case (apply s e) (apply s t) (apply s alts)
  apply s (Type t)        = Type (apply s t)
  apply s e               = e

  tv s           = []

instance Types (AltCon) where
  apply s (DataAlt dc) = DataAlt $ apply s dc
  apply s ac           = ac

  tv s                 = []

instance Types DataCon where
  apply s (MkDataCon { dName = n, conType = c}) =
    MkDataCon { dName = n, conType = apply s c}

  tv s = []


