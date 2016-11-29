module Infer.TyAppFixer where

import CoreAST.Types
import CoreAST.Kind
import CoreAST.Var
import CoreAST.CoreExpr

import Infer.Subst
import Infer.Subst
import Infer.TIM
import Infer.Id

type TyApp e r = [(Id,Var)] -> e -> TI r

