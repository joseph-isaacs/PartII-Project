module Infer.TILit where

import Infer.TIM
import CoreAST.Types
import CoreAST.Kind
import CoreAST.Literal


tiLit  :: Literal -> TI Type
tiLit (LitChar _) = return tChar
tiLit (LitInt  _) = return tInt
