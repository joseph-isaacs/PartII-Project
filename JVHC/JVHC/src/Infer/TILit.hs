module Infer.TILit where

import Infer.TIM
import CoreAST.Types
import CoreAST.Kind
import Parsing.ParsingAST



tiLit  :: Literal -> TI Type
tiLit (LitChar _) = return tChar
tiLit (LitInt  _) = return tInt
