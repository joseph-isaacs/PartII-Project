module CoreAST.DataType where

import Infer.KAssumption as KA
import Infer.Assumption  as A

data DataType = DT { name         :: String
                      , paraMap      :: KA.KAssumption
                      , constructors :: A.Assumption }
  deriving Show


