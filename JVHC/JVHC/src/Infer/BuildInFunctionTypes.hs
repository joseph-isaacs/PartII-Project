module Infer.BuildInFunctionTypes where

import CoreAST.BuildInFunctions


import Infer.Scheme
import Infer.Subst
import Infer.Assumption


buildInAssumptions :: [Assumption]
buildInAssumptions = map (\bi -> (fnName bi) :>: (toScheme $ fnType bi)) buildIn
  where toScheme t = quantify (tv t) t
