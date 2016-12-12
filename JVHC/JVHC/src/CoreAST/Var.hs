module CoreAST.Var where

import CoreAST.Types
import CoreAST.TScheme

data Var = MkVar  { varName :: String,  varType :: TScheme }
         | MkTVar { tvarName :: Type }
  deriving (Show,Eq)
