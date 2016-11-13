module CoreAST.Var where

import CoreAST.Types

data Var = MkVar  { varName :: String,  varType :: Type }
         | MkTVar { tvarName :: Type }
  deriving Show
