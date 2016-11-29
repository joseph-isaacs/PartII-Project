module CoreAST.Var where

import CoreAST.Types

data Var = MkVar  { varName :: String,  varType :: Type }
         | MkTVar { tvarName :: Tyvar }
  deriving (Show,Eq)
