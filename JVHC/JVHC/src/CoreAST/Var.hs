module CoreAST.Var where

import CoreAST.Types

data TScheme = TScheme [Type] Type
  deriving (Show,Eq)

data Var = MkVar  { varName :: String,  varType :: TScheme }
         | MkTVar { tvarName :: Type }
  deriving (Show,Eq)
