module CoreAST.TScheme where

import CoreAST.Types

data TScheme = TScheme [Type] Type
  deriving (Show,Eq)
