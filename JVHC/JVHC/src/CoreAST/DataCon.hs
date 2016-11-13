module CoreAST.DataCon where

import CoreAST.Types

data TyCon = Unit
 deriving Show

data DataCon = MkDataCon { name :: String, conType :: Type, tyCon :: TyCon }
  deriving Show
