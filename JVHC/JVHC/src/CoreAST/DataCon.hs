module CoreAST.DataCon where

import CoreAST.Types


data TyCon = MkTyCon { mtyCon :: Tycon }
 deriving (Show,Eq)

data DataCon = MkDataCon { dName :: String, conType :: Type }
  deriving (Show,Eq)
