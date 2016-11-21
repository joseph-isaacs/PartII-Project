module CoreAST.DataCon where

import CoreAST.Types
import CoreAST.Kind


data TyCon = MkTyCon { mtyCon :: Tycon }
 deriving Show

data DataCon = MkDataCon { dName :: String, conType :: Type }
  deriving Show
