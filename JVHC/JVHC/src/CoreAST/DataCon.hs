module CoreAST.DataCon where

import CoreAST.Types

import CoreAST.TScheme

data TyCon = MkTyCon { mtyCon :: Tycon, constrs :: [DataCon] }
 deriving (Show,Eq)

data DataCon = MkDataCon { dName :: String, tName :: String, fields :: [Type], conType :: TScheme }
  deriving (Show,Eq)
