{-# LANGUAGE DeriveGeneric #-}

module CoreAST.DataCon where

import Control.DeepSeq
import GHC.Generics

import CoreAST.Types

import CoreAST.TScheme

data TyCon = MkTyCon { mtyCon :: Tycon, constrs :: [DataCon] }
 deriving (Show,Eq,Generic)

data DataCon = MkDataCon { dName :: String, tName :: String, fields :: [Type], conType :: TScheme }
  deriving (Show,Eq,Generic)

instance NFData DataCon
instance NFData TyCon
