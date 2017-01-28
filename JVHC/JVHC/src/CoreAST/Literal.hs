{-# LANGUAGE DeriveGeneric #-}

module CoreAST.Literal where

import GHC.Generics
import Control.DeepSeq

data Literal = LitInt     Integer
             | LitChar    Char
             | LitString  String
  deriving (Show,Eq,Generic)

instance NFData Literal
