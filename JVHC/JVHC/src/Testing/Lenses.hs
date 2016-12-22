{-# LANGUAGE TemplateHaskell #-}

module Testing.Lenses where

import Control.Lens

data Data = MkData { _name :: String
                 , _age  :: Int
                 }

makeLenses ''Data
