{-# LANGUAGE OverloadedStrings #-}

module CodeGen.CGFileWriter where

import Data.Bifunctor (bimap)
import Codec.JVM (classFileBS,ClassFile)

import Data.Text as T
import Data.ByteString as BS

import Data.Monoid

writeFiles ::  Text                 ->  -- Path
               [(Text,ClassFile)]   ->  -- Files
               IO ()

writeFiles path files =
  mapM_ (\(p,c) -> BS.writeFile (T.unpack (path `mappend` p `mappend` ".class")) (classFileBS c)) files
