{-# LANGUAGE TemplateHaskell #-}

module MemoryUsage.MemUsageTypes where

import Control.Lens


data Times = Times { user :: Double, sys :: Double, real :: Double }
  deriving (Show,Eq)

data GCRun = GCRun { before :: Integer, after :: Integer, total :: Integer }
  deriving (Show,Eq)

data GCTimed = GCTimed { gcTime :: Double
                       , gcDetails :: GCOutput }
  deriving (Show,Eq)

data GCHeap = GCHeap { youngUsed      :: Integer
                     , youngTotal     :: Integer
                     , parOldTotal    :: Integer
                     , parOldUsed     :: Integer
                     , metaspaceTotal :: Integer
                     , metaspaceUsed  :: Integer }
  deriving (Show,Eq)

data GCOutput = GCAlloc { youngGen :: GCRun
                        , otherGen :: GCRun
                        , allTimes :: Times
                        , realTime :: Double }
              | GCFull { youngGen  :: GCRun
                       , parOldGen :: GCRun
                       , otherGen  :: GCRun
                       , metaSpace :: GCRun
                       , allTimes  :: Times
                       , realTime  :: Double }
  deriving (Show,Eq)

makeLenses ''Times
makeLenses ''GCHeap
makeLenses ''GCRun
makeLenses ''GCTimed
makeLenses ''GCOutput


type GCParserOut = ([GCTimed],GCHeap)
