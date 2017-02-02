module MemoryUsage.MemUsageTypes where


data Times = Times { user :: Double, sys :: Double, real :: Double }
  deriving (Show,Eq)

data GCRun = GCRun { before :: Integer, after :: Integer, total :: Integer }
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
                       , realTime  :: Double}
  deriving (Show,Eq)
