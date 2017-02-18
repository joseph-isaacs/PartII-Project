module MemoryUsage.GCCount where

import MemoryUsage.MemUsageTypes


totalGCTime :: GCParserOut -> Double
totalGCTime = sum . (map (realTime . gcDetails)) . fst
