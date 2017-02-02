module MemoryUsage.MemUsageBench where

import SampleProg.InlineProg
import SampleProg.ProgMaker

import Pipeline.Compiler


import TimeProgram.RunProgram

import MemoryUsage.MemUsageParser
import MemoryUsage.MemUsageTypes


runBenchmark = varyProg [300000]

varyProg :: [Int] -> IO [(String,[[GCOutput]])]
varyProg enum =
  do opt  <- mapM (runProg normalOpt) enum
     nOpt <- mapM (runProg noOpt)     enum
     return $ opt ++ nOpt

runProg :: OptimizeParams -> Int -> IO (String,[[GCOutput]])
runProg op n =
  do let src = buildProg n
     mean <- runN 1 src False op "-Xss400m -XX:+PrintGC -XX:+PrintGCDetails" parseLines
     return ((if inlineTimes op == 1 then "OP" else "") ++ show n, mean)

buildProg :: Int -> String
buildProg times = functionsToProg [mkPutIntMainMethod times "testN", testProg ]
