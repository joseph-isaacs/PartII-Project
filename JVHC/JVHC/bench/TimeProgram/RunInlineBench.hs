module TimeProgram.RunInlineBench where

import TimeProgram.RunProgram

import SampleProg.InlineProg
import SampleProg.ProgMaker

import Pipeline.Compiler

runBenchmark = varyTestInline [300000]

varyTestInline :: [Int] -> IO [(String,[Int])]
varyTestInline enum =
  do opt  <- mapM (runTestInline normalOpt) enum
     nOpt <- mapM (runTestInline noOpt)     enum
     return $ opt ++ nOpt

runTestInline :: OptimizeParams -> Int -> IO (String,[Int])
runTestInline op n =
  do let src = buildProg n
     mean <- runN 2 src False op "-Xss400m" (\x -> read (last (lines x)) :: Int)
     return ((if inlineTimes op == 1 then "OP" else "") ++ show  n,mean)

buildProg :: Int -> String
buildProg times = functionsToProg [mkPutIntMainMethod times "testN", testProg ]

