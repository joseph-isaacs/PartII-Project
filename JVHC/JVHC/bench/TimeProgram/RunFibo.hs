module TimeProgram.RunFibo where

import TimeProgram.RunProgram

import Pipeline.Compiler

runBenchmark = varyFibo [30]

varyFibo :: [Int] -> IO [(String,[Int])]
varyFibo enum =
  do opt  <- mapM (runFibo normalOpt) enum
     nOpt <- mapM (runFibo noOpt)     enum
     return $ opt ++ nOpt

runFibo :: OptimizeParams -> Int -> IO (String,[Int])
runFibo op n =
  do src <- buildFibo n
     mean <- runN 2 src False op "-Xss400m" (\x -> read (last (lines x)) :: Int)
     return ((if inlineTimes op == 1 then "OP" else "") ++ show  n,mean)

buildFibo :: Int -> IO String
buildFibo times =
  do file <- readFile "test/TestProg/fibo.bhs"
     let prog = file ++ "main = putInt (fib " ++ show times ++ ") }"
     return prog
