module TimeProgram.RunSumN where

import TimeProgram.RunProgram

import Pipeline.Compiler

runBenchmark = varySum [10,100,1000,10000,100000]

varySum :: [Int] -> IO [(String,[Int])]
varySum enum =
  do opt  <- mapM (runSum normalOpt) enum
     nOpt <- mapM (runSum noOpt)     enum
     return $ opt ++ nOpt

runSum :: OptimizeParams -> Int -> IO (String,[Int])
runSum op n =
  do src <- buildSumN n
     mean <- runN  50 src False op "-Xss400m" (\x -> read (last $ lines x) :: Int)
     return ((if inlineTimes op == 1 then "OP" else "") ++ show  n,mean)

buildSumN :: Int -> IO String
buildSumN times =
  do file <- readFile "test/TestProg/sumN.hs"
     let prog = file ++ "main = putInt (sumN " ++ show times ++ ") }"
     return prog
