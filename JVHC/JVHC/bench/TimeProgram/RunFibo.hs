module TimeProgram.RunFibo where

import Control.Monad

import Pipeline.Compiler

import SampleProg.Programs
import SampleProg.ProgMaker

import TimeProgram.RunProgram
import TimeProgram.TimeRunningParser

import Printing.CSVPrint

input :: (Num a, Enum a) => [a]
input = [1..35]

saveBenchmark :: FilePath -> IO ()
saveBenchmark path =
  do out <- (liftM toCSV $ runBenchmark)
     putStr out
     writeFile path out


runBenchmark = varyFibo input

varyFibo :: (Show a, Num a) => [Int] -> IO [(IsOp String,[a])]
varyFibo enum =
  do p  <- mapM (runFibo noOpt) enum
     return p

runFibo :: (Show a, Num a) => OptimizeParams -> Int -> IO (IsOp String,[a])
runFibo op n =
  do let src = buildFibo n
     res <- runN 40 src False op "-Xss400m" parseTime
     putStrLn (show n ++ ": " ++ show res)
     return ((inlineTimes op == 1 , show  n),res)

buildFibo :: Int -> String
buildFibo times = functionsToProg [mkPutIntMainMethod times "fib", fib ]
