module TimeProgram.RunBadInlineBench where

import Control.Monad

import Printing.CSVPrint

import TimeProgram.RunProgram

import SampleProg.BadInlineProg
import SampleProg.ProgMaker

import Pipeline.Compiler

import TimeProgram.TimeRunningParser

runBenchmark = varyTestInline numb

saveBenchmark :: FilePath -> IO ()
saveBenchmark path =
  do out <- (liftM toCSV $ runBenchmark)
     putStr out
     writeFile path out


varyTestInline :: Fractional a =>  [Int] -> IO [(IsOp String,[a])]
varyTestInline enum =
  do opt  <- mapM (runBadTestInline normalOpt) enum
     -- nOpt <- mapM (runBadTestInline noOpt)     enum
     return $ opt -- ++ nOpt

runBadTestInline :: Fractional a => OptimizeParams -> Int -> IO (IsOp String,[a])
runBadTestInline op n =
  do let src = buildProg n
     putStrLn (show n)
     res <- liftM (map fromIntegral) $  runN 50 src False op "-Xss400m" parseTime
     return ((inlineTimes op == 1 , show  n),res)

buildProg :: Int -> String
buildProg times = functionsToProg [mkPutIntMainMethod times "badProg",badTestProg  ]

numb :: [Int]
numb = 50 : map ( (\x -> x -10000 ) . floor . (*) 100000 . log . log) [4,7..70]

