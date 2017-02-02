module CountConstructs.CountProg where

import SampleProg.Programs


import Pipeline.Compiler

import TimeProgram.RunProgram


import CountConstructs.OutputParser

countConstructionFromProg  :: String -> IO [ConstructCount]
countConstructionFromProg source = runN 1 source True noOpt "" (countConstructs . lines)

countCFib :: IO [ConstructCount]
countCFib =
  do let source = functionsToProg [fib,"main = putInt (fib 3)"]
     countConstructionFromProg source
