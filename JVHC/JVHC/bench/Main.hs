module Main where

import TimeProgram.RunProgram
import TimeProgram.RunSumN
import TimeProgram.RunFibo
import TimeProgram.RunInlineBench
import TimeProgram.RunBadInlineBench

import TimeTI.TimeTI

import qualified BenchCompiler.BenchTI as BTI

import qualified BenchCompiler.BenchCompiler as BC

import CountConstructs.OutputParser
import CountConstructs.CountProg

import CodeSize.CountCodeSize

import MemoryUsage.MemUsageParser
import MemoryUsage.MemUsageTypes
import MemoryUsage.MemUsageBench

import qualified TimeGHCProgram.RunFibo as GF
import qualified TimeGHCProgram.RunTestN as TN

main = GF.main
