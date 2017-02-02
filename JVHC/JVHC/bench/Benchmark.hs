module Benchmark where

import TimeProgram.RunProgram
import TimeProgram.RunSumN
import TimeProgram.RunFibo
import TimeProgram.RunInlineBench

import TimeTI.TimeTI

import BenchCompiler.BenchCompiler

import CountConstructs.OutputParser
import CountConstructs.CountProg

import MemoryUsage.MemUsageParser
import MemoryUsage.MemUsageTypes
import MemoryUsage.MemUsageBench

main = putStr "hello"
