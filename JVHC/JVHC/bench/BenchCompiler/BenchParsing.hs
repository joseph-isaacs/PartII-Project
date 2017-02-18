module BenchCompiler.BenchParsing where


import Criterion.Main

import SampleProg.Programs

import Pipeline.Compiler

import BenchCompiler.BenchProgams

parseBench :: (String, String) -> Benchmark
parseBench (n,s) = bench ("Parse: " ++ n) $ nf (show . lexAndparse) s

benchmark :: Benchmark
benchmark = bgroup "Parsing"  (map parseBench allProgs)
