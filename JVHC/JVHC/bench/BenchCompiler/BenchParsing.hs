module BenchCompiler.BenchParsing where


import Criterion.Main

import SampleProg.Programs

import Pipeline.Compiler

parseBench :: [String] -> Benchmark
parseBench s = bench "parse fib" $ nf (show . lexAndparse)
                         (functionsToProg s)

parseBenchFib :: Benchmark
parseBenchFib = parseBench [fib,"main = putInt (fib 30)"]

parseBenchEvenOdd :: Benchmark
parseBenchEvenOdd = parseBench [evenOdd,"main = putInt (odd 31)"]

benchmark :: Benchmark
benchmark = bgroup "Parsing"  [ parseBenchFib, parseBenchEvenOdd ]

main = defaultMain [benchmark]
