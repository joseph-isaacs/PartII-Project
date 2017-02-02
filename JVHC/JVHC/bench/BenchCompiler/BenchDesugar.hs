{-# LANGUAGE BangPatterns #-}

module BenchCompiler.BenchDesugar where


import Criterion.Main

import Data.Maybe(fromJust)

import SampleProg.Programs

import Pipeline.Compiler

desugar' b = show $ fromJust $ desugar b

desugarBenchFib :: Benchmark
desugarBenchFib = bench "desugar fib" $ nf desugar' body
  where (!body) = (lexAndparse $ functionsToProg [fib,"main = putInt (fib 30)"])

desugarBenchEvenOdd :: Benchmark
desugarBenchEvenOdd = bench "desugar even odd" $ nf desugar' body
  where (!body) = (lexAndparse $ functionsToProg [evenOdd,"main = putInt (odd 31)"])

benchmark :: Benchmark
benchmark = bgroup "Desugar"  [ desugarBenchFib, desugarBenchEvenOdd ]

main = defaultMain [benchmark]
