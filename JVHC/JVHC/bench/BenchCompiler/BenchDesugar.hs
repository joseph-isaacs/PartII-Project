{-# LANGUAGE BangPatterns #-}

module BenchCompiler.BenchDesugar where


import Criterion.Main

import Data.Maybe(fromJust)

import SampleProg.Programs

import Pipeline.Compiler

import BenchCompiler.BenchProgams

desugar' b = show $ fromJust $ desugar b

desugarB (n,p) = bench ("Desugar: " ++ show n) $ nf desugar' body
  where (!body) = lexAndparse p


benchmark :: Benchmark
benchmark = bgroup "Desugar"  (map desugarB allProgs)
