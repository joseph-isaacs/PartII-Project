{-# LANGUAGE BangPatterns #-}

module BenchCompiler.BenchInlining where


import Criterion.Main

import Control.DeepSeq

import Data.Maybe

import SampleProg.Programs

import CoreAST.CoreExpr
import CoreAST.Types

import Desugar.DTopDecls(splitDataType)
import Desugar.DExpr(Program)

import Infer.TIProgram
import Infer.Id
import Infer.BuildInFunctionTypes
import Infer.Assumption

import Pipeline.Compiler

mkCoreExpr = (\((c,_),_,_) -> c) . fromJust . compilerSo

inlineBenchFib = bench "Inlining fib" $ nf (optimize normalOpt) e
  where !e = mkCoreExpr (functionsToProg [fib,"main = putInt (fib 30)"])

inlineBenchEvenOdd = bench "Inlining evenOdd" $ nf (optimize normalOpt) e
  where !e = mkCoreExpr (functionsToProg [evenOdd,"main = putInt (odd 31)"])


benchmark = bgroup "Inlining" [inlineBenchFib, inlineBenchEvenOdd]

main = defaultMain [benchmark]
