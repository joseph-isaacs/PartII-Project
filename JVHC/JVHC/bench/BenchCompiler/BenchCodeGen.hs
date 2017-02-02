{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module BenchCompiler.BenchCodeGen where

import Criterion.Main

import GHC.Generics
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

import Codec.JVM.Class

import Pipeline.Compiler


mkCEDTC programSource = compilerPreCodeGen noOpt programSource

codeGen' e = map (\(_,c) -> classFileBS c) (codeGen False e)

codeGenBenchFib = bench "CodeGen fib" $ nf codeGen' e
  where (!e) = mkCEDTC (functionsToProg [fib,"main = putInt (fib 30)"])

codeGenBenchEvenOdd = bench "CodeGen EvenOdd" $ nf codeGen' e
  where (!e) = mkCEDTC (functionsToProg [evenOdd,pair2,pair3,pair4,pair5,fib,"main = putInt (odd 31)"])


benchmark = bgroup "CodeGen" [codeGenBenchFib, codeGenBenchEvenOdd]


main = defaultMain [benchmark]
