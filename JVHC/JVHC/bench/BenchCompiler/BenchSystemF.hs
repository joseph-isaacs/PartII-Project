{-# LANGUAGE BangPatterns #-}

module BenchCompiler.BenchSystemF where


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

mkAssAndDt programSource =
  do ((ced,as), idToCT)  <- (typeInference . desugar . lexAndparse) programSource
     let tlv = getTopLevelVars ced
     return (idToCT, tlv, ced)

sysFBenchFib = bench "SystemF fib" $ nf (map (toSystemFExpr m tlv )) expr
  where (!m,!tlv,!expr) = fromJust $ mkAssAndDt (functionsToProg [fib,"main = putInt (fib 30)"])

sysFBenchEvenOdd = bench "SystemF evenOdd" $ nf (map (toSystemFExpr m tlv )) expr
  where (!m,!tlv,!expr) = fromJust $ mkAssAndDt (functionsToProg [evenOdd,"main = putInt (odd 31)"])


benchmark = bgroup "System F" [sysFBenchFib, sysFBenchEvenOdd]
