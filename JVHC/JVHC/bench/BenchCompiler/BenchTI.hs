{-# LANGUAGE BangPatterns #-}

module BenchCompiler.BenchTI where

import Criterion.Main

import Control.DeepSeq

import Data.Maybe

import CoreAST.CoreExpr

import SampleProg.Programs

import Desugar.DTopDecls(splitDataType)
import Desugar.DExpr(Program)

import Infer.TIProgram
import Infer.BuildInFunctionTypes
import Infer.Assumption
import Infer.Scheme

import Pipeline.Compiler

import BenchCompiler.BenchProgams

mkAssAndDt :: String ->  ([Assumption],Program)
mkAssAndDt programSource = fromJust $
  do (bg,des)  <- (desugar . lexAndparse) programSource
     let sdt = splitDataType des
         ass = snd sdt ++ buildInAssumptions
     return (ass,[bg])

benchTI (n,p) = bench ("TI: " ++ show n) $ nf (tiProgram ass) pr
  where (!ass,!pr) = mkAssAndDt p


tiBenchPair = bgroup "TI Pairs"
  [ bench "Pair 2" (nf (tiProgram assP2) progP2)
  , bench "Pair 3" (nf (tiProgram assP3) progP3)
  , bench "Pair 4" (nf (tiProgram assP4) progP4)
  , bench "Pair 5" (nf (tiProgram assP5) progP5)
  ]
  where (!assP2,!progP2) = mkAssAndDt (functionsToProg [pair2])
        (!assP3,!progP3) = mkAssAndDt (functionsToProg [pair3])
        (!assP4,!progP4) = mkAssAndDt (functionsToProg [pair4])
        (!assP5,!progP5) = mkAssAndDt (functionsToProg [pair5])

main = defaultMain [tiBenchPair]

benchmark = bgroup "TI"  (map benchTI allProgs)
