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

mkAssAndDt :: Monad m => String -> m ([Assumption],Program)
mkAssAndDt programSource =
  do (bg,des)  <- (desugar . lexAndparse) programSource
     let sdt = splitDataType des
         ass = snd sdt ++ buildInAssumptions
     return (ass,[bg])

tiProgram' :: [Assumption] -> Program -> CoreExprDefs
tiProgram' a p = (\((x,_),_) -> x) $ tiProgram a p


tiBenchFib = bench "TI fib" $ nf (tiProgram fibAss) fibDT
  where (!fibAss,!fibDT) = fromJust $ mkAssAndDt (functionsToProg [fib,"main = putInt (fib 30)"])


tiBenchEO = bench "TI Even Odd" $ whnf (tiProgram assump) prog
  where (!assump,!prog) = fromJust $ mkAssAndDt (functionsToProg [evenOdd,"main = putInt (odd 31)"])

tiBenchPair = bgroup "TI Pairs"
  [ bench "Pair 2" (nf (tiProgram' assP2) progP2)
  , bench "Pair 3" (nf (tiProgram' assP3) progP3)
  , bench "Pair 4" (nf (tiProgram' assP4) progP4)
  , bench "Pair 5" (nf (tiProgram' assP5) progP5)
  ]
  where (!assP2,!progP2) = fromJust $ mkAssAndDt (functionsToProg [pair2])
        (!assP3,!progP3) = fromJust $ mkAssAndDt (functionsToProg [pair3])
        (!assP4,!progP4) = fromJust $ mkAssAndDt (functionsToProg [pair4])
        (!assP5,!progP5) = fromJust $ mkAssAndDt (functionsToProg [pair5])

benchmark = bgroup "TI" [tiBenchFib, tiBenchEO, tiBenchPair]
