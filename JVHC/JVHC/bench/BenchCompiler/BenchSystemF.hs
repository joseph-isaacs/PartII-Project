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

import BenchCompiler.BenchProgams

mkAssAndDt programSource = fromJust $
  do ((ced,as), idToCT)  <- (typeInference . desugar . lexAndparse) programSource
     let tlv = getTopLevelVars ced
     return (idToCT, tlv, ced)



benchSysF (n,p) = bench ("SystemF: " ++ n) $ nf bSysFExprs exprs
  where (!m,!tlv,!exprs) = mkAssAndDt p
        bSysFExprs = map (toSystemFExpr m tlv)

benchmark = bgroup "System F" (map benchSysF allProgs)
