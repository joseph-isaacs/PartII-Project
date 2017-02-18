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

import BenchCompiler.BenchProgams


mkCoreExpr = (\((c,_),_,_) -> c) . fromJust . compilerSo

benchInline (n,p) = bench ("Inlining: " ++ n) $ nf (optimize normalOpt) e
  where !e = mkCoreExpr p


benchmark = bgroup "Inlining" (map benchInline allProgs)
