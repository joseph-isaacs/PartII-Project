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

import BenchCompiler.BenchProgams

mkCEDTC programSource = compilerPreCodeGen noOpt programSource

codeGen' e = map (\(_,c) -> classFileBS c) (codeGen False e)

benCGen (n,p) = bench ("CodeGen: " ++ show n) $ nf codeGen' e
  where !e = mkCEDTC p

benchmark = bgroup "CodeGen" (map benCGen allProgs)
