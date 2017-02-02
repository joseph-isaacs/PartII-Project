module BenchCompiler.BenchCompiler where

import Criterion.Main

import qualified BenchCompiler.BenchParsing  as BP
import qualified BenchCompiler.BenchDesugar  as BD
import qualified BenchCompiler.BenchTI       as BTI
import qualified BenchCompiler.BenchSystemF  as BSF
import qualified BenchCompiler.BenchInlining as BI
import qualified BenchCompiler.BenchCodeGen  as BCG

benchmark = bgroup "Compiler"
                  [ BP.benchmark
                  , BD.benchmark
                  , BTI.benchmark
                  , BSF.benchmark
                  , BI.benchmark
                  , BCG.benchmark
                  ]

main = defaultMain [benchmark]
