module TestParser(tests)
  where

import qualified TestDeclParser as TDP
import qualified TestExpParser as TEP
import qualified TestFullParser as TFP
import qualified TestGendecl as TG
import qualified TestTopDeclParser as TTDP

import Test.HUnit

tests = test [TDP.tests, TEP.tests,TFP.tests,TG.tests,TTDP.tests]
