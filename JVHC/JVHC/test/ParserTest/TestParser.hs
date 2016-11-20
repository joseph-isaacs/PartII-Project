module ParserTest.TestParser(tests)
  where

import qualified ParserTest.TestDeclParser as TDP
import qualified ParserTest.TestExpParser as TEP
import qualified ParserTest.TestFullParser as TFP
import qualified ParserTest.TestGendecl as TG
import qualified ParserTest.TestTopDeclParser as TTDP

import Test.HUnit

tests = test [TDP.tests, TEP.tests,TFP.tests,TG.tests,TTDP.tests]
