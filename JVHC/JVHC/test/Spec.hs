module Spec (main)
  where

import qualified ParserTest.TestParser as TP
import qualified LexerTest.TestLexer   as TL
import qualified Desugar.TestDesugar as TD
import qualified TI.TestTI as TI
import qualified OptTest.TestOpt as OT
import Test.HUnit

main :: IO ()
main = do
  putStrLn "Running Parsing Tests"
  runTestTT tests
  return ()


tests = test [TL.tests, TP.tests, TD.tests, TI.tests, OT.tests]
