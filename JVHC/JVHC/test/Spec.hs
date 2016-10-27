module Spec (main)
  where

import qualified TestParser as TP
import qualified TestLexer   as TL
import Test.HUnit

main :: IO ()
main = do
  putStrLn "Running Parsing Tests"
  runTestTT tests
  return ()


tests = test [TL.tests, TP.tests]
