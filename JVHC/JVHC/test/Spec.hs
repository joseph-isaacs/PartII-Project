module Spec (main)
  where

import qualified TestParser as TP
import Test.HUnit

main :: IO ()
main = do
  putStrLn "Running Parsing Tests"
  runTestTT tests
  return ()


tests = test [TP.tests]
