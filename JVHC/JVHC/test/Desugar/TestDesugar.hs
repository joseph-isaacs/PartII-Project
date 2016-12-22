module Desugar.TestDesugar where

import Desugar.TestDType as DT
import Desugar.TestFreeVariables as TFV

import Test.HUnit

tests = test [DT.tests, TFV.tests]
