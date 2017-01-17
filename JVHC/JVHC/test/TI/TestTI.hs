module TI.TestTI where

import TI.TIExpr as TIE
import TI.TIFixer as TIF

import Test.HUnit

tests = test [TIE.tests, TIF.tests]
