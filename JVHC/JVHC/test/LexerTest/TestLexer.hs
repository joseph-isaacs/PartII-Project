module TestLexer(tests)
  where

import qualified TestCommentLexer as CL
import qualified TestConidAndVaridLexer as CVL
import qualified TestLiteralLexer as LL

import Test.HUnit

tests = test [CL.tests, CVL.tests, LL.tests]
