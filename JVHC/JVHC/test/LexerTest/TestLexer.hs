module LexerTest.TestLexer(tests)
  where

import qualified LexerTest.TestCommentLexer as CL
import qualified LexerTest.TestConidAndVaridLexer as CVL
import qualified LexerTest.TestLiteralLexer as LL

import Test.HUnit

tests = test [CL.tests, CVL.tests, LL.tests]
