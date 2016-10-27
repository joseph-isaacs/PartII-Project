module TestCommentLexer(tests)
  where

import Lexer
import AST
import Test.HUnit

test1 = "One Line Comment" ~: alexScanTokens "--12gs--\n"  ~=? []
test2 = "not a comment" ~: alexScanTokens "-->12\n"      ~=? [Varsym "-->", Literal (TInteger 12)]
test3 = "multiline comment"       ~: alexScanTokens "--helo\n12\n--\n"       ~=? [Literal (TInteger 12)]

tests = test [test1, test2, test3]


