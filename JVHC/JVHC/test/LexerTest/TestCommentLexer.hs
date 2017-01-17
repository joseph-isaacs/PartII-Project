module LexerTest.TestCommentLexer(tests)
  where

import Parsing.Lexer
import Parsing.ParsingAST
import CoreAST.Literal
import Test.HUnit

test1 = "One Line Comment" ~: alexScanTokens "--12gs--\n"  ~=? []
test2 = "not a comment" ~: alexScanTokens "-->12\n"      ~=? [Varsym "-->", Literal (LitInt 12)]
test3 = "multiline comment"       ~: alexScanTokens "--helo\n12\n--\n"       ~=? [Literal (LitInt 12)]

tests = test [test1, test2, test3]


