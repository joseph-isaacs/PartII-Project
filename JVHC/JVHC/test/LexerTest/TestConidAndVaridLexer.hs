module LexerTest.TestConidAndVaridLexer(tests)
  where

import Parsing.Lexer
import Parsing.ParsingAST
import Test.HUnit

test1 = "Conid" ~: alexScanTokens "Hell1o"  ~=? [Conid "Hell1o"]
test2 = "Varid" ~: alexScanTokens "hel1r3"      ~=? [Varid "hel1r3"]
test3 = "ReservedID"       ~: alexScanTokens "case Hello"       ~=? [ReservedID Case, Conid "Hello"]

tests = test [test1, test2, test3]


