module LexerTest.TestLiteralLexer(tests)
  where

import Parsing.Lexer
import Parsing.ParsingAST
import CoreAST.Literal
import Test.HUnit

testString = "string hello" ~: alexScanTokens "\"hello\"" ~=? [Literal (LitString "hello")]
testInt    = "integer 0153" ~: alexScanTokens "0153"      ~=? [Literal (LitInt 153)]
testChar   = "char p"       ~: alexScanTokens "'p'"       ~=? [Literal (LitChar    'p')]
testSIC    = "lex char, integer, string"
                            ~: alexScanTokens "\"hello\"123'a'" ~=?
                               [Literal (LitString "hello"), Literal (LitInt 123),
                                Literal (LitChar   'a')]

tests = test [testString, testInt, testChar, testSIC]


