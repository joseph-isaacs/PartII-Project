module TestLiteralLexer(tests)
  where

import Lexer
import AST
import Test.HUnit

testString = "string hello" ~: alexScanTokens "\"hello\"" ~=? [Literal (TString "hello")]
testInt    = "integer 0153" ~: alexScanTokens "0153"      ~=? [Literal (TInteger 153)]
testChar   = "char p"       ~: alexScanTokens "'p'"       ~=? [Literal (TChar    'p')]
testSIC    = "lex char, integer, string"
                            ~: alexScanTokens "\"hello\"123'a'" ~=?
                               [Literal (TString "hello"), Literal (TInteger 123),
                                Literal (TChar   'a')]

tests = test [testString, testInt, testChar, testSIC]


