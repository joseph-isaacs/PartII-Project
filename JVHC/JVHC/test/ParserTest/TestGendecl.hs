module ParserTest.TestGendecl(tests)
  where

import Parsing.Lexer
import Parsing.Parser
import Parsing.ParsingAST
import Test.HUnit

tests = test [test1, test2, test3]

test1 = "Simple type" ~: genDecl [Varid "x",ReservedOP DColon,Varid "t"]  ~=? TGendecl ["x"] (TTyVar "t")
test2 = "Multiple variables" ~: genDecl [Varid "x",Special Comma, Varid "y", ReservedOP DColon,Varid "t"]  ~=? TGendecl ["x","y"] (TTyVar "t")
test3 = "Complex type" ~: genDecl [Varid "x",Special Comma,Varid "y",ReservedOP DColon,Special LParen,Conid "IO",Varid "a",Special RParen,Varid "t"]
                                                                                                 ~=? TGendecl ["x","y"] (TATypeAp (TATypeAp (TGTyCon "IO") (TTyVar "a")) (TTyVar "t"))

