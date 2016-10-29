module TestGendecl(tests)
  where

import Lexer
import Parser
import ParsingAST
import Test.HUnit

tests = test [test1, test2, test3]

test1 = "Simple type" ~: genDecl [Varid "x",ReservedOP DColon,Varid "t"]  ~=? (["x"],TATypeNested [TTyVar "t"])
test2 = "Multiple variables" ~: genDecl [Varid "x",Special Comma, Varid "y", ReservedOP DColon,Varid "t"]  ~=? (["x","y"],TATypeNested [TTyVar "t"])
test3 = "Complex type" ~: genDecl [Varid "x",Special Comma,Varid "y",ReservedOP DColon,Special LParen,Conid "IO",Varid "a",Special RParen,Varid "t"]
                                                                                                 ~=? (["x","y"],TATypeNested [TATypeNested [TGTyCon "IO",TTyVar "a"],TTyVar "t"])

