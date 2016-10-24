module TestDeclParser(tests)
  where

import Lexer
import Parser (decl)
import AST
import Test.HUnit

tests = test [test1,test2]

test1 = "tdecl f x = 2" ~: decl [Varid "f",Varid "x",ReservedOP Equal,Literal (TInteger 2)] ~=? TFunDecl (TVarPat (TVarID "f") ["x"]) (TELiteral (TInteger 2))

test2 = "tdecl f x y = E" ~: decl [Varid "f",Varid "x",Varid "y",ReservedOP Equal,Conid "E"] ~=? TFunDecl (TVarPat (TVarID "f") ["x","y"]) (TEConstr "E")

