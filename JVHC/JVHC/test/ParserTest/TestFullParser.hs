module TestFullParser(tests)
  where

import Parsing.Lexer
import Parsing.Parser (jvhcParse)
import Parsing.ParsingAST
import Test.HUnit

tests = test [test1]

test1 = "tdecl {data U = U; f x = 2}" ~: jvhcParse [Special LCurly,ReservedID Data,Conid "U",ReservedOP Equal,Conid "U",Special SemiColon,Varid "f",Varid "x",ReservedOP Equal,Literal (LitInt 2),Special RCurly]
                                      ~=? TTopDecls [TData (TSimpleType "U" []) [TConstr "U" []],TDecl (TFunDecl (TVarPat (TVarID "f") ["x"]) (TELiteral (LitInt 2)))]

