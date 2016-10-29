module TestExpParser(tests)
  where

import Lexer
import Parser (expParser)
import ParsingAST
import Test.HUnit

tests = test [test1, test2, test3, test4, test5]

test1 = "lambda \\x -> \\y -> x" ~: expParser [ReservedOP BSlash,Varid "x",ReservedOP RArrow,ReservedOP BSlash,Varid "y",ReservedOP RArrow,Varid "x"]  ~=? TELambda "x" (TELambda "y" (TEVar "x"))
test2 = "let {let x = 2} in y"   ~: expParser [ReservedID Let,Special LCurly,Varid "x",ReservedOP Equal,Literal (TInteger 2),Special RCurly,ReservedID In,Varid "y"]
                                                      ~=? TELet (TFunDecl (TVarPat (TVarID "x") []) (TELiteral (TInteger 2))) (TEVar "y")
test3 = "case x of {A -> b; B x -> c}" ~: expParser [ReservedID Case,Varid "x",ReservedID Of,Special LCurly,Conid "A",ReservedOP RArrow,Varid "b"
                            ,Special SemiColon,Conid "B",Varid "x",ReservedOP RArrow,Varid "c",Special RCurly]
                                                      ~=? TECase (TEVar "x") [TAlt (TPat "A" []) (TEVar "b"),TAlt (TPat "B" [TVarID "x"]) (TEVar "c")]

test4 = "application 1 1 1" ~: expParser [Literal (TInteger 1),Literal (TInteger 1),Literal (TInteger 1)] -- 1 1 1
                                                      ~=? TEApp (TEApp (TELiteral (TInteger 1)) (TELiteral (TInteger 1))) (TELiteral (TInteger 1))
test5 = "application 1 (1 1)" ~: expParser [Literal (TInteger 1),Special LParen,Literal (TInteger 1),Literal (TInteger 1),Special RParen] -- 1 (1 1)
                                                      ~=? TEApp (TELiteral (TInteger 1)) (TEApp (TELiteral (TInteger 1)) (TELiteral (TInteger 1)))

