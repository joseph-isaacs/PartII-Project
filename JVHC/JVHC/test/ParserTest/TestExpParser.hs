module ParserTest.TestExpParser(tests)
  where

import Parsing.Lexer
import Parsing.Parser (expParser)
import Parsing.ParsingAST
import Test.HUnit

tests = test [test1, test2, test3, test4, test5]

test1 = "lambda \\x -> \\y -> x" ~: expParser [ReservedOP BSlash,Varid "x",ReservedOP RArrow,ReservedOP BSlash,Varid "y",ReservedOP RArrow,Varid "x"]  ~=? TELambda "x" (TELambda "y" (TEVar "x"))
test2 = "let {let x = 2} in y"   ~: expParser [ReservedID Let,Special LCurly,Varid "x",ReservedOP Equal,Literal (LitInt 2),Special RCurly,ReservedID In,Varid "y"]
                                ~=? TELet (TFunDecl $ TFundecl (TVarPat "x" []) (TELiteral (LitInt 2))) (TEVar "y")
test3 = "case x of {A -> b; B x -> c}" ~: expParser [ReservedID Case,Varid "x",ReservedID Of,Special LCurly,Conid "A",ReservedOP RArrow,Varid "b"
                            ,Special SemiColon,Conid "B",Varid "x",ReservedOP RArrow,Varid "c",Special RCurly]
                                                      ~=? TECase (TEVar "x") [TAlt (TPat "A" []) (TEVar "b"),TAlt (TPat "B" [TVarID "x"]) (TEVar "c")]

test4 = "application 1 1 1" ~: expParser [Literal (LitInt 1),Literal (LitInt 1),Literal (LitInt 1)] -- 1 1 1
                                                      ~=? TEApp (TEApp (TELiteral (LitInt 1)) (TELiteral (LitInt 1))) (TELiteral (LitInt 1))
test5 = "application 1 (1 1)" ~: expParser [Literal (LitInt 1),Special LParen,Literal (LitInt 1),Literal (LitInt 1),Special RParen] -- 1 (1 1)
                                                      ~=? TEApp (TELiteral (LitInt 1)) (TEApp (TELiteral (LitInt 1)) (TELiteral (LitInt 1)))

