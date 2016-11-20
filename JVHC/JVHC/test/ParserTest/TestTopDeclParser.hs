module ParserTest.TestTopDeclParser(tests)
  where

import Parsing.Lexer
import Parsing.Parser
import Parsing.ParsingAST
import Test.HUnit

test1 = "Simple data decl" ~: jvhcParse [Special LCurly, ReservedID Data, Conid "A", ReservedOP Equal, Conid "B", Special RCurly]  ~=? TTopDecls [TData $ TDatadecl(TSimpleType "A" []) [TConstr "B" []]]
test2 = "Nested data decl" ~: jvhcParse [Special LCurly, ReservedID Data, Conid "A", Varid "b", ReservedOP Equal, Conid "B", Special LParen, -- {data A b = B (IO
                                         Conid "IO", Varid "a", Special RParen, ReservedOP Pipe, Conid "C", Varid "d", Special RCurly]
                                                                                                 ~=? TTopDecls [TData $ TDatadecl (TSimpleType "A" ["b"])
                                                                                                                 [ TConstr "B" [TATypeAp (TGTyCon "IO") (TTyVar "a")],
                                                                                                                   TConstr "C" [TTyVar "d"]]]
tests = test [test1, test2]


