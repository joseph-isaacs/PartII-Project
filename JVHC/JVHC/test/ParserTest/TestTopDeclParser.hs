module TestTopDeclParser(tests)
  where

import Lexer
import Parser
import AST
import Test.HUnit

test1 = "Simple data decl" ~: jvhcParse [Special LCurly, ReservedID Data, Conid "A", ReservedOP Equal, Conid "B", Special RCurly]  ~=? TTopDecls [TData (TSimpleType "A" []) [TConstr "B" []]]
test2 = "Nested data decl" ~: jvhcParse [Special LCurly, ReservedID Data, Conid "A", Varid "b", ReservedOP Equal, Conid "B", Special LParen, -- {data A b = B (IO
                                         Conid "IO", Varid"a", Special RParen, ReservedOP Pipe, Conid "C", Varid "d", Special RCurly]
                                                                                                 ~=? TTopDecls [TData (TSimpleType "A" ["b"])
                                                                                                                 [ TConstr "B" [TATypeNested [TGTyCon "IO", TTyVar "a"]],
                                                                                                                   TConstr "C" [TTyVar "d"]]]
tests = test [test1, test2]


