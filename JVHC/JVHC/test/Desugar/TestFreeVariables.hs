module Desugar.TestFreeVariables where

import Test.HUnit
import Infer.TIMain
import Infer.TIPat
import Infer.TILit
import Parsing.ParsingAST
import CoreAST.Literal
import Desugar.FreeVariables
import Desugar.DExpr


test1 = "let (f x = let { g = \\y -> x } in g x y)" ~:
             (fv ("f",Lam "x" (Let ([],[[("g",Lam "y" (Var "x"))]]) (Ap (Ap (Var "g") (Var "x")) (Var "y")))))
             ~=? ["y"]

test2 = "Case (f x = case y x of { x -> 1; u -> n})"
        ~: (fv ("f",Lam "x" (Case (Ap (Var "y") (Var "x")) [(PVar "x",Lit (LitInt 1)),(PVar "u",Var "n")])))
       ~=? ["y","n"]

test3 = "let2 (f = let { x = let { y = 2} in x} in y x)"
        ~: (fv ("f",Let ([],[[("x",Let ([],[[("y",Lit (LitInt 2))]]) (Var "x"))]]) (Ap (Var "y") (Var "x"))))
       ~=? ["y"]

tests = test [test1,test2,test3]
