module TI.TIExpr where

import Test.HUnit

import CoreAST.Types
import CoreAST.Kind
import CoreAST.Literal
import CoreAST.CoreExpr (ExprDef(..), CoreExprDef, CoreExprDefs)
import CoreAST.Var
import CoreAST.TScheme

import Infer.TIProgram
import Infer.Id
import Infer.Scheme
import Infer.Assumption

import Desugar.DExpr


getScheme :: ((a,[Assumption]),b) -> String -> Maybe Scheme
getScheme t name  = find name ass
  where ((_,ass),_) =  t


inferExpr :: Test
inferExpr = "f = \\x y -> case x of 1 -> 2; z -> y z" ~: getScheme (tiProgram [] prog) name ~=? Just exprType
  where
    name = "f"
    testExpr = Lam "x" (Lam "y" (Case (Var "x") [(PLit (LitInt 1), Lit (LitInt 2)), (PVar "z", Ap (Var "y") (Var "z")) ]))
    prog = [([],[[(name,testExpr)]])]
    exprType = Scheme [] (tInt `fn` ((tInt `fn` tInt) `fn` tInt))

inferRestricted :: Test
inferRestricted = "id x = x; id2 = id" ~: [getScheme progType nameId, getScheme progType nameId2] ~?= [idT,idT]
  where
    nameId  = "id"
    nameId2 = "id2"
    idExpr  = (nameId,  Lam "x" (Var "x"))
    id2Expr = (nameId2, Var "id")
    prog    = [([],[[idExpr,id2Expr]])]
    progType = tiProgram [] prog
    idT     = Just $ Scheme [Star] (TGen 0 `fn` TGen 0)


tests = test [inferExpr, inferRestricted]
