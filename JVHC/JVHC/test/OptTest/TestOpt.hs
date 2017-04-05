module OptTest.TestOpt where

import CoreAST.CoreExpr
import CoreAST.Types
import CoreAST.Kind
import CoreAST.TScheme
import CoreAST.Var
import CoreAST.Literal

import Opt.Inlining

import Test.HUnit

tV    = starTVar "a"
tV2   = starTVar "b"

testUndefExpr = "Test Halting" ~: inlineOnce expr ~=? expr
  where expr  = [ExprDef idVar (Lam xId (App (Var "id") (Var "x")))]
        idVar = MkVar { varName = "id", varType = TScheme [tV] tV }
        xId   = MkVar { varName = "x" , varType = TScheme [tV2] tV2 }

testSimpleInline = "Simple let x = 2 in x" ~: inlineOnce simExpr ~=? inSimExpr
  where simExpr   = [ExprDef xVar (Let (ExprDef yVar (Lit (LitInt 2))) (Var "y"))]
        inSimExpr = [ExprDef xVar (Lit (LitInt 2))]
        xVar = MkVar "x" (emptyTS tInt)
        yVar = MkVar "y" (emptyTS tInt)

constProp = "case 2 of 2 -> 1" ~: inlineOnce caseExpr ~=? oneExpr
  where caseExpr = [ExprDef cVar (Case (Lit i2) tInt [(LitAlt i2, [], li1)])]
        i2       = LitInt 2
        li1      = Lit $ LitInt 1
        oneExpr  = [ExprDef cVar li1]
        cVar     = MkVar { varName = "c", varType = TScheme [tV] tV }

tests = test [testUndefExpr, testSimpleInline, constProp]
