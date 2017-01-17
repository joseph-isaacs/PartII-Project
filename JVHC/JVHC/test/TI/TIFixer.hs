module TI.TIFixer where

import CoreAST.CoreExpr
import CoreAST.TScheme
import CoreAST.Types
import CoreAST.Kind
import CoreAST.Var

import Infer.Id

import Infer.TyAppFixer
import Infer.TyFixerMonad

import Pipeline.Compiler (mkData)

import Test.HUnit

import Data.Map (fromList)


mkTV :: Int -> Type
mkTV i = TVar $ Tyvar (enumId i) Star

tv1 = mkTV 1
tv2 = mkTV 2
tv3 = mkTV 3
tv4 = mkTV 4
tv5 = mkTV 5

tr1 = TVar (Tyvar "0" Star)
tr2 = TVar (Tyvar "1" Star)
tr3 = TVar (Tyvar "2" Star)

comp = ExprDef (MkVar {varName = "comp", varType = TScheme [] ((tv1  `fn` tv2) `fn` ((tv3 `fn` tv1) `fn` (tv3 `fn` tv2)))}) (Lam (MkVar {varName = "f",varType = TScheme [] (tv1 `fn` tv2)}) (Lam (MkVar {varName = "g", varType = TScheme [] (tv3 `fn` tv1)}) (Lam (MkVar {varName = "x", varType = TScheme [] tv3}) (App (Var "f") (App (Var "g") (Var "x"))))))

compTest = ExprDef (MkVar {varName = "comp", varType = TScheme [tr1,tr2,tr3] ((tr1  `fn` tr2) `fn` ((tr3 `fn` tr1) `fn` (tr3 `fn` tr2)))}) (Lam (MkTVar { tvarName = tr1 }) (Lam (MkTVar {tvarName = tr2}) (Lam (MkTVar {tvarName = tr3}) (Lam (MkVar {varName = "f",varType = TScheme [] (tr1 `fn` tr2)}) (Lam (MkVar {varName = "g", varType = TScheme [] (tr3 `fn` tr1)}) (Lam (MkVar {varName = "x", varType = TScheme [] tr3}) (App (Var "f") (App (Var "g") (Var "x")))))))))

compTyMap = map mkData $ [(Lam (MkVar {varName = "f", varType = TScheme [] tv4}) (Lam (MkVar {varName = "g", varType = TScheme [] tv5}) (Lam (MkVar {varName = "x", varType = TScheme [] tv3}) (App (Var "f") (App (Var "g") (Var "x"))))),(tv1 `fn` tv2) `fn` ((tv3 `fn` tv1) `fn` (tv3 `fn` tv2))),(Lam (MkVar {varName = "g", varType = TScheme [] tv5}) (Lam (MkVar {varName = "x", varType = TScheme [] tv3 }) (App (Var "f") (App (Var "g") (Var "x")))),(tv3 `fn` tv1) `fn` tv3 `fn` tv2),(Lam (MkVar {varName = "x", varType = TScheme [] tv3}) (App(Var "f") (App (Var "g") (Var "x"))),tv3 `fn` tv2),(App (Var "f") (App (Var "g") (Var "x")),tv2),(App (Var "g") (Var "x"),tv1)]



compFXR :: Test
compFXR = "\\f g x -> f (g x) to system F" ~: show (runFXR (frxExprDef [] [] compTyMap comp)) ~?= show compTest

tests:: Test
tests = test [compFXR]
