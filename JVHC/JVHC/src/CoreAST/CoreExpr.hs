module CoreAST.CoreExpr
  where

import CoreAST.Literal
import CoreAST.Types
import CoreAST.Var
import CoreAST.DataCon

import Infer.Id

type Binder = Var

type CoreExpr = Expr Binder

data Expr b
  = Var  Id
  | Lit  Literal
  | App  (Expr b) (Arg  b)
  | Lam  b        (Expr b)
  | Let  (ExprDef b) (Expr b)
  | Case (Expr b) Type    [Alt b]
  | Type Type
  deriving (Eq,Show)

type Arg b = Expr b
type Alt b = (AltCon, [b], Expr b)

data AltCon = DataAlt DataCon | LitAlt Literal | DEFAULT
  deriving (Show,Eq)

data ExprDef b = ExprDef b (Expr b)
  deriving (Show,Eq)

type CoreExprDef = ExprDef Binder

type CoreExprDefs = [CoreExprDef]
