module CoreAST.CoreExpr
  where

import Parsing.ParsingAST(Literal)

import CoreAST.Types
import CoreAST.Var
import CoreAST.DataCon

import Infer.Id

type CoreExpr = Expr Var

data Expr b
  = Var  Id
  | Lit  Literal
  | App  (Expr b) (Arg  b)
  | Lam  b        (Expr b)
  | Let  (Bind b) (Expr b)
  | Case (Expr b) Type    [Alt b]
  | Type Type
  deriving Show

type Arg b = Expr b
type Alt b = (AltCon, [b], Expr b)

data AltCon = DataAlt DataCon | LitAlt Literal | DEFAULT
  deriving Show
data Bind b = NonRec b (Expr b) | Rec [(b, Expr b)]
  deriving Show
