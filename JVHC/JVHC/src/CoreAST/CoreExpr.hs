module CoreExpr
  where

import ParsingAST(Literal)


data Expr = Var  Var
          | Lit  Literal
          | App  Expr    Expr
          | TApp Expr    Expr
          | Lam  Var     Expr
          | BLam Var     Expr
          | Let  Bind    Expr
          | Case Expr    Var  Alt
          | Type Type

type Arg = Expr
type Alt = (AltCon, [Var], Expr)

data AltCon = DataAlt DataCon | LitAlt Literal | DEFAULT

data Bind b = NonRec Var Expr | Rec [(Var, Expr)]
