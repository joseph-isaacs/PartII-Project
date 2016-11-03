module CoreAST
  where

import ParsingAST(Literal)


data Expr b
  = Var   Id
  | Lit   Literal
  | App   (Expr b)        (Arg b)
  | Lam   b               (Expr b)
  | Let   (Bind b)        (Expr b)
  | Case  (Expr b) b Type [Alt b]
  | Type  Type


type Arg b = Expr b
type Alt b = (AltCon, [b], Expr b)

data AltCon = DataAlt DataCon | LitAlt Literal | DEFAULT

data Bind b = NonRec b (Expr b) | Rec [(b, Expr b)]
