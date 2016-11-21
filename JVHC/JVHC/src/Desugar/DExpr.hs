module Desugar.DExpr where

import Infer.Id
import CoreAST.Literal
import Infer.Assumption
import Infer.Scheme

data Pat = PVar Id
         | PLit Literal
         | PCon Assumption [Pat]
    deriving (Show)

data Expr = Var Id
          | Lit Literal
          | Const Assumption
          | Ap Expr Expr
          | Lam Id Expr
          | Let BindGroup Expr
          | Case Expr [(Pat,Expr)]
     deriving Show

type Alt = Expr


type Impl = (Id,Alt)
type Expl = (Id, Scheme, Alt)

type BindGroup = ([Expl],[[Impl]])

type Program = [BindGroup]
