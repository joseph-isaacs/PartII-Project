module Desugar.DExpr where

import CoreAST.Kind
import CoreAST.DataCon

import Infer.Id
import CoreAST.Literal
import Infer.Assumption
import Infer.Scheme


data DataType = DT { dTName :: String, dKind :: Kind, dConstrs :: [Assumption], tCon :: TyCon }
  deriving Show

data Pat = PVar Id
         | PLit Literal
         | PCon DataCon Assumption [Pat]
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
