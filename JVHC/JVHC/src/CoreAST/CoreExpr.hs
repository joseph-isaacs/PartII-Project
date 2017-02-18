{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module CoreAST.CoreExpr
  where

import GHC.Generics
import Control.DeepSeq

import CoreAST.Literal
import CoreAST.Types
import CoreAST.Var
import CoreAST.DataCon

import Infer.Id

type Binder = Var

type CoreExpr = Expr Binder

data Expr b
  = Var  !Id
  | Lit  !Literal
  | App  !(Expr b)     (Arg  b)
  | Lam  !b           !(Expr b)
  | Let  !(ExprDef b) !(Expr b)
  | Case !(Expr b)    !Type     ![Alt b]
  | Type !Type
  deriving (Eq,Show,Generic)

type Arg b = Expr b
type Alt b = (AltCon, [b], Expr b)

data AltCon = DataAlt DataCon | LitAlt Literal | DEFAULT
  deriving (Show,Eq,Generic)

data ExprDef b = ExprDef b (Expr b)
  deriving (Show,Eq,Generic)

type CoreExprDef = ExprDef Binder

type CoreExprDefs = [CoreExprDef]

instance NFData (Expr Var)
instance NFData AltCon
instance NFData (ExprDef Var)
