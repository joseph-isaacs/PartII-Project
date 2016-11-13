module CoreAST.Types
  where

import CoreAST.Kind
import Infer.Id

type FreshId = Int

data Type = TVar  Tyvar
          | TCon  Tycon
          | TAp   Type    Type
          | TGen  FreshId
          deriving (Show, Eq)


data Tyvar = Tyvar Id Kind
  deriving (Show, Eq)

data Tycon = Tycon Id Kind
  deriving (Show, Eq)

class HasKind t where
  kind :: t -> Kind

instance HasKind Tyvar where
  kind (Tyvar _ k) = k

instance HasKind Tycon where
  kind (Tycon _ k) = k

instance HasKind Type where
  kind   (TCon x)   = kind x
  kind   (TVar x)   = kind x
  kind k@(TAp  t _) = case kind t of
                      (Kfun _ k) -> k
                      (Star)     -> error $ (show k) ++  " is not valid kind"
  kind x            = error $ (show x) ++ "has no kind"

tChar  = TCon $ Tycon "Char" Star
tArrow = TCon $ Tycon "(->)" (Kfun Star
                          (Kfun Star Star))
tInt   = TCon $ Tycon  "Int"  Star

tList  = TCon $ Tycon "List" (Kfun Star Star)

fn :: Type -> Type -> Type
fn a b = TAp (TAp tArrow a) b
