module CoreAST.Types
  where

import CoreAST.Kind
import Infer.Id
import Printing.PPrint
import Printing.PPrintTypes

import Printing.PPrint
import Text.PrettyPrint

type FreshId = Int

data Type = TVar  Tyvar
          | TCon  Tycon
          | TAp   Type    Type
          | TGen  FreshId
          deriving Eq


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
tUnit  = TCon $ Tycon "Unit" Star

fn :: Type -> Type -> Type
fn a b = TAp (TAp tArrow a) b


instance PPrint Type where
  pprint    = pptype 0
  parPprint = pptype 10

pptype d (TAp (TAp a x) y)
    | a==tArrow    = ppParen (d>=5) (pptype 5 x <+> text "`fn`"
                                                <+> pptype 0 y)
pptype d (TAp l r) = ppParen (d>=10) (text "TAp" <+> pptype 10 l
                                                 <+> pptype 10 r)
pptype d (TGen n)  = ppParen (d>=10) (text "TGen" <+> int n)
pptype d t
    | t==tList     = text "tList"
    | t==tArrow    = text "tArrow"
    | t==tUnit     = text "tUnit"
pptype d (TCon (Tycon i k))
                   = text ('t':i)
pptype d (TVar v)  = pprint v

instance Show Type where
  show = pretty

instance PPrint Tyvar where
  pprint (Tyvar v k)  = text v

