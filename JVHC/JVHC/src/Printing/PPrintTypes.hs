module Printing.PPrintTypes where

import Printing.PPrint
import Text.PrettyPrint

import CoreAST.Types
import CoreAST.Kind
import Infer.Scheme
import Infer.Assumption

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

instance PPrint Tyvar where
  pprint (Tyvar v k)  = text v

instance PPrint Kind where
  pprint    = ppkind 0
  parPprint = ppkind 10

ppkind             :: Int -> Kind -> Doc
ppkind d Star       = text "Star"
ppkind d (Kfun l r) = ppParen (d>=10)
                         (text "Kfun" <+> ppkind 10 l <+> ppkind 0 r)

instance PPrint Scheme where
  pprint (Forall ks qt)
    = (text "Forall" <+> pprint ks) $$ nest 2 (parPprint qt)

instance PPrint Assumption where
  pprint (i :>: s) = (text (show i) <+> text ":>:") $$ nest 2 (pprint s)
