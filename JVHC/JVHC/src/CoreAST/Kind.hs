{-# LANGUAGE DeriveGeneric #-}

module CoreAST.Kind where

import GHC.Generics
import Control.DeepSeq

import Infer.Id

import Printing.PPrint
import Text.PrettyPrint

data Kind = Star | Kfun Kind Kind | KVar KVar
  deriving (Eq,Show,Generic)

data KVar = Kvar Id
  deriving (Show,Eq,Generic)

removeVars :: Kind -> Kind
removeVars (KVar _)     = Star
removeVars (Kfun k1 k2) = Kfun (removeVars k1) (removeVars k2)
removeVars k            = k

instance PPrint Kind where
  pprint    = ppkind 0
  parPprint = ppkind 10

ppkind             :: Int -> Kind -> Doc
ppkind _ Star       = text "Star"
ppkind _ (KVar v)   = text (show v)
ppkind d (Kfun l r) = ppParen (d>=10)
                         (text "Kfun" <+> ppkind 10 l <+> ppkind 0 r)


instance NFData Kind
instance NFData KVar
