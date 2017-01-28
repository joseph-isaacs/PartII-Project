{-# LANGUAGE DeriveGeneric #-}

module Infer.Scheme where

import GHC.Generics
import Control.DeepSeq

import CoreAST.Types
import CoreAST.Kind
import Infer.Subst

import Printing.PPrint
import Text.PrettyPrint

data Scheme = Scheme [Kind] Type
  deriving (Eq, Show, Generic)


instance Types Scheme where
  apply s (Scheme ks t) = Scheme ks (apply s t)
  tv (Scheme ks t)      = tv t

quantify :: [Tyvar] -> Type -> Scheme
quantify vs t = Scheme ks (apply s t)
  where vs' = [v | v <- tv t, v `elem` vs]
        ks  = map kind vs'
        s   = zip vs' (map TGen [0..])

toScheme   :: Type -> Scheme
toScheme t = Scheme [] t

instance HasKind Scheme where
  kind (Scheme _ t) = kind t


instance PPrint Scheme where
  pprint (Scheme ks qt)
    = (text "Scheme" <+> pprint ks) $$ nest 2 (parPprint qt)

instance NFData Scheme
