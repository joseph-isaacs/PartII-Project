module Infer.Scheme where

import CoreAST.Types
import CoreAST.Kind
import Infer.Subst

data Scheme = Forall [Kind] Type
  deriving (Eq, Show)


instance Types Scheme where
  apply s (Forall ks t) = Forall ks (apply s t)
  tv (Forall ks t)      = tv t

quantify :: [Tyvar] -> Type -> Scheme
quantify vs t = Forall ks (apply s t)
  where vs' = [v | v <- tv t, v `elem` vs]
        ks  = map kind vs'
        s   = zip vs' (map TGen [0..])

toScheme   :: Type -> Scheme
toScheme t = Forall [] t

instance HasKind Scheme where
  kind (Forall _ t) = kind t
