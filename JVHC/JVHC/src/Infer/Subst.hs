module Infer.Subst
  where

import CoreAST.Types
import CoreAST.Kind

import Data.List(nub, union, intersect)
import Control.Monad


type Subst = [(Tyvar, Type)]

nullSubst :: Subst
nullSubst = []

(+->)  :: Tyvar -> Type -> Subst
u +-> t = [(u,t)]

class Types t where
  apply :: Subst -> t -> t
  tv    :: t -> [Tyvar]

instance Types Type where
  apply s x@(TVar u) = case lookup u s of
                       Just t -> t
                       Nothing -> x
  apply s (TAp l r) = TAp (apply s l) (apply s r)
  apply s t         = t

  tv (TVar u)       = [u]
  tv (TAp l r)      = (tv l) `union` (tv r)
  tv t              = []

instance Types a => Types [a] where
  apply s = map (apply s)
  tv      = nub . (concatMap tv)

infixr 4 @@
(@@)  :: Subst -> Subst -> Subst
s1 @@ s2 = [(u, s1 `apply` t) | (u,t) <- s2] ++ s1


merge :: (Monad m) => Subst -> Subst -> m Subst
merge s1 s2 = if agree then return merged else fail "subst merge failed"
  where
    domain = map fst
    merged = s1 ++ s2
    agree = all (\v -> apply s1 (TVar v) == apply s2 (TVar v))
                (domain s1 `intersect` domain s2)




