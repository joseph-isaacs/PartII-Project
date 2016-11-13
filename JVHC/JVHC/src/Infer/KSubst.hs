module Infer.KSubst where

import CoreAST.Kind
import Control.Arrow
import Data.List(union, nub, intersect)


type KSubst = [(KVar,Kind)]


kemptySubst :: KSubst
kemptySubst = []

(++->) :: KVar -> Kind -> KSubst
v ++-> k = [(v,k)]

class Kinds k where
  applyK :: KSubst -> k -> k
  kv     :: k -> [KVar]

instance Kinds Kind where
  applyK s x@(KVar id) = case lookup id s of
                          Just t -> t
                          Nothing -> x
  applyK s (Kfun k1 k2) = Kfun (applyK s k1) (applyK s k2)
  applyK s t           = t

  kv (KVar kvar)      = [kvar]
  kv (Kfun k1 k2)     = (kv k1) `union` (kv k2)
  kv Star             = []

instance Kinds a => Kinds [a] where
  applyK s = map (applyK s)
  kv      = nub . (concatMap kv)

--instance Kinds
--  applyK s = map (id *** (applyK s))
--  kv x = undefined

infixr 4 @@@
(@@@) :: KSubst -> KSubst -> KSubst
s1 @@@ s2 = [(kv, s1 `applyK` k) | (kv,k) <- s2] ++ s1

merge :: (Monad m) => KSubst -> KSubst -> m KSubst
merge s1 s2 = if agree then return merged else fail "ksubst merge failed"
  where
    domain = map fst
    merged = s1 ++ s2
    agree = all (\v -> applyK s1 (KVar v) == applyK s2 (KVar v))
            (domain s1 `intersect` domain s2)


