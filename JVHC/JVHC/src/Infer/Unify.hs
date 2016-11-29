module Infer.Unify
  where

import CoreAST.Kind
import CoreAST.Types
import Infer.Subst

import Control.Monad

mgu :: Monad m =>  Type -> Type -> m Subst
mgu (TVar u) t        = varBind u t
mgu t        (TVar u) = varBind u t
mgu (TCon tc1) (TCon tc2)
     | tc1 == tc2 = return nullSubst
mgu (TAp l1 r1) (TAp l2 r2) = do s1 <- mgu l1 l2
                                 s2 <- mgu (apply s1 r1) (apply s1 r2)
                                 return (s2 @@ s1)
mgu t1       t2   = fail "cannot unity types"

varBind :: Monad m => Tyvar -> Type -> m Subst
varBind u t | t == (TVar u)    = return nullSubst
            | u `elem` tv t    = fail "occurs check failed"
            | kind u == kind t = return (u +-> t)
            | otherwise        = fail "kind error"

match :: Monad m => Type -> Type -> m Subst
match (TVar u) t
  | kind u == kind t = return $ u +-> t
match (TCon t1) (TCon t2)
       | t1 == t2   = return nullSubst

match (TAp l1 r1) (TAp l2 r2) = do s1 <- match l1 l2
                                   s2 <- match r1 r2
                                   merge s1 s2

match t1 t2         = error $ "cannot match types " ++ (show t1) ++ " " ++ (show t2)
