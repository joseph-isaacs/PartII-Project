{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Infer.TIM where

import Control.Monad.State(State, MonadState, evalState,get,modify)
import Control.Monad

import Infer.Subst
import Infer.Scheme
import Infer.Unify
import CoreAST.Types
import CoreAST.Kind
import Infer.Id
import Infer.Assumption

newtype TI a = TI (State (Subst, Int) a)
  deriving (Functor, Applicative, Monad, MonadState (Subst, Int))

type Infer e t = [Assumption] -> e -> TI t

runTI :: TI a -> a
runTI (TI s) = evalState s (nullSubst, 0)

getSubst :: TI Subst
getSubst = liftM fst get

unify :: Type -> Type -> TI ()
unify t1 t2 = do s <- getSubst
                 u <- mgu (apply s t1) (apply s t2)
                 extSubst u
        where
          extSubst s' = modify $ \(s,n) -> (s @@ s',n)

newTVar :: Kind -> TI Type
newTVar k = do
  n <- liftM snd get
  modify $ \(s,n) -> (s,n+1)
  return $ TVar $ Tyvar (enumId n) k

freshInstance :: Scheme -> TI Type
freshInstance (Forall k t) = do ts <- mapM newTVar k
                                return (inst ts t)

class Instantiate t where
  inst :: [Type] -> t -> t

instance Instantiate Type where
  inst ts (TGen n)  = ts !! n
  inst ts (TAp l r) = TAp (inst ts l) (inst ts r)
  inst ts t         = t

instance Instantiate a => Instantiate [a] where
  inst ts = map (inst ts)


