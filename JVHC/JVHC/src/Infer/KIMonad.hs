{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Infer.KIMonad where

import Infer.KSubst
import Infer.KAssumption
import Infer.Id
import Infer.KUnify

import CoreAST.Kind

import Control.Monad
import Control.Monad.State(State, MonadState, evalState, get, modify)

newtype KI a = KI (State (KSubst, Int) a)
  deriving (Functor, Applicative, Monad, MonadState (KSubst, Int))

type KInfer e t = [KAssumption] -> e -> KI t

runKI :: KI a -> a
runKI (KI a) = evalState a (kemptySubst, 0)

getKSubst :: KI KSubst
getKSubst = liftM fst get

unify :: Kind -> Kind -> KI ()
unify k1 k2 = do s <- getKSubst
                 u <- kmgu (applyK s k1) (applyK s k2)
                 extKSubst u
        where
          extKSubst s' = modify $ \(s,n) -> (s @@@ s', n)

newKVar :: KI Kind
newKVar = do
  num <- liftM snd get
  modify $ \(s,n) -> (s,n+1)
  return $ KVar $ Kvar $ enumId num





