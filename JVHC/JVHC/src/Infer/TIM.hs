{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Infer.TIM where

import CoreAST.CoreExpr
import CoreAST.Types

import Control.Monad.State(State, MonadState, runState,get,modify)
import Control.Monad.Writer
import Control.Monad

import Infer.Subst
import Infer.Scheme
import Infer.Unify
import CoreAST.Types
import CoreAST.Kind
import Infer.Id
import Infer.Assumption

import Infer.CoreExprSubst

import qualified Data.Map as M
import Data.List (groupBy)

newtype TI a = TI (WriterT [(Id,(CoreExpr,Type))] (State (Subst, (Int, Id))) a)
  deriving (Functor, Applicative, Monad
           , MonadWriter [(Id,(CoreExpr,Type))]
           , MonadState (Subst, (Int, Id)))

type Infer e t = [Assumption] -> e -> TI t

runTI :: TI a -> (a,M.Map Id [(CoreExpr,Type)])
runTI (TI s) = (res,mLog)
  where ((res,log),(sub,_)) = runState (runWriterT s) (nullSubst, (0,undefined))
        ceG  = map (mapFun sub) log
        mLog = M.fromListWith (++) ceG

mapFun :: Subst -> (Id,(CoreExpr,Type)) -> (Id,[(CoreExpr,Type)])
mapFun sub (id,(c,t)) = (id,[(c, t')])
  where t' = applyTillNoChange sub t

getSubst :: TI Subst
getSubst = liftM fst get

logExpr :: CoreExpr -> Type -> TI ()
logExpr e t =
  do v <- getId
     tell [(v,(e,t))]

unify :: Type -> Type -> TI ()
unify t1 t2 = do s <- getSubst
                 u <- mgu (apply s t1) (apply s t2)
                 extSubst u
        where
          extSubst s' = modify $ \(s,n) -> (s @@ s',n)

setId :: Id -> TI ()
setId v = modify $ \(s,(n,_)) -> (s,(n,v))

getId :: TI Id
getId = liftM (snd.snd) get

newTVar :: Kind -> TI Type
newTVar k = do
  n <- liftM (fst.snd) get
  modify $ \(s,(n,v)) -> (s,(n+1,v))
  return $ TVar $ Tyvar (enumId n) k

freshInstance :: Scheme -> TI Type
freshInstance (Scheme k t) = do ts <- mapM newTVar k
                                return (inst ts t)

class Instantiate t where
  inst :: [Type] -> t -> t

instance Instantiate Type where
  inst ts (TGen n)  = ts !! n
  inst ts (TAp l r) = TAp (inst ts l) (inst ts r)
  inst ts t         = t

instance Instantiate a => Instantiate [a] where
  inst ts = map (inst ts)

