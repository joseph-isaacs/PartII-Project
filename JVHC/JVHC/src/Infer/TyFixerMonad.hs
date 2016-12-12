{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Infer.TyFixerMonad where

import CoreAST.Types
import CoreAST.Kind
import CoreAST.CoreExpr
import CoreAST.TScheme

import Infer.Subst
import Infer.Id

import Control.Monad
import Control.Monad.State(State, MonadState, evalState,get,modify)
import Control.Monad

newtype FXR a = FXR (State ([Tyvar],(Int,Int)) a)
  deriving (Functor, Applicative, Monad,
            MonadState ([Tyvar],(Int,Int)))

type TyFxr e r = [CoreExpr] -> Subst -> [(CoreExpr,(Type,[Tyvar]))] -> e -> FXR r

runFXR :: FXR a -> a
runFXR (FXR f) = evalState f ([],(0,0))

getBound :: FXR [Tyvar]
getBound = liftM fst get

addBound :: [Tyvar] -> FXR ()
addBound add =  modify $ \(b,i) -> (add ++ b,i)

setBound :: [Tyvar] -> FXR ()
setBound b = modify $ \(_,i) -> (b,i)

getNextFV :: FXR Int
getNextFV = liftM (fst.snd) get

setNextFV :: Int -> FXR ()
setNextFV i = modify $ \(b,(_,x)) -> (b,(i,x))

newTVar :: Kind -> FXR Tyvar
newTVar k =
  do n <- liftM (snd.snd) get
     modify $ \(s,(i,v)) -> (s,(i,v+1))
     return $ Tyvar (enumId2 n) k

quantifyTScheme :: Type -> FXR (TScheme,Subst)
quantifyTScheme t =
  do bound <- getBound
     start <- getNextFV
     let freeVars = filter (`notElem` bound) (tv t)
         end      = length freeVars + start
     setNextFV end
     let tyvar = map (\x -> Tyvar (show x) Star) [start..end-1]
         vars  = map TVar tyvar
         s     = zip freeVars vars
     addBound tyvar
     return (TScheme vars (apply s t),s)

freshTVars :: Type -> FXR (Type, Subst, Subst)
freshTVars t =
  do let tvar = tv t
         tlen = length tvar
     ts <- replicateM tlen (newTVar Star)
     let fsub = zip tvar (map TVar ts)
         rsub = zip ts (map TVar tvar)
     return (apply fsub t, fsub, rsub)
