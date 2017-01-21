{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module CoreAST.RenameMonad where

import CoreAST.CoreExpr
import CoreAST.CoreExpr
import CoreAST.Var

import Infer.Id

import Control.Monad.State(State, MonadState, runState,get,modify,put)
import Control.Monad.Reader
import Control.Monad

import Data.List(find)
import Data.Map

newtype RN a = RN (State (Map Id Id,Int) a)
  deriving (Functor, Applicative, Monad
           , MonadState (Map Id Id,Int))

runRN :: Int -> RN a -> (a,Int)
runRN start (RN s) = (res,end)
  where (res,(_,end)) = runState s (empty,start)

getNewInt :: RN Int
getNewInt = liftM snd get

getMap :: RN (Map Id Id)
getMap = liftM fst get

addIdToMap :: Id -> Id -> RN ()
addIdToMap from to = modify (\(m,i) -> (insert from to m,i))

updateId :: Id -> RN Id
updateId id =
  do rename <- getMap
     return $ findWithDefault id id rename

addBinding :: Id -> RN Id
addBinding id =
  do id' <- mkNewName id
     addIdToMap id id'
     return id'



mkNewName :: String -> RN String
mkNewName nm =
  do i <- getNewInt
     setNewInt (i+1)
     return $ "_RENAME_" ++ show i ++ nm
   where
     setNewInt i = modify (\(id,_) -> (id,i))
