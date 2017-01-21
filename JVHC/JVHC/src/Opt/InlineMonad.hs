{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Opt.InlineMonad where


import CoreAST.CoreExpr
import CoreAST.CoreExpr
import CoreAST.Var

import Infer.Id

import Control.Monad.State(State, MonadState, evalState,get,modify,put)
import Control.Monad.Reader
import Control.Monad

import Data.List(find)


newtype IL a = IL (ReaderT CoreExprDefs (State ([Id],Int)) a)
  deriving (Functor, Applicative, Monad
           , MonadReader CoreExprDefs
           , MonadState ([Id],Int))

runIL :: CoreExprDefs -> IL a -> a
runIL defs (IL s) = evalState (runReaderT s defs) ([],0)

getNewInt :: IL Int
getNewInt = liftM snd get

setNewInt :: Int -> IL ()
setNewInt i = modify (\(id,_) -> (id,i))

getDefs :: IL CoreExprDefs
getDefs = ask

getInlined :: IL [Id]
getInlined = liftM fst get

setInlined :: [Id] -> IL ()
setInlined ids = modify (\(_,i) -> (ids,i))

addInlinedId :: Id -> IL ()
addInlinedId id = modify (\(ids,i) -> (id : ids,i))

wasInlined :: Id -> IL Bool
wasInlined id =
  do ids <- getInlined
     return (id `elem` ids)

mkNewName :: String -> IL String
mkNewName nm =
  do i <- getNewInt
     setNewInt (i+1)
     return $ "_INLINE_" ++ show i ++ nm


lookupExpr :: Id -> IL (Maybe CoreExpr)
lookupExpr id =
  do defs <- getDefs
     return $ fmap (\(ExprDef _ e) -> e) (find findPred defs)
  where
    findPred (ExprDef (MkVar { varName = v}) _) = id == v
    findPred _                                  = False

