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


newtype IL a = IL (ReaderT CoreExprDefs (State [Id]) a)
  deriving (Functor, Applicative, Monad
           , MonadReader CoreExprDefs
           , MonadState [Id])

runIL :: CoreExprDefs -> IL a -> a
runIL defs (IL s) = evalState (runReaderT s defs) []

getDefs :: IL CoreExprDefs
getDefs = ask

getInlined :: IL [Id]
getInlined = get

setInlined :: [Id] -> IL ()
setInlined ids = put ids

addInlinedId :: Id -> IL ()
addInlinedId id = modify (\ids -> id : ids)

wasInlined :: Id -> IL Bool
wasInlined id =
  do ids <- getInlined
     return (id `elem` ids)


lookupExpr :: Id -> IL (Maybe CoreExpr)
lookupExpr id =
  do defs <- getDefs
     return $ fmap (\(ExprDef _ e) -> e) (find findPred defs)
  where
    findPred (ExprDef (MkVar { varName = v}) _) = id == v
    findPred _                                  = False

