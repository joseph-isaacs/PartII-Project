{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module CodeGen.CGMonad where

import CoreAST.Types

import CodeGen.JTypes

import Control.Monad.State(State, MonadState, evalState,get,modify)
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad
import Data.Text
import Data.String(fromString,IsString)

import Codec.JVM.Class (ClassFile)
import Codec.JVM.Types (FieldType)

data ScopeVar = ScopeVar Text FieldType
  deriving Show

data Scope = Scope ScopeVar [ScopeVar]
  deriving Show

type PreDefFunctionMap = [(Text,(Text,Bool))]

data CodeGenReader = CGR { funMap :: PreDefFunctionMap
                         , typeNameMap :: [(Text,Text)]
                         , debug :: Bool }

newtype CG a = CG (ReaderT CodeGenReader (WriterT [(Text,ClassFile)] (State ([Scope],Int))) a)
  deriving (Functor, Applicative, Monad
           , MonadWriter [(Text,ClassFile)]
           , MonadState ([Scope], Int)
           , MonadReader CodeGenReader)


runCG :: CodeGenReader     ->
         CG a              ->
         (a,[(Text,ClassFile)])
runCG pre (CG m) = evalState (runWriterT (runReaderT m pre)) ([],0)

getPreDefinedFunction :: Text -> CG (Maybe (Text,Bool))
getPreDefinedFunction funName = liftM (lookup funName) getCGReaderDef

getTypeNameMap :: CG [(Text,Text)]
getTypeNameMap = liftM typeNameMap ask

typeFromName :: Text -> CG Text
typeFromName n =
  do tNM <- getTypeNameMap
     return (maybe n id (lookup n tNM))

isDebug :: CG Bool
isDebug = liftM debug ask

getCGReaderDef :: CG PreDefFunctionMap
getCGReaderDef = liftM funMap ask

logClass :: Text -> ClassFile -> CG ()
logClass n c = tell [(n,c)]

getFreshInt :: CG Int
getFreshInt =
  do int <- liftM snd get
     modify $ \(s,_) -> (s,int+1)
     return int

updateFreshInt :: Int -> CG ()
updateFreshInt i = modify $ \(s,_) -> (s,i)

getScope :: CG [Scope]
getScope = liftM fst get

updateScope :: Scope -> CG ()
updateScope s = modify $ \(scope,i) -> (s:scope,i)

setScope :: [Scope] -> CG ()
setScope s = modify $ \(_,i) -> (s,i)

getParent :: CG (Text,JType)
getParent =
  do (Scope (ScopeVar pname ptype) _) <- liftM Prelude.head getScope
     return (pname,ptype)

newFunName :: Text -> CG Text
newFunName fnName = do
  i <- getFreshInt
  return $ fnName `mappend` (fromString $ show i)
