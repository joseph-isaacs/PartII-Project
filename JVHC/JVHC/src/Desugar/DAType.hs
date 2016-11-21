module Desugar.DAType where

import Parsing.ParsingAST
import CoreAST.Types
import CoreAST.Kind

import Data.Maybe

import Infer.Assumption
import Desugar.DTypes

dAType :: Monad m => (TypeList,[Assumption]) -> AType -> m Type
dAType _      (TTyVar v) = return $ TVar (Tyvar v Star)
dAType (tl,_) (TGTyCon tc) =
  do tc <- lookupTl tc tl
     return (TCon tc)

dAType as (TATypeAp a1 a2) = do
  a1' <- dAType as a1
  a2' <- dAType as a2
  return $ TAp a1' a2'

dAType as (TATypeArrow a1 a2) = do
  a1' <- dAType as a1
  a2' <- dAType as a2
  return (a1' `fn` a2')
