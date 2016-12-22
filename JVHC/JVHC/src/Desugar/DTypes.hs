module Desugar.DTypes where

import Infer.Assumption
import Infer.Id

import CoreAST.Types

type TypeList = [(Id,Tycon)]

lookupTl :: Monad m => Id  -> TypeList -> m Tycon
lookupTl id lt =
  case lookup id lt of
    Just x  -> return x
    Nothing -> fail $ "Cannot find type " ++ (show id)
