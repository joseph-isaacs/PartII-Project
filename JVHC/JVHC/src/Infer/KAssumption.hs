module Infer.KAssumption where

import Infer.KSubst
import Infer.Id

import CoreAST.Kind


data KAssumption = Id :>: Kind
  deriving (Show,Eq)

instance Kinds KAssumption where
  applyK s (i :>: k) = i :>: (applyK s k)
  kv (_ :>: k) = kv k

find :: Monad m => Id -> [KAssumption] -> m Kind
find i as = safeHead [k | (i' :>: k) <- as, i' == i]
  where
    safeHead [] = fail $ "cannot find Data Type Constructor " ++ (show i)
    safeHead x  = return $ head x
