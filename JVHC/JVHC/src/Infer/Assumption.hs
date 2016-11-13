module Infer.Assumption where

import Infer.Scheme
import Infer.Subst
import Infer.Id


data Assumption = Id :>: Scheme
  deriving (Show)

instance Types Assumption where
  apply s (i :>: sc) = i :>: (apply s sc)
  tv  (i :>: sc)     = tv sc

find :: Monad m => Id -> [Assumption] -> m Scheme
find i as = if schemes == [] then fail ("unbound identifier: " ++ i) else return (head schemes)
  where schemes = [sc | (i':>: sc) <- as, i == i']
