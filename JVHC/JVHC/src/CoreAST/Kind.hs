module CoreAST.Kind where

import Infer.Id

data Kind = Star | Kfun Kind Kind | KVar KVar
  deriving (Eq,Show)

data KVar = Kvar Id
  deriving (Show,Eq)

removeVars :: Kind -> Kind
removeVars (KVar _)     = Star
removeVars (Kfun k1 k2) = Kfun (removeVars k1) (removeVars k2)
removeVars k            = k

--instance Show Kind where
--  show (Star) = "*"
--  show (Kfun k1 k2) = (show k1) ++ " -> (" ++ (show k2) ++ ")"
--  show (KVar v) = "(" ++ show v ++ ")"
