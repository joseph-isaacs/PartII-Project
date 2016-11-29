{-# LANGUAGE FlexibleInstances #-}

module Infer.CoreExprFixer where

import CoreAST.CoreExpr

import CoreAST.Var

import CoreAST.Types
import CoreAST.Kind

import Infer.CoreExprSubst
import Infer.Subst
import Infer.Unify (mgu)

import Data.Maybe
import Data.List(foldl')

class SystemF a where
  -- [Tycon] are the already bound variables.
  addLambda :: [Tyvar]      -> a -> a
  addTvApp  :: Monad m => [Tyvar] -> [(CoreExpr,Type)] -> a -> m a

instance SystemF Var where
  addLambda bind (MkVar { varName = n, varType = t }) =
    (MkVar { varName = n, varType = t})
  addLambda bind v = v
  addTvApp b s v = return v


instance SystemF (Bind Var) where
  addLambda bind (NonRec a e) = (NonRec a (addLambda bind e))
  addLambda bind (Rec bs)     = Rec $ map (\(v,e) -> (v, addLambda bind e)) bs
  addTvApp b s r = undefined

instance SystemF [Alt Var] where
  addLambda bind = map (addLambda bind)
  addTvApp b s  = mapM (addTvApp b s)

instance SystemF (Alt Var) where
  addLambda bind (ac, bs, e) = (ac, map (addLambda bind) bs, addLambda bind e)
  addTvApp b s (ac,bs,e) =
    do let bss = map (\(MkVar { varName = v, varType = t }) -> (Var v,t)) bs
       e' <- addTvApp b (bss++s) e
       return (ac,bs,e')


instance SystemF (Var,CoreExpr) where
  addLambda bound (v,core) = (addLambda bound v, addLambda bound core)

  addTvApp bound scope (v,core) =
    do c' <- addTvApp bound scope core
       return  (v,c')

addBigLambda :: [Tyvar] -> CoreExpr -> CoreExpr
addBigLambda t e = foldr (\x acc -> Lam (MkTVar { tvarName = x }) acc) e t

instance SystemF CoreExpr where
  addLambda bound (App e e') = App (addLambda bound e) (addLambda bound e')
  addLambda bound (Lam v e) = addBigLambda (filter (`notElem` bound) tvars) lam
    where tvars = (tv v)
          lam  = Lam (addLambda bound v) (addLambda (tvars ++ bound) e)

  addLambda bound (Let b e) = (Let b' e')
    where b' = addLambda bound b
          e' = addLambda bound e

  addLambda bound (Case e t al) = (Case e' t al')
    where e'  = addLambda bound e
          al' = addLambda bound al

  addLambda _ e = e

  addTvApp bound scope (App e1 e2) =
    do e1t <- lookupF e1 scope
       e2t <- lookupF e2 scope
       let (e1tl,_) = splitAp e1t
           e1ltv            = filter (`notElem` bound) $ tv e1tl
           e2tv             = filter (`notElem` bound) $ tv e2t
       subst <- mgu e1tl e2t
       e1' <- addTvApp bound scope e1
       e2' <- addTvApp bound scope e2
       let e1'' = applyTypes (apply subst $ map TVar e1ltv) e1'
           e2'' = applyTypes (map TVar e2tv) e2'
       return $ addBigLambda e2tv (App e1'' e2'')

  addTvApp bound scope (Lam v@(MkVar { varName = n, varType = t}) e) =
    do e' <- (addTvApp bound ((Var n,t):scope) e)
       return $ Lam v e'

  addTvApp bound scope (Lam v@(MkTVar {tvarName = t}) e) =
    do e' <- addTvApp (t : bound )scope e
       return $ Lam v e'

  addTvApp bound scope (Let (NonRec b le) e) =
    do let (MkVar { varName = vn, varType = vt }) = b
           scope' = (Var vn, vt) : scope
       le' <- addTvApp bound scope' le
       e'  <- addTvApp bound scope' e
       return (Let (NonRec b le') e')


  addTvApp bound scope (Case e y als) =
    do als' <- addTvApp bound scope als
       e'   <- addTvApp bound scope e
       return (Case e' y als')

  addTvApp _ _ x = return x

lookupF :: Monad m => CoreExpr -> [(CoreExpr,Type)] -> m Type
lookupF e c = case lookup e c of
                Just x -> return x
                Nothing -> error $ "Cannot find expression " ++ (show e)

applyTypes :: [Type] ->  CoreExpr -> CoreExpr
applyTypes ts e = foldl' (\acc x -> App acc (Type x)) e ts

splitAp :: Type -> (Type,Type)
splitAp (TAp (TAp _ l) r) = (l,r)
splitAp t                 = error $ "cannot split " ++ (show t)

