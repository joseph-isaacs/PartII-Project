{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Desugar.DMain where

import Desugar.DPat
import Desugar.DAType
import Desugar.DTypes
import Desugar.FreeVariables
import Desugar.DExpr as DE



import Parsing.ParsingAST as PP
import Infer.Scheme
import Infer.Subst
import Infer.Assumption


import qualified Data.List as L
import Data.Graph(stronglyConnComp,flattenSCC)

dExpr :: Monad m => (TypeList,[Assumption]) -> PP.Exp -> m DE.Expr
dExpr _      (PP.TEVar v)  = return $ DE.Var v
dExpr _      (PP.TELiteral l) = return $ DE.Lit l
dExpr (_,as) (PP.TEConstr c)  =
  do assump <- find c as
     return $ DE.Const (c :>: assump)

dExpr as (PP.TEApp e1 e2) =
  do e1' <- dExpr as e1
     e2' <- dExpr as e2
     return $ DE.Ap e1' e2'

dExpr as (PP.TELambda id e) =
  do e' <- dExpr as e
     return $ DE.Lam id e'

dExpr as (PP.TELet d e) =
  do bg <- dDecl as [d]
     e' <- dExpr as e
     return $ DE.Let bg e'

dExpr as (PP.TECase e alts) =
  do e'    <- dExpr as e
     alts' <- mapM (dCaseAlt as) alts
     return $ DE.Case e' alts'

dCaseAlt :: Monad m => (TypeList,[Assumption]) -> PP.Alt -> m (DE.Pat,DE.Expr)
dCaseAlt as (PP.TAlt pat exp) =
  do pat' <- dPat  as pat
     exp' <- dExpr as exp
     return (pat',exp')

dDecl :: Monad m => (TypeList,[Assumption]) -> [PP.Decl] -> m DE.BindGroup
dDecl as decls =
  do let (explDecl,funDecl)   = splitDecl decls
     (expl,impl)  <- predSplitImplExpl explDecl funDecl
     dExpl        <- mapM (dExpl as) expl
     dImpl        <- mapM (dImpl as) impl
     let dImpls = splitDep dImpl
     return (dExpl,dImpls)

dExpl :: Monad m => (TypeList,[Assumption]) -> (TSGenDecl,TFunDecl) -> m DE.Expl
dExpl as ((TSGendecl vId typ), funs) =
  do dType <- dAType   as typ
     alts  <- dFunDecl as funs
     return (vId,quantify (tv dType) dType,alts)


splitDep :: [DE.Impl] -> [[DE.Impl]]
splitDep f = map flattenSCC (stronglyConnComp implNodes)
  where implNodes = map (\i@(id,e) -> (i,id,fv i)) f

-- TODO: add dependancy reductions.



dImpl :: Monad m => (TypeList,[Assumption]) -> TFunDecl -> m DE.Impl
dImpl as f@(TFundecl (TVarPat id _) _) =
  do alt <- dFunDecl as f
     return (id,alt)

dFunDecl :: Monad m => (TypeList,[Assumption]) -> TFunDecl -> m DE.Alt
dFunDecl as (TFundecl (TVarPat id ids) exp) =
  do p'   <- dPat as (TVarID id)
     ps'  <- mapM (dPat as) (map TVarID ids)
     expn  <- dExpr as exp
     exp'  <- dExpr as $ foldr (\i acc -> TELambda i acc) exp ids -- (make pats into lambdas)
     return exp'



predSplitImplExpl :: Monad m => [TSGenDecl] -> [TFunDecl] -> m ([(TSGenDecl,TFunDecl)],[TFunDecl])
predSplitImplExpl gs funs = driver gs funs []
  where
    driver [] funs expl = return (reverse expl, funs)
    driver ((g@(TSGendecl var _)):gs) funs expl =
      if toManyFuns then fail $ "to many function bodies for " ++ (show var)
      else
      if funsEmpty then fail $ "type signature for " ++ var ++ "lacks accompanying binding"
                   else driver gs other ((g,head currentFuns):expl)
      where (currentFuns,other) = L.partition (\x -> var == genVar x) funs
            funsEmpty = currentFuns == []
            toManyFuns = length currentFuns > 1
            genVar (TFundecl (TVarPat v _)_) = v

splitDecl :: [Decl] -> ([TSGenDecl],[TFunDecl])
splitDecl decls = driver decls [] []
  where
    driver []                gen fun = (reverse gen, reverse fun)
    driver ((TGenDecl x):xs) gen fun = driver xs (expandGenDecl x ++ gen) fun
    driver ((TFunDecl x):xs) gen fun = driver xs gen (x:fun)
    expandGenDecl (TGendecl vars t) = map (\v -> TSGendecl v t) vars

