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

import Data.Maybe (isJust, fromJust)

import qualified Data.List as L
import Data.Graph(stronglyConnComp,flattenSCC)

dExpr :: Monad m => [DataType] -> (TypeList,[Assumption]) -> PP.Exp -> m DE.Expr
dExpr _ (_,as) (PP.TEVar v)  =
  do let var = find v as
     return (if isJust var then DE.Const (v :>: fromJust var) else DE.Var v)
dExpr _ _      (PP.TELiteral l) = return $ DE.Lit l
dExpr _ (_,as) (PP.TEConstr c)  =
  do assump <- find c as
     return $ DE.Const (c :>: assump)

dExpr dt as (PP.TEApp e1 e2) =
  do e1' <- dExpr dt as e1
     e2' <- dExpr dt as e2
     return $ DE.Ap e1' e2'

dExpr dt as (PP.TELambda id e) =
  do e' <- dExpr dt as e
     return $ DE.Lam id e'

dExpr dt as (PP.TELet d e) =
  do bg <- dDecl dt as [d]
     e' <- dExpr dt as e
     return $ DE.Let bg e'

dExpr dt as (PP.TECase e alts) =
  do e'    <- dExpr dt as e
     alts' <- mapM (dCaseAlt dt as) alts
     return $ DE.Case e' alts'

dCaseAlt :: Monad m => [DataType] -> (TypeList,[Assumption]) -> PP.Alt -> m (DE.Pat,DE.Expr)
dCaseAlt dt as (PP.TAlt pat exp) =
  do pat' <- dPat  dt as pat
     exp' <- dExpr dt as exp
     return (pat',exp')

dDecl :: Monad m => [DataType] -> (TypeList,[Assumption]) -> [PP.Decl] -> m DE.BindGroup
dDecl dt as decls =
  do let (explDecl,funDecl)   = splitDecl decls
     (expl,impl)  <- predSplitImplExpl explDecl funDecl
     dExpl        <- mapM (dExpl dt as) expl
     dImpl        <- mapM (dImpl dt as) impl
     let dImpls = splitDep dImpl
     return (dExpl,dImpls)

dExpl :: Monad m => [DataType] -> (TypeList,[Assumption]) -> (TSGenDecl,TFunDecl) -> m DE.Expl
dExpl dt as ((TSGendecl vId typ), funs) =
  do dType <- dAType   as typ
     alts  <- dFunDecl dt as funs
     return (vId,quantify (tv dType) dType,alts)


splitDep :: [DE.Impl] -> [[DE.Impl]]
splitDep f = map flattenSCC (stronglyConnComp implNodes)
  where implNodes = map (\i@(id,e) -> (i,id,fv i)) f


dImpl :: Monad m => [DataType] -> (TypeList,[Assumption]) -> TFunDecl -> m DE.Impl
dImpl dt as f@(TFundecl (TVarPat id _) _) =
  do alt <- dFunDecl dt as f
     return (id,alt)

dFunDecl :: Monad m => [DataType] -> (TypeList,[Assumption]) -> TFunDecl -> m DE.Alt
dFunDecl dt as (TFundecl (TVarPat id ids) exp) =
  do p'   <- dPat dt as (TVarID id)
     ps'  <- mapM (dPat dt as) (map TVarID ids)
     expn  <- dExpr dt as exp
     exp'  <- dExpr dt as $ foldr (\i acc -> TELambda i acc) exp ids -- (make pats into lambdas)
     return exp'



predSplitImplExpl :: Monad m => [TSGenDecl] -> [TFunDecl] -> m ([(TSGenDecl,TFunDecl)],[TFunDecl])
predSplitImplExpl gs funs = driver gs funs []
  where
    driver [] funs expl = return (reverse expl, funs)
    driver ((g@(TSGendecl var _)):gs) funs expl =
      if toManyFuns then fail $ "to many function bodies for " ++ (show var)
      else
      if funsEmpty then fail $ "type signature for " ++ var ++ " lacks accompanying binding"
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

