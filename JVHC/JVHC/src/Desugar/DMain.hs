{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Desugar.DMain where

import Desugar.DPat
import Desugar.DAType
import Desugar.DTypes

import Parsing.ParsingAST as PP
import Infer.Scheme
import Infer.Subst
import Infer.TIMain        as TIM
import Infer.TIPat         as TIP
import Infer.Assumption

import qualified Data.List as L

dExpr :: Monad m => (TypeList,[Assumption]) -> PP.Exp -> m TIM.Expr
dExpr _      (PP.TEVar v)  = return $ TIM.Var v
dExpr _      (PP.TELiteral l) = return $ TIM.Lit l
dExpr (_,as) (PP.TEConstr c)  =
  do assump <- find c as
     return $ TIM.Const (c :>: assump)

dExpr as (PP.TEApp e1 e2) =
  do e1' <- dExpr as e1
     e2' <- dExpr as e2
     return $ TIM.Ap e1' e2'

dExpr as (PP.TELambda id e) =
  do e' <- dExpr as e
     return $ TIM.Lam ([TIP.PVar id],e')

dExpr as (PP.TELet d e) =
  do bg <- dDecl as [d]
     e' <- dExpr as e
     return $ TIM.Let bg e'

dExpr as (PP.TECase e alts) =
  do e'    <- dExpr as e
     alts' <- mapM (dCaseAlt as) alts
     return $ TIM.Case e' alts'

dCaseAlt :: Monad m => (TypeList,[Assumption]) -> PP.Alt -> m (TIP.Pat,TIM.Expr)
dCaseAlt as (PP.TAlt pat exp) =
  do pat' <- dPat  as pat
     exp' <- dExpr as exp
     return (pat',exp')

dDecl :: Monad m => (TypeList,[Assumption]) -> [PP.Decl] -> m TIM.BindGroup
dDecl as decls =
  do let (explDecl,funDecl)   = splitDecl decls
     (expl,impl)  <- predSplitImplExpl explDecl funDecl
     dExpl        <- mapM (dExpl as) expl
     dImpl        <- mapM (mapM $ dImpl as) (mergeDep impl)
     return (dExpl,dImpl)

dExpl :: Monad m => (TypeList,[Assumption]) -> (TSGenDecl,[TFunDecl]) -> m TIM.Expl
dExpl as ((TSGendecl vId typ), funs) =
  do dType <- dAType   as typ
     alts  <- mapM (dFunDecl as) funs
     return (vId,quantify (tv dType) dType,alts)


mergeDep :: [TFunDecl] -> [[TFunDecl]]
mergeDep f = [f]

dImpl :: Monad m => (TypeList,[Assumption]) -> TFunDecl -> m TIM.Impl
dImpl as f@(TFundecl (TVarPat id _) _) =
  do alt <- dFunDecl as f
     return (id,[alt])

dFunDecl :: Monad m => (TypeList,[Assumption]) -> TFunDecl -> m TIM.Alt
dFunDecl as (TFundecl (TVarPat id ids) exp) =
  do p'   <- dPat as (TVarID id)
     ps'  <- mapM (dPat as) (map TVarID ids)
     expn  <- dExpr as exp
     exp'  <- dExpr as $ foldr (\i acc -> TELambda i acc) exp ids -- (make pats into lambdas)
     return (ps',expn)



predSplitImplExpl :: Monad m => [TSGenDecl] -> [TFunDecl] -> m ([(TSGenDecl,[TFunDecl])],[TFunDecl])
predSplitImplExpl gs funs = driver gs funs []
  where
    driver [] funs expl = return (reverse expl, funs)
    driver ((g@(TSGendecl var _)):gs) funs expl =
      if funsEmpty then fail $ "type signautre for " ++ var ++ "lacks accompanying binding"
                   else driver gs other ((g,currentFuns):expl)
      where (currentFuns,other) = L.partition (\x -> var == genVar x) funs
            funsEmpty = currentFuns == []
            genVar (TFundecl (TVarPat v _)_) = v

splitDecl :: [Decl] -> ([TSGenDecl],[TFunDecl])
splitDecl decls = driver decls [] []
  where
    driver []                gen fun = (reverse gen, reverse fun)
    driver ((TGenDecl x):xs) gen fun = driver xs (expandGenDecl x ++ gen) fun
    driver ((TFunDecl x):xs) gen fun = driver xs gen (x:fun)
    expandGenDecl (TGendecl vars t) = map (\v -> TSGendecl v t) vars

