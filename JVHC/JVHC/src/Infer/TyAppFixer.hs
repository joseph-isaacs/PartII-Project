module Infer.TyAppFixer where

import CoreAST.Types
import CoreAST.Kind
import CoreAST.TScheme
import CoreAST.Var
import CoreAST.CoreExpr
import CoreAST.DataCon

import Infer.Unify
import Infer.Subst
import Infer.Id
import Infer.Scheme

import Infer.CoreExprSubst
import Infer.TyFixerMonad

import Data.List


frxExpr :: TyFxr (CoreExpr,Type) CoreExpr
-- First we create new type variables for one of the two applications.
-- Then unify these two type variables, then remove the new type variables
-- and any *x* type variables. Using only quantified tyvars.
frxExpr b sub mp (App e1 e2,t) =
  do (e1t,e1LeftTv) <- lookupF e1 mp
     (e2tl,e2tvl)   <- lookupF e2 mp
     (e2t,e2FSub,e2RSub) <- freshTVars e2tl
     bou <- getBound
     let bound = map fst sub ++ bou
         e2tv  = apply e2FSub e2tvl
     (e1Left,e2Right) <- splitAp e1t

     subst1 <- mgu e1Left e2t
     let subFun0 x = apply e2RSub $ apply subst1 x
     subst2 <- mgu (subFun0 e2Right) (subFun0 t)
     let subFun2 x = apply subst2 (apply e2RSub x)
         subFun1 x = subFun2 (apply subst1 x)
     e1' <- frxExpr b sub mp (e1,subFun1 e1t)
     e2' <- frxExpr b sub mp (e2,subFun1 e2t)
     let e1'' = if ground e1' then applyTypes (subFun1 (map TVar e1LeftTv)) e1'
                              else e1'
         e2'' = if ground e2' then applyTypes (subFun1 (map TVar e2tv)) e2'
                              else e2'
     return (App e1'' e2'')


frxExpr b sub mp (Lam v@(MkVar { varName = n, varType = TScheme [] lt}) e,t) =
  do (fn,body) <- splitAp t
     subst <- match fn lt
     let v' = MkVar { varName = n, varType = TScheme [] (apply sub lt) }
     e' <- frxExpr (Var n : b) sub ((Var n,(fn,[])) : mp) (e,body)
     return (Lam v' e')

frxExpr b sub mp (Lam v e,t) =
  do e' <- frxExpr b sub mp (e,t)
     return (Lam v e')

frxExpr b sub mp (l@(Lam _ _),t) =
  fail $ "scheme was quantified " ++ (show l)

frxExpr b sub mp ((Let binder e),t) =
  do bound <- getBound
     (b'@(ExprDef bi be)) <- frxExprDef b sub mp binder
     setBound bound
     let (MkVar { varName = n, varType = TScheme a st }) = bi
     e' <- frxExpr (Var n:b) sub ((Var n,(apply sub st,tv $ apply sub a)):mp) (e,t)
     return (Let  b' e')

frxExpr b sub mp ((Case e ct a),t) =
  do subst <- match ct t
     (e1t,e1tv) <- lookupF e mp
     let e1t' = apply sub (apply subst e1t)
     let sub' = sub @@ subst
     e'   <- frxExpr b sub' mp (e,e1t')
     alts <- frxAlts b sub' mp (a,t)
     return (Case e' t alts)

frxExpr _ _ _ (ty@(Type _),t) = return ty

frxExpr b sub mp (v,t) =
  do (idT,idTtv) <- lookupF v mp
     subst <- mgu idT t
     let  v' = applyTypes (apply subst (map TVar idTtv)) v
     return v'


frxAlts :: TyFxr ([Alt Binder],Type) [Alt Binder]
frxAlts b sub mp (alts,t) =
  mapM (\x -> frxAlt b sub mp (x,t)) alts


frxAlt :: TyFxr (Alt Binder,Type) (Alt Binder)

frxAlt b sub mp ((dc,bs,exp),t) =
  do (dc',subst) <- frxAltCon b sub mp (dc,t)
     let sub' = sub @@ subst
         bs' = map (apply sub') bs
         nv  = map (\(MkVar { varName = n, varType = TScheme [] t }) -> (Var n,(t,[]))) bs'
     e' <- frxExpr (map fst nv ++ b) sub' (nv ++ mp) (exp,t)
     return (dc',bs',e')

frxAltCon :: TyFxr (AltCon,Type) (AltCon,Subst)
frxAltCon b sub mp ((DataAlt (MkDataCon { dName = dname, conType = TScheme [] ty, tName = tname, fields = f  })),t) =
  do (sc@(TScheme b ts),subst) <- quantifyTScheme (apply sub ty)
     let dc = DataAlt $  MkDataCon { dName = dname, conType = sc, tName = tname, fields = f }
         sub' = sub @@ subst
     return (dc,sub')

frxAltCon _ _ _ (dc,_) = return (dc,[])

ground :: CoreExpr -> Bool
ground (App _ _) = False
ground _         = True

frxExprDef :: TyFxr CoreExprDef CoreExprDef
frxExprDef b sub mp (ExprDef v@(MkVar {varName = n, varType = TScheme [] t}) e) =
  do let t' = apply sub t
     (sc@(TScheme forall ts),subst) <- quantifyTScheme t'
     let var = MkVar { varName = n, varType = sc }
     e' <- frxExpr b (sub ++ subst) mp (e,ts)
     let e'' = addBigLambda forall e'
     return (ExprDef var e'')


addBigLambda :: [Type] -> CoreExpr -> CoreExpr
addBigLambda t e = foldr (\x acc -> Lam (MkTVar { tvarName = x }) acc) e t

applyTypes :: [Type] ->  CoreExpr -> CoreExpr
applyTypes ts e = foldl' (\acc x -> App acc (Type x)) e ts


lookupF :: Monad m => CoreExpr -> [(CoreExpr,(Type,[Tyvar]))] -> m (Type,[Tyvar])
lookupF e c = case lookup e c of
                Just x -> return x
                Nothing -> error $ "\nCannot find expression\n " ++ (show e) ++ "\n" ++ (show c)

splitAp :: Monad m => Type -> m (Type,Type)
splitAp (TAp (TAp _ l) r) = return (l, r)
splitAp t                 = fail $ "cannot split " ++ (show t)
