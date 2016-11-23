module Infer.TIMain where

import CoreAST.Types
import CoreAST.Kind
import CoreAST.Var
import CoreAST.Literal
import qualified CoreAST.CoreExpr as C

import Desugar.DExpr

import Infer.Subst
import Infer.Scheme
import Infer.Unify
import Infer.CoreExprSubst
import Infer.TIM
import Infer.Id
import Infer.TILit
import Infer.TIPat
import Infer.Assumption

import Data.List((\\),union)


tiExpr :: Infer Expr (C.CoreExpr,Type)
tiExpr as (Var i) =
  do sc <- find i as
     t  <- freshInstance sc
     return (C.Var i,t)

tiExpr _  (Lit l) =
  do t <- tiLit l
     return (C.Lit l, t)

tiExpr as (Const (i :>: sc)) =
  do t <- freshInstance sc
     return (C.Var i, t)

tiExpr as (Ap e1 e2) =
  do (e1',t1) <- tiExpr as e1
     (e2',t2) <- tiExpr as e2
     t        <- newTVar Star
     unify t1 (t2 `fn` t)
     return (C.App e1' e2',t)


tiExpr as (Let bg e) =
  do (bind,as')  <- tiBindGroup as bg
     let (v,expr) = head bind
     (e', t')    <- tiExpr (as' ++ as) e
     s           <- getSubst
     return (C.Let (C.NonRec v expr) e', t')

tiExpr as (Lam p expr) =
  do (as', ts) <- tiPat (PVar p)
     (e',t)    <- tiExpr (as' ++ as) expr
     s <- seq t getSubst
     return (C.Lam (MkVar { varName = p, varType = ts }) e', ts `fn` t)

tiExpr as (Case e cases)
  = do (e',t) <- tiExpr as e
       v <- newTVar Star
       let tiCase (pat,body) =
             do (dCon,vars,as',t') <- tiCaseAlt pat
                unify t t'
                (e'',retT) <- tiExpr (as' ++ as) body
                unify v retT
                return (dCon,vars,e'')
       alts <-  mapM tiCase cases
       s    <- getSubst
       return (C.Case e' v alts, v)

tiAlt :: [Assumption] -> Alt -> Type -> TI (C.CoreExpr, Type)
tiAlt as alts t =
  do (e,pt) <- tiExpr as alts
     unify t pt
     s' <- getSubst
     return (e, apply s' pt)

-- TIImpl --


restricted :: [Impl] -> Bool
restricted bs = any simple bs
  where simple (id, (Lam _ _)) = False
        simple (_ , s  ) = True



tiImpls :: Infer [Impl] (C.CoreExprDefs,[Assumption])
tiImpls as bs =
  do ts <- mapM (\_ -> newTVar Star) bs
     let is    = map fst bs
         scs   = map toScheme ts
         as'   = zipWith (:>:) is scs ++ as
         alt   = map snd bs
     impls <- sequence (zipWith (tiAlt as') alt ts)
     s <- getSubst
     let pss = map snd impls
         es  = map fst impls
         ps' = apply s pss
         ts' = apply s ts
         fs  = tv (apply s as)
         vss = map tv ts'
         gs  = (foldr1 union vss) \\ fs
     s' <- getSubst
     if restricted bs then
        let gs'  = gs \\ (tv ps')
            scs' = map (quantify gs') ts'
         in  return (zipWith3 zipVECore is (apply s' es) ts',zipWith (:>:) is scs')
     else
       let scs' = map (quantify gs) ts'
        in return (zipWith3 zipVECore is (apply s' es) ts',zipWith (:>:) is scs')

zipVECore :: Id -> C.CoreExpr -> Type -> (Var,C.CoreExpr)
zipVECore id e t = (MkVar { varName = id, varType = t }, e)

-- TIExpl --


tiExpl :: [Assumption] -> Expl -> TI ((Var,C.CoreExpr),Type)
tiExpl as (i,sc,alt) =
  do t <- freshInstance sc
     (e,p) <-  tiAlt as alt t
     s     <- getSubst
     let ps  = apply s p
         t'  = apply s t
         fs  = tv (apply s as)
         gs  = (tv t' ++ seq ps []) \\ fs
         sc' = quantify gs t'
     if sc /= sc' then error $ "signature too general for " ++ i
     else return ((MkVar { varName = i, varType = apply s ps },apply s e),apply s ps)


-- TIBind --


tiBindGroup :: Infer BindGroup (C.CoreExprDefs,[Assumption])
tiBindGroup as (es,is) =
  do let as' = [v :>: sc | (v,sc,_) <- es]
     (eImpl,as'') <- tiSeq tiImpls (as' ++ as) is
     expls <- mapM (tiExpl (as'' ++ as' ++ as)) es
     return (map fst expls ++ eImpl,as'' ++ as')

tiSeq :: Infer tiI (C.CoreExprDefs,[Assumption]) -> Infer [tiI] (C.CoreExprDefs,[Assumption])
tiSeq _  _  [] = return ([],[])
tiSeq ti as (bs:bss) = do (expr, as' ) <- ti as bs
                          (expr',as'') <- tiSeq ti (as' ++ as) bss
                          return (expr ++ expr', as'' ++ as')
