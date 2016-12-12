module Infer.TIMain where

import CoreAST.Types
import CoreAST.Kind
import CoreAST.Var
import CoreAST.Literal
import CoreAST.TScheme
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

import Data.List((\\),union,intersect,partition)
import System.IO.Unsafe

tiExpr :: Infer Expr (C.CoreExpr,Type)
tiExpr as (Var i) =
  do sc <- find i as
     t  <- freshInstance sc
     return (C.Var i,t)

tiExpr _  (Lit l) =
  do t <- tiLit l
     let clit = C.Lit l
     logExpr clit t
     return (clit, t)

tiExpr as (Const (i :>: sc)) =
  do t <- freshInstance sc
     t' <- freshInstance sc
     logExpr (C.Var i) t'
     return (C.Var i, t)

tiExpr as (Ap e1 e2) =
  do (e1',t1) <- tiExpr as e1
     (e2',t2) <- tiExpr as e2
     t        <- newTVar Star
     unify t1 (t2 `fn` t)
     let ca = C.App e1' e2'
     logExpr ca t
     return (ca,t)


tiExpr as (Let bg e) =
  do id <- getId
     (bind,as')  <- tiBindGroup as bg
     setId id
     let expr = head bind
     (e', t')    <- tiExpr (as' ++ as) e
     s           <- getSubst
     let le = C.Let expr e'
     logExpr le t'
     return (le, t')

tiExpr as (Lam p expr) =
  do (as', ts) <- tiPat (PVar p)
     (e',t)    <- tiExpr (as' ++ as) expr
     s <- seq t getSubst
     let lam = C.Lam (MkVar { varName = p, varType = TScheme [] ts }) e'
     return (lam, ts `fn` t)


tiExpr as (Case e cases)
  = do (e',t) <- tiExpr as e
       logExpr e' t
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

tiAlt :: [Assumption] -> (Id,Alt) -> Type -> TI (C.CoreExpr, Type)
tiAlt as (id,alts) t =
  do setId id
     (e,pt) <- tiExpr as alts
     unify pt t
     s' <- getSubst
     return (e, t)


split :: Monad m => [Tyvar] -> [Tyvar] -> [Type] -> m ([Type],[Type])
split fs gs ps = do let (ds,rs) = partition (all (`elem` fs) . tv) ps
                    rs' <- defaultedPreds (fs ++ gs) rs
                    return (ds, rs ++ rs')


type Ambiguity = (Tyvar, [Type])

ambiguities :: [Tyvar] -> [Type] -> [Ambiguity]
ambiguities vs ps = [ (v, filter (elem v .tv) ps) | v <- tv ps \\ vs]

defaultedPreds :: Monad m => [Tyvar] -> [Type] -> m [Type]
defaultedPreds = withDefaults (\vps ts -> concat (map snd vps))

withDefaults :: Monad m => ([Ambiguity] -> [Type] -> a) -> [Tyvar] -> [Type] -> m a
withDefaults f vs ps
  | any null tss = fail "cannot resolve ambiguity"
  | otherwise    = return (f vps (map head tss))
      where vps = ambiguities vs ps
            tss = map candidates vps

candidates :: Ambiguity -> [Type]
candidates (v,qs) = error $ "got to candidates " ++ (show (v,qs))
--[ t' | let ts = [ t' | qs ],
--       all ((TVar v)==)ts]



restricted :: [Impl] -> Bool
restricted bs = any simple bs
  where simple (id, (Lam _ _)) = False
        simple (_ , s  ) = True


-- TIImpl --
tiImpls :: Infer [Impl] (C.CoreExprDefs,[Assumption])
tiImpls as bs =
  do ts <- mapM (\_ -> newTVar Star) bs
     let is    = map fst bs
         scs   = map toScheme ts
         as'   = zipWith (:>:) is scs ++ as
         alt   = map snd bs
     impls <- sequence (zipWith (tiAlt as') bs ts)
     s <- getSubst
     let pss = map snd impls
         es  = seq (show pss) (map fst impls)
         ts' = applyTillNoChange s ts
         fs  = tv (applyTillNoChange s as)
         vss = map tv ts'
         gs  = (foldr1 union vss) \\ fs
         psss = show pss
     s' <- getSubst
     (ds,rs) <- split fs (foldr1 intersect vss) ts'
     if restricted bs then
        let gs'  = gs
            scs' = map (quantify gs') ts'
        in return (zipWith3 zipVECore is (applyTillNoChange s' es) ts',zipWith (:>:) is scs')
     else
       let scs' = map (quantify gs) ts'
        in return (zipWith3 zipVECore is (applyTillNoChange s' es) ts',zipWith (:>:) is scs')

zipVECore :: Id -> C.CoreExpr -> Type -> C.CoreExprDef
zipVECore id e t = C.ExprDef (MkVar { varName = id, varType = TScheme [] t }) e

-- TIExpl --

tiExpl :: [Assumption] -> Expl -> TI (C.CoreExprDef,Type)
tiExpl as (id,sc,alt) =
  do t <- freshInstance sc
     (e,p') <-  tiAlt as (id,alt) t
     s     <- getSubst
     let p = apply s p'
     let ps  = apply s p
         t'  = apply s t
         fs  = tv (apply s as)
         gs  = (tv t' ++ seq ps []) \\ fs
         sc' = quantify gs t'
         t'' = apply s ps
     if sc /= sc' then error $ "signature too general for \n" ++ (show sc) ++ "\n" ++ (show sc')
      else return (C.ExprDef (MkVar { varName = id, varType = TScheme [] t'' }) (applyTillNoChange s e),t'')


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
