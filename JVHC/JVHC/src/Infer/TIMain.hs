module Infer.TIMain where

import Infer.TIM
import Infer.Id
import Infer.TILit
import Infer.TIPat
import Infer.Assumption
import CoreAST.Types
import CoreAST.Kind
import CoreAST.Var
import Infer.Subst
import Infer.Scheme
import Infer.Unify

import qualified CoreAST.CoreExpr as C

import Parsing.ParsingAST(Literal(LitInt,LitChar), Literal)
import Data.List((\\),union)

data Expr = Var Id
          | Lit Literal
          | Const Assumption
          | Ap Expr Expr
          | Lam Id Expr
          | Let BindGroup Expr
          | Case Expr [(Pat,Expr)]
     deriving Show



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
     t  <- newTVar Star
     unify t1 (t2 `fn` t)
     return (C.App e1' e2',t)


tiExpr as (Let bg e) =
  do as'     <- tiBindGroup as bg
     (e', t) <- tiExpr (as' ++ as) e
     return (undefined, t)

tiExpr as (Lam p expr) =  -- Just changed the lam def such is can only have a single binding variable (Pat) hence conv to core will be easier.
  do (as', ts) <- tiPat (PVar p)
     (e',t)    <- tiExpr (as' ++ as) expr
     s <- getSubst
     tv <- newTVar Star
     return (C.Lam (MkTVar {tvarName = tv }) $ C.Lam (MkVar { varName = p, varType = tv }) e', ts `fn` t)

tiExpr as (Case e cases)
  = do (e',t) <- tiExpr as e
       v <- newTVar Star
       let tiCase (pat,body) =
             do (as',t') <- tiPat pat
                unify t t'
                (e'',retT) <- tiExpr (as' ++ as) body
                unify v retT
                return ()
       mapM_ tiCase cases
       return (undefined,v)



type Alt = ([Pat], Expr)

tiAlt :: Infer Alt Type
tiAlt as (pats, e) =
  do (as', ts) <- tiPats pats
     (e',t)    <- tiExpr (as' ++ as) e
     return (foldr fn t ts)

tiAlts :: [Assumption] -> [Alt] -> Type -> TI [Type]
tiAlts as alts t = do patType <- mapM (tiAlt as) alts
                      mapM (unify t) (patType)
                      s' <- getSubst
                      return $ map (apply s') patType

-- TIImpl --


restricted :: [Impl] -> Bool
restricted bs = any simple bs
  where simple (id, alts) = any (null.fst) alts


type Impl = (Id,[Alt])

tiImpls :: Infer [Impl] [Assumption]
tiImpls as bs =
  do ts <- mapM (\_ -> newTVar Star) bs
     let is    = map fst bs
         scs   = map toScheme ts
         as'   = zipWith (:>:) is scs ++ as
         altss = map snd bs
     pss <- sequence (zipWith (tiAlts as') altss ts)
     s <- getSubst
     let ps' = apply s (concat pss)
         ts' = apply s ts
         fs  = tv (apply s as)
         vss = map tv ts'
         gs  = (foldr1 union vss) \\ fs
     if restricted bs then
        let gs'  = gs \\ (tv ps')
            scs' = map (quantify gs') ts'
        in  return $ zipWith (:>:) is scs'
     else
       let scs' = map (quantify gs) ts'
       in return (zipWith (:>:) is scs')


-- TIExpl --

type Expl = (Id, Scheme, [Alt])

tiExpl :: [Assumption] -> Expl -> TI [Type]
tiExpl as (i,sc,alts) =
  do t  <- freshInstance sc
     ps <- tiAlts as alts t
     s  <- getSubst
     let t'  = apply s t
         fs  = tv (apply s as)
         gs  = (tv t' ++ seq ps []) \\ fs
         sc' = quantify gs t'
     if sc /= sc' then error "signature too general"
     else return $ map (apply s) ps


-- TIBind --

type BindGroup = ([Expl],[[Impl]])

tiBindGroup :: Infer BindGroup [Assumption]
tiBindGroup as (es,is) =
  do let as' = [v :>: sc | (v,sc,_) <- es]
     as'' <- tiSeq tiImpls (as' ++ as) is
     mapM (tiExpl (as'' ++ as' ++ as)) es
     return (as'' ++ as')

tiSeq :: Infer tiI [Assumption] -> Infer [tiI] [Assumption]
tiSeq _  _  [] = return []
tiSeq ti as (bs:bss) = do as'  <- ti as bs
                          as'' <- tiSeq ti (as' ++ as) bss
                          return (as'' ++ as')
