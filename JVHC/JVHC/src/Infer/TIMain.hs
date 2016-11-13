module Infer.TIMain where

import Infer.TIM
import Infer.Id
import Infer.TILit
import Infer.TIPat
import Infer.Assumption
import CoreAST.Types
import CoreAST.Kind
import Infer.Subst
import Infer.Scheme
import Infer.Unify

import Parsing.ParsingAST(Literal(LitInt,LitChar), Literal)
import Data.List((\\),union)

data Expr = Var Id
          | Lit Literal
          | Const Assumption
          | Ap Expr Expr
          | Lam Alt
          | Let BindGroup Expr
          | Case Expr [(Pat,Expr)]
     deriving Show

tiExpr :: Infer Expr Type
tiExpr as (Var i) = do sc <- find i as
                       t  <- freshInstance sc
                       return t

tiExpr _  (Lit l) = tiLit l

tiExpr as (Const (_ :>: sc)) = freshInstance sc

tiExpr as (Ap e1 e2)     = do lt <- tiExpr as e1
                              rt <- tiExpr as e2
                              t  <- newTVar Star
                              unify (rt `fn` t) lt
                              return t

tiExpr as (Let bg e)    = do as' <- tiBindGroup as bg
                             tiExpr     (as' ++ as)   e

tiExpr as (Lam alt)    = tiAlt as alt

tiExpr as (Case e cases)
  = do t <- tiExpr as e
       v <- newTVar Star
       let tiCase (pat,body) =
             do (as',t') <- tiPat pat
                unify t t'
                retT <- tiExpr (as' ++ as) body
                unify v retT
                return ()
       mapM_ tiCase cases
       return v



type Alt = ([Pat], Expr)

tiAlt :: Infer Alt Type
tiAlt as (pats, e) =
  do (as', ts) <- tiPats pats
     t         <- tiExpr (as' ++ as) e
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
