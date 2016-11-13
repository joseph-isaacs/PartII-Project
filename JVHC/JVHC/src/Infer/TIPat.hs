module Infer.TIPat where

import CoreAST.Types
import CoreAST.Kind
import Parsing.ParsingAST(Literal(LitInt,LitChar), Literal)
import Infer.TIM
import Infer.Assumption
import Infer.TILit
import Infer.Scheme
import Infer.Subst
import Infer.Unify
import Infer.Id

data Pat = PVar Id
         | PLit Literal
         | PCon Assumption [Pat]
    deriving (Show)

tiPat :: Pat -> TI([Assumption], Type)
tiPat (PVar id) = do v <- newTVar Star
                     return ([id :>: toScheme v],v)
tiPat (PLit l)  = do t <- tiLit l
                     return ([], t)
tiPat (PCon (i :>: sc) ps)
  = do (as,ts) <- tiPats ps
       t' <- newTVar Star
       t  <- freshInstance sc
       unify t (foldr fn t' ts)
       return (as,t')

tiPats :: [Pat] -> TI([Assumption], [Type])
tiPats pats = do patAS <- mapM tiPat pats
                 let ass = [a|(as,_) <- patAS, a <- as]
                     ts =  [t|(_,t) <- patAS]
                 return (ass,ts)

