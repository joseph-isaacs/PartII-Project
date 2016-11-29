module Infer.TIPat where

import CoreAST.Types
import CoreAST.Kind
import CoreAST.CoreExpr
import CoreAST.DataCon
import CoreAST.Var
import CoreAST.Literal

import Desugar.DExpr

import Infer.TIM
import Infer.Assumption
import Infer.TILit
import Infer.Scheme
import Infer.Subst
import Infer.Unify
import Infer.Id

tiCaseAlt :: Pat -> TI(AltCon, [Var], [Assumption], Type)
tiCaseAlt (PVar id) =
  do v <- newTVar Star
     return (DEFAULT, [MkVar { varName = id, varType = v}], [id :>: toScheme v],v)

tiCaseAlt (PLit l) =
  do t <- tiLit l
     return (LitAlt l, [], [], t)

tiCaseAlt (PCon (i :>: sc@(Scheme _ t)) pats) =
  do (binders, as, ts) <- tiCaseAlts pats
     t'                <- newTVar Star
     t                 <- freshInstance sc
     unify t (foldr fn t' ts)
     return (DataAlt $ MkDataCon { dName = i, conType = t },
             binders, as, t')

tiCaseAlts :: [Pat] -> TI([Var],[Assumption],[Type])
tiCaseAlts pats =
  do caseAlts' <- mapM tiCaseAlt pats
     let var = [v | (_,vs,_,_) <- caseAlts', v <- vs]
         ass = [a | (_,_,as,_) <- caseAlts', a <- as]
         ts  = [t | (_,_,_,t)  <- caseAlts']
     return (var, ass, ts)

tiPat :: Pat -> TI([Assumption], Type)
tiPat p =
  do (altC,vars,as,t) <- tiCaseAlt p
     return (as,t)


tiPats :: [Pat] -> TI([Assumption], [Type])
tiPats pats = do (_,as,ts) <- tiCaseAlts pats
                 return (as,ts)

--tiPat (PCon (i :>: sc) ps)
--  = do (as,ts) <- tiPats ps
--       t' <- newTVar Star
--       t  <- freshInstance sc
--      unify t (foldr fn t' ts)
--          return (as,t')
