module Opt.Inlining where

import CoreAST.CoreExpr
import CoreAST.Types
import CoreAST.Kind
import CoreAST.Literal
import CoreAST.Var
import CoreAST.TScheme
import CoreAST.RenameCoreExpr

import Opt.InlineMonad

import Infer.Id
import Infer.Subst
import Infer.CoreExprSubst
import Data.Int
import Data.Monoid hiding(Alt)
import Data.List(find)


inlineN :: Int -> CoreExprDefs -> CoreExprDefs
inlineN n defs = iterate inlineOnce defs !! n

inlineOnce :: CoreExprDefs -> CoreExprDefs
inlineOnce defs = runIL defs opt
  where opt = mapM inl defs
        inl (ExprDef b@(MkVar {varName = n}) e) =
          do addInlinedId n
             e' <- inline e
             setInlined []
             return (ExprDef b e')
        inl x = return x



inline :: CoreExpr -> IL CoreExpr
inline (App (Lam (MkTVar {tvarName = TVar t1}) e) (Type t2)) =
  do e' <- inline e
     return $ applyType (t1 +-> t2) e'

inline ap@(App (Lam b@(MkVar {varName = n})  e) a) =
  do addInlinedId n
     inline (Let (ExprDef b a) e)

inline (App (Var id) a) =
  do wasInl <- wasInlined id
     idE    <- lookupExpr id
     i      <- getNewInt
     let isRec = case idE of {Just ide -> countVars id ide > 0; Nothing -> False}
     case wasInl || idE == Nothing || isRec of
       True  -> do {a' <- inline a; return (App (Var id) a')}
       False -> let Just (ide,i') = fmap (runRenameExpr i) idE
                 in do addInlinedId id
                       setNewInt i'
                       inline (App ide a)

inline (App e1 e2) =
  do before <- getInlined
     e1' <- inline e1
     e1Inl <- getInlined
     setInlined before
     e2' <- inline e2
     e2Inl <- getInlined
     setInlined (e1Inl ++ e2Inl)
     if e1' /= e1 || e2' /= e2 then inline (App e1' e2') else (return $ App e1' e2')

inline (Lam b e)   =
  do e' <- inline e
     return $ Lam b e'

inline l@(Let (ExprDef v@(MkVar { varName = n }) a) e) =
  do
     let numVars = countVars n a + countVars n e
     l' <- (if numVars < 4
               then do addInlinedId n
                       a' <- inline a
                       inline (applyExpr (n,a') e)
               else do a' <- inline a
                       return (Let (ExprDef v a') e))
     if l' == l then return l' else inline l'

inline (Case e t alts) =
  do inl <- getInlined
     e'  <- inline e
     setInlined inl
     inlineCase e' t alts

inline v@(Var _) = return v

inline l@(Lit _) = return l

inline t@(Type _)  = return t

inline x         = fail ("not valid core: " ++ show x)

inlineCase :: CoreExpr -> Type -> [Alt Binder] -> IL CoreExpr
inlineCase e@(Lit l) t alts = maybe (return (Case e t alts)) (\x -> inline $ embedAlt l x) branch
  where branch = find (findBranch l . \(a,_,_) -> a) alts

inlineCase e         t alts =
  do alts' <- mapM inlineAlt alts
     return $ Case e t alts'


findBranch :: Literal -> AltCon -> Bool
findBranch lit (LitAlt l) = lit == l
findBranch _   DEFAULT    = True
findBranch _   _          = False

embedAlt :: Literal -> Alt Binder -> CoreExpr
embedAlt l (LitAlt lit,_,e) | l == lit = e
embedAlt l (DEFAULT,[var],e)           = Let (ExprDef var (Lit l)) e
embedAlt l alt                         = error $ "could not embed " ++ show l ++ " in " ++ show alt

inlineAlt :: Alt Binder -> IL (Alt Binder)
inlineAlt (dc,vars,e) =
  do inl <- getInlined
     e' <- inline e
     setInlined inl
     return (dc,vars,e')


applyExpr :: (Id,CoreExpr) -> CoreExpr -> CoreExpr
applyExpr (v,e') e | Var v == e = e'
applyExpr s (App e1 e2) = (App (applyExpr s e1) (applyExpr s e2))
applyExpr s@(v,e') l@(Lam b e)
  | isId v b  = l
  | otherwise = Lam b (applyExpr s e)

applyExpr s@(v,e') l@(Let (ExprDef b a) e)
  | isId v b  = l
  | otherwise = Let (ExprDef b (applyExpr s a)) (applyExpr s e)

applyExpr s (Case e t alts) = Case (applyExpr s e) t (map (applyExprAlts s) alts)

applyExpr _ e = e

applyExprAlts :: (Id,CoreExpr) -> (Alt Binder) -> (Alt Binder)
applyExprAlts s@(id,_) (d,vars,e)
  | or (map (isId id) vars)  = (d,vars,e)
  | otherwise                = (d,vars,applyExpr s e)

isId :: Id -> Binder -> Bool
isId id (MkVar {varName = n}) = id == n
isId _  _                     = False

applyType :: Subst -> CoreExpr -> CoreExpr
applyType s (App e1 e2) = App (applyType s e1) (applyType s e2)
applyType s@[(t1,_)] l@(Lam tv@(MkTVar { tvarName = ty}) e2)
  | TVar t1 == ty   = l
  | otherwise = Lam tv (applyType s e2)

applyType s (Lam v e) =
  (Lam (apply s v) (applyType s e))

applyType s (Let (ExprDef b a) e) = Let (ExprDef (apply s b) (applyType s a)) (applyType s e)

applyType s (Case e t alts) = Case (applyType s e) (apply s t) (map (\(dc,bs,es) -> (dc,apply s bs,applyType s es)) alts)

applyType s e@(Type t) = Type (apply s t)

applyType t e = e

t0 = TVar $ Tyvar "0" Star
t1 = TVar $ Tyvar "1" Star

ty0 = Tyvar "0" Star

apppas = Lam (MkTVar {tvarName = t0}) (Lam (MkVar {varName = "x", varType =TScheme [] t0}) (App (App (App (Var "id") (Type t0)) (Type t1)) (Var "x")))

tInts = tInt

apps = (Lam (MkVar {varName = "numberN", varType = TScheme [] tInt}) (Case (Var "numberN") tInt [(LitAlt (LitInt 0),[],Lit (LitInt 0)),(DEFAULT,[MkVar {varName = "tt", varType = TScheme [] tInt}],App (App (Var "plus") (Var "numberN")) (App (Var "sumN") (App (Var "dec") (Var "numberN"))))]))

countVars :: Id -> CoreExpr -> Int
countVars v (Var x) | v == x = 1

countVars v (App e1 e2) = countVars v e1 + countVars v e2
countVars v (Lam (MkVar { varName = var })  e)
         | v /= var  = countVars v e
         | otherwise = 0

countVars v (Let (ExprDef (MkVar { varName = var }) a) e)
  | v /= var  = countVars v a + countVars v e
  | otherwise = 0

countVars v (Case e _ alts) = countVars v e + sum (map (countVarsAlt v) alts)

countVars _ _ = 0

countVarsAlt :: Id -> Alt Binder -> Int
countVarsAlt v (_, vars, e)
  | not (v `elem` (map varName vars))  = countVars v e
  | otherwise            = 0
