module CoreAST.RenameCoreExpr where

import CoreAST.CoreExpr
import CoreAST.Var


import CoreAST.RenameMonad


runRenameExpr :: Int -> CoreExpr -> (CoreExpr,Int)
runRenameExpr s expr =
  runRN s (renameExpr expr)

renameExpr :: CoreExpr -> RN CoreExpr
renameExpr (Var i) =
  do i' <- updateId i
     return $ Var i'

renameExpr l@(Lit _) = return l

renameExpr (App e1 e2) =
  do e1' <- renameExpr e1
     e2' <- renameExpr e2
     return $ App e1' e2'

renameExpr (Lam var e) =
  do var' <- renameVar var
     e'   <- renameExpr e
     return $ (Lam var' e')

renameExpr (Let (ExprDef var a) e) =
  do var' <- renameVar var
     a' <- renameExpr a
     e' <- renameExpr e
     return (Let (ExprDef var' a') e')

renameExpr (Case e t alts) =
  do e' <- renameExpr e
     alts' <- mapM renameAlt alts
     return $ Case e' t alts'

renameExpr t@(Type _) = return t


renameAlt :: Alt Binder -> RN (Alt Binder)
renameAlt (dc, vars, e) =
  do vars' <- mapM renameVar vars
     e'    <- renameExpr e
     return (dc, vars', e')


renameVar :: Var -> RN Var
renameVar (MkVar { varName = n, varType = t}) =
  do n' <- addBinding n
     return $ (MkVar {varName = n', varType = t})

renameVar x = return x


