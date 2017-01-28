module CodeGen.Test where

import Pipeline.Compiler

import CoreAST.CoreExpr
import CoreAST.DataCon
import CoreAST.Kind
import CoreAST.Types
import CoreAST.Var
import CoreAST.TScheme

import Data.Maybe
import qualified Desugar.DExpr as DS

-- testTy :: [TyCon]
-- testTy = map DS.tCon ( (\((_,a),_,_) -> a) (fromJust (compilerSo "{ data Bool = True | False; data List a = Cons a Bool (List a) | Nil }")))

-- testCaseChar :: CoreExprDefs
-- testCaseChar = ( (\((a,_),_,_) -> a) (fromJust (compilerSo "{main = plus 2 4}")))



-- test = Lam (MkVar {varName = "y", varType = TScheme [] (TVar (Tyvar "0" Star) `fn` TVar (Tyvar "0" Star))}) ((Lam (MkVar { varName = "x", varType = TScheme [] (TVar (Tyvar "0" Star))})) (App (Var "y") (App (Var "y") (Var "x"))))

-- test2 = Let (ExprDef (MkVar {varName = "y", varType = TScheme [] (TVar (Tyvar "0" Star) `fn` TVar (Tyvar "0" Star))}) (Var "y"))
--            ((Lam (MkVar { varName = "x", varType = TScheme [] (TVar (Tyvar "0" Star))})) (App (Var "y") (Var "x")))

