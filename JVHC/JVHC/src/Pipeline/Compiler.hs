module Pipeline.Compiler where

import CoreAST.Types
import CoreAST.Kind
import CoreAST.TScheme
import CoreAST.Var
import CoreAST.CoreExpr

import Parsing.ParsingAST(Body)
import Parsing.Parser(jvhcParse)
import Parsing.Lexer(alexScanTokens,LToken)
import Desugar.DTopDecls(dTopDecls,splitDataType)
import Desugar.DDataDecl(DataType)
import Desugar.DExpr (BindGroup)

import Infer.Id
import Infer.Scheme
import Infer.Subst
import Infer.Assumption

import Infer.TIProgram

import Infer.TyAppFixer
import Infer.TyFixerMonad

import Control.Monad

import Data.Char (isUpper)

import qualified Data.Map as M

lexAndparse :: String -> Body
lexAndparse = jvhcParse . alexScanTokens

desugar :: Monad m => Body -> m (BindGroup, [DataType])
desugar = dTopDecls

tVr = TVar $ Tyvar "_t_" Star

mkCore :: Monad m => m (BindGroup, [DataType]) ->
                     m ((CoreExprDefs,[DataType]),M.Map Id [(CoreExpr,Type)],[Assumption])
mkCore bgdt =
  do (bg,des) <- bgdt
     let sdt = splitDataType $ des
         ass = snd sdt
         (x1,x2) = tiProgram ass [bg]
         (ict, ass')  = x1
          -- Adds the other top level function to the fixer
         topLevelVars =
            map (\(ExprDef (MkVar { varName = i, varType = TScheme [] t }) _) -> (Var i,(t, tv t))) ict
         ict' =   map (\v@(ExprDef (MkVar { varName = i }) _) ->
            runFXR (frxExprDef [] [] (map mkData (M.findWithDefault [] i x2) ++ topLevelVars) v )) ict
     return ((ict',des),x2,ass')

-- This create the correct free type variables for Data constructors which were logged in the TI
mkData :: (CoreExpr,Type) -> (CoreExpr,(Type,[Tyvar]))
mkData (Var x@(l:_),t) = (Var x,(t, tvs))
  where tvs = if isUpper l then tv t else []

mkData (a,b) = (a,(b,[]))

compilerSo :: Monad m => String -> m ((CoreExprDefs,[DataType]),M.Map Id [(CoreExpr,Type)], [Assumption])
compilerSo = mkCore . desugar . lexAndparse


