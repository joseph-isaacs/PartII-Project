module Pipeline.Compiler where

import CoreAST.CoreExpr
import CoreAST.Types
import CoreAST.Kind
import CoreAST.Var

import Parsing.ParsingAST(Body)
import Parsing.Parser(jvhcParse)
import Parsing.Lexer(alexScanTokens,LToken)
import Desugar.DTopDecls(dTopDecls,splitDataType)
import Desugar.DDataDecl(DataType)
import Desugar.DExpr (BindGroup)

import Infer.Id
import Infer.Scheme
import Infer.Assumption

import Infer.TIProgram

import Infer.TyAppFixer

import Control.Monad

import qualified Data.Map as M

lexAndparse :: String -> Body
lexAndparse = jvhcParse . alexScanTokens

desugar :: Monad m => Body -> m (BindGroup, [DataType])
desugar = dTopDecls

tVr = TVar $ Tyvar "_t_" Star

mkCore :: Monad m => m (BindGroup, [DataType]) ->
                     m ((CoreExprDefs,[DataType]),M.Map Id [(CoreExpr,Type)],[Assumption])
mkCore bgdt =
  do des <- bgdt
     let bg  = fst des
         sdt = splitDataType $ snd des
         ass = snd sdt
         (x1,x2) = tiProgram ass [bg]
         (ict, ass')  = x1
         topLevelVars =
           map (\(ExprDef (MkVar { varName = i, varType = TScheme [] t }) _) -> (Var i, t)) ict
         --tlV' = (Var "Cons", tVr `fn` ((TAp tList tVr) `fn` (TAp tList tVr))) : topLevelVars
         ict' =   map (\v@(ExprDef (MkVar { varName = i }) _) ->
           runFXR (frxExprDef [] [] (M.findWithDefault [] i x2 ++ topLevelVars) v )) ict
     return ((ict',snd des),x2,ass')



compilerSo :: Monad m => String -> m ((CoreExprDefs,[DataType]),M.Map Id [(CoreExpr,Type)], [Assumption])
compilerSo = mkCore . desugar . lexAndparse


