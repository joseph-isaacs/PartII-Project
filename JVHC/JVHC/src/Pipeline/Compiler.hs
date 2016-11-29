module Pipeline.Compiler where

import CoreAST.CoreExpr
import CoreAST.Types
import CoreAST.Var

import Parsing.ParsingAST(Body)
import Parsing.Parser(jvhcParse)
import Parsing.Lexer(alexScanTokens,LToken)
import Desugar.DTopDecls(dTopDecls,splitDataType)
import Desugar.DDataDecl(DataType)
import Desugar.DExpr (BindGroup)
import Infer.Id

import Infer.TIProgram

import Infer.CoreExprFixer

import Control.Monad

import qualified Data.Map as M

lexAndparse :: String -> Body
lexAndparse = jvhcParse . alexScanTokens

desugar :: Monad m => Body -> m (BindGroup, [DataType])
desugar = dTopDecls

mkCore :: Monad m => m (BindGroup, [DataType]) ->
                     m ((CoreExprDefs,[DataType]),M.Map Id [(CoreExpr,Type)])
mkCore bgdt =
  do des <- bgdt
     let bg  = fst des
         sdt = splitDataType $ snd des
         ass = snd sdt
         (x1,x2) = tiProgram ass [bg]
         (ict, ass')  = x1
         idCoreExpr   = map (addLambda []) ict
         topLevelVars = map (\((MkVar { varName = i, varType = t }),_) -> (Var i, t)) idCoreExpr
     idCoreExpr'  <- mapM (\v@((MkVar { varName = i }),_) ->
                          addTvApp [] (M.findWithDefault [] i x2 ++ topLevelVars) v ) idCoreExpr
     return ((idCoreExpr',snd des),x2)

compilerSo :: Monad m => String -> m ((CoreExprDefs,[DataType]),M.Map Id [(CoreExpr,Type)])
compilerSo = mkCore . desugar . lexAndparse


