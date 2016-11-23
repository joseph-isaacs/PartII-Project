module Pipeline.Compiler where

import CoreAST.CoreExpr
import CoreAST.Types

import Parsing.ParsingAST(Body)
import Parsing.Parser(jvhcParse)
import Parsing.Lexer(alexScanTokens,LToken)
import Desugar.DTopDecls(dTopDecls,splitDataType)
import Desugar.DDataDecl(DataType)
import Desugar.DExpr
import Infer.Id

import Infer.TIProgram

import Infer.CoreExprFixer

import Control.Monad


lexAndparse :: String -> Body
lexAndparse = jvhcParse . alexScanTokens

desugar :: Monad m => Body -> m (BindGroup, [DataType])
desugar = dTopDecls

mkCore :: Monad m => m (BindGroup, [DataType]) -> m (CoreExprDefs,[DataType])
mkCore bgdt =
  do des <- bgdt
     let bg  = fst des
         sdt = splitDataType $ snd des
         ass = snd sdt
         (ict, ass') =  tiProgram ass [bg]
         idCoreExpr = map (\(v,c) -> (v,addLambda [] c)) ict
     return (idCoreExpr,snd des)

compilerSo :: Monad m => String -> m (CoreExprDefs,[DataType])
compilerSo = mkCore . desugar . lexAndparse
