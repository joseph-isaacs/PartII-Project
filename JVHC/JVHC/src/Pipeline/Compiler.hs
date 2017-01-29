{-# LANGUAGE OverloadedStrings #-}

module Pipeline.Compiler where

import CoreAST.Types
import CoreAST.Kind
import CoreAST.TScheme
import CoreAST.Var
import CoreAST.CoreExpr
import CoreAST.BuildInFunctions
import CoreAST.DataCon as DC

import Parsing.ParsingAST(Body)
import Parsing.Parser(jvhcParse)
import Parsing.Lexer(alexScanTokens,LToken)
import Desugar.DTopDecls(dTopDecls,splitDataType)
import Desugar.DExpr (BindGroup,DataType(..))

import Infer.Id
import Infer.Scheme
import Infer.Subst
import Infer.Assumption
import Infer.TIProgram
import Infer.TyAppFixer
import Infer.TyFixerMonad
import Infer.BuildInFunctionTypes

import Opt.InlineMonad
import Opt.Inlining

import CodeGen.CGMonad
import CodeGen.CGMain
import CodeGen.CGCustomDataType
import CodeGen.CGFileWriter

import Codec.JVM

import Data.Text (Text,unpack,pack)
import Data.String (fromString)
import Data.Maybe (fromJust)
import Data.Char (isUpper)
import qualified Data.Map as M

import Control.Monad

type TopLevelVars = (CoreExpr,(Type,[Tyvar]))

lexAndparse :: String -> Body
lexAndparse = jvhcParse . alexScanTokens

desugar :: Monad m => Body -> m (BindGroup, [DataType])
desugar = dTopDecls

tVr = TVar $ Tyvar "_t_" Star


buildInMap :: PreDefFunctionMap
buildInMap = map (\bi -> (fromString $ fnName bi, (fromString $ fnClassName bi,False))) buildIn

typeInference :: Monad m => m (BindGroup, [DataType]) -> m ((CoreExprDefs,[Assumption]),M.Map Id [(CoreExpr,Type)])
typeInference bgdt =
  do (bg,des) <- bgdt
     let sdt = splitDataType $ des
         ass = snd sdt ++ buildInAssumptions
     return $ tiProgram ass [bg]

getTopLevelVars :: CoreExprDefs -> [TopLevelVars]
getTopLevelVars = map (\(ExprDef (MkVar { varName = i, varType = TScheme [] t }) _) -> (Var i,(t, tv t)))

toSystemFExpr :: M.Map Id [(CoreExpr,Type)] -> [TopLevelVars] -> ExprDef Binder -> CoreExprDef
toSystemFExpr idCoreTMap tlv expr = runFXR (frxExprDef [] [] (map mkData (M.findWithDefault [] i idCoreTMap) ++ tlv) expr)
  where (ExprDef (MkVar { varName = i }) _) = expr

mkCore :: Monad m => m (BindGroup, [DataType]) ->
                     m ((CoreExprDefs,[DataType]),M.Map Id [(CoreExpr,Type)],[Assumption])
mkCore bgdt =
  do (bg,des) <- bgdt
     let sdt = splitDataType $ des
         ass = snd sdt ++ buildInAssumptions
         (x1,x2) = tiProgram ass [bg]
         (ict, ass')  = x1
          -- Adds the other top level function to the fixer
         topLevelVars = getTopLevelVars ict
         ict' =   map (toSystemFExpr x2 topLevelVars) ict
     return ((ict',des),x2,ass')

data OptimizeParams = OP { inlineTimes :: Int }

optimize :: OptimizeParams -> CoreExprDefs -> CoreExprDefs
optimize op = inlineN (inlineTimes op)


-- This create the correct free type variables for Data constructors which were logged in the TI
mkData :: (CoreExpr,Type) -> TopLevelVars
mkData (Var x@(l:_),t) = (Var x,(t, tvs))
  where tvs = if isUpper l then tv t else []

mkData (a,b) = (a,(b,[]))

jvmPath :: Text
jvmPath = "/Users/joeisaacs/Dropbox/git/JVMTesting/out/production/JVMTesting/"

codeGen :: Bool -> (CoreExprDefs,[TyCon]) -> [(Text,ClassFile)]
codeGen d (exprs,tyCons) =  (snd $ runCG (CGR { funMap = buildInMapping, debug = d }) (cgEnv exprs)) ++ concatMap mkDataType tyCons
  where buildInMapping = buildInMap ++ (concatMap buildInType tyCons)

buildInType :: TyCon -> PreDefFunctionMap
buildInType tyCon = map buildMap ctrs
  where ctrs = constrs tyCon
        buildMap c = (name,(newName,True))
          where isNull = null $ DC.fields c
                name = fromString $ dName c
                newName = if isNull then name else (name `mappend` "0")

writeCodeFiles :: Text -> [(Text,ClassFile)] -> IO ()
writeCodeFiles outPutDir = writeFiles outPutDir

compilerSo :: Monad m => String -> m ((CoreExprDefs,[DataType]),M.Map Id [(CoreExpr,Type)], [Assumption])
compilerSo = mkCore  . desugar . lexAndparse

compilerPreCodeGen :: OptimizeParams -> String -> (CoreExprDefs,[TyCon])
compilerPreCodeGen op = (\((c,d),_,_) -> (optimize op c,map tCon d)) . fromJust . compilerSo

compiler :: Bool -> OptimizeParams -> Text -> String -> IO ()
compiler d op output = (writeCodeFiles output) . (codeGen d) . (compilerPreCodeGen op)

compileFromSourceFile :: Bool -> OptimizeParams -> FilePath -> FilePath -> IO ()
compileFromSourceFile d op outputPath fp =
  do fileContent <- readFile fp
     (compiler d op) (pack outputPath) fileContent

compileSoFar op fp =
  do fileContent <- readFile fp
     compilerSo  fileContent

noOpt = (OP {inlineTimes = 0})

normalOpt = (OP {inlineTimes = 1})
