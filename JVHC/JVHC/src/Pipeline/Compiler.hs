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

import Data.Text (Text)
import Data.String (fromString)
import Data.Maybe (fromJust)
import Data.Char (isUpper)
import qualified Data.Map as M

import Control.Monad



lexAndparse :: String -> Body
lexAndparse = jvhcParse . alexScanTokens

desugar :: Monad m => Body -> m (BindGroup, [DataType])
desugar = dTopDecls

tVr = TVar $ Tyvar "_t_" Star


buildInMap :: PreDefFunctionMap
buildInMap = map (\bi -> (fromString $ fnName bi, (fromString $ fnClassName bi,False))) buildIn

mkCore :: Monad m => m (BindGroup, [DataType]) ->
                     m ((CoreExprDefs,[DataType]),M.Map Id [(CoreExpr,Type)],[Assumption])
mkCore bgdt =
  do (bg,des) <- bgdt
     let sdt = splitDataType $ des
         ass = snd sdt ++ buildInAssumptions
         (x1,x2) = tiProgram ass [bg]
         (ict, ass')  = x1
          -- Adds the other top level function to the fixer
         topLevelVars =
            map (\(ExprDef (MkVar { varName = i, varType = TScheme [] t }) _) -> (Var i,(t, tv t))) ict
         ict' =   map (\v@(ExprDef (MkVar { varName = i }) _) ->
            runFXR (frxExprDef [] [] (map mkData (M.findWithDefault [] i x2) ++ topLevelVars) v )) ict
     return ((optimize ict',des),x2,ass')

optimize :: CoreExprDefs -> CoreExprDefs
optimize = inlineN 2000


-- This create the correct free type variables for Data constructors which were logged in the TI
mkData :: (CoreExpr,Type) -> (CoreExpr,(Type,[Tyvar]))
mkData (Var x@(l:_),t) = (Var x,(t, tvs))
  where tvs = if isUpper l then tv t else []

mkData (a,b) = (a,(b,[]))

jvmPath :: Text
jvmPath = "/Users/joeisaacs/Dropbox/git/JVMTesting/out/production/JVMTesting/"

codeGen :: (CoreExprDefs,[TyCon]) -> [(Text,ClassFile)]
codeGen (exprs,tyCons) =  (snd $ runCG buildInMapping (cgEnv exprs)) ++ concatMap mkDataType tyCons
  where buildInMapping = buildInMap ++ (concatMap buildInType tyCons)

buildInType :: TyCon -> PreDefFunctionMap
buildInType tyCon = map buildMap ctrs
  where ctrs = constrs tyCon
        buildMap c = (name,(newName,True))
          where isNull = null $ DC.fields c
                name = fromString $ dName c
                newName = if isNull then name else (name `mappend` "0")

writeCodeFiles :: [(Text,ClassFile)] -> IO ()
writeCodeFiles = writeFiles jvmPath

compilerSo :: Monad m => String -> m ((CoreExprDefs,[DataType]),M.Map Id [(CoreExpr,Type)], [Assumption])
compilerSo = mkCore . desugar . lexAndparse

compiler :: String -> IO ()
compiler = writeCodeFiles . codeGen . (\((c,d),_,_) -> (c,map tCon d)) . fromJust . compilerSo

compileFromSourceFile :: FilePath -> IO ()
compileFromSourceFile fp =
  do fileContent <- readFile fp
     compiler fileContent

compileSoFar fp =
  do fileContent <- readFile fp
     compilerSo fileContent
