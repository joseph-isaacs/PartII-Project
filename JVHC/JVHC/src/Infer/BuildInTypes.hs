{-# LANGUAGE OverloadedStrings #-}

module Infer.BuildInTypes where

import Data.Text(Text)

import CoreAST.Kind
import CoreAST.Types
import CoreAST.DataCon
import CoreAST.BuildInFunctions
import CoreAST.TScheme


import Desugar.DExpr

import Infer.Scheme
import Infer.Subst
import Infer.Assumption

intType, charType, unitType, ioType, boolType :: DataType
intType  = buildType tInt    []
charType = buildType tChar   []
unitType = buildType tUnit   []
ioType   = buildType tIO     []
boolType = buildType tBool   [MkDataCon "True" "Bool" [] (TScheme [] tBool), MkDataCon "False" "Bool" [] (TScheme [] tBool)]

dataTypeNameMap :: [(Text,Text)]
dataTypeNameMap = [("True","BuildIn/True"),("False","BuildIn/False")]

types :: [DataType]
types = [intType, charType, unitType, ioType,boolType]

buildType :: Type -> [DataCon] -> DataType
buildType (TCon tyc@(Tycon n k)) dts = DT { dTName = n, dKind = k, dConstrs = ass, tCon = MkTyCon tyc dts}
  where ass = map (\x -> let TScheme [] t = conType x in dName x :>: quantify (tv t) t) dts
buildType t _ = error $ "Cannot build a type which is not a Tycon, " ++ show t ++ "cannot be built."
