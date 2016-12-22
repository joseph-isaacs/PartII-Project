module Infer.BuildInTypes where

import CoreAST.Kind
import CoreAST.Types

import CoreAST.BuildInFunctions


import Desugar.DExpr

import Infer.Assumption

intType, charType :: DataType
intType  = buildType tInt  []
charType = buildType tChar []
unitType = buildType tUnit []
ioType   = buildType tIO   []

types :: [DataType]
types = [intType, charType, unitType, ioType]

buildType :: Type -> [Assumption] -> DataType
buildType (TCon (Tycon n k)) as = DT { dTName = n, dKind = k, dConstrs = as }
buildType t _ = error $ "Cannot build a type which is not a Tycon, " ++ show t ++ "cannot be built."
