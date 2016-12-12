module Infer.BuildInTypes where

import CoreAST.Kind
import CoreAST.Types

import Desugar.DDataDecl

import Infer.Assumption

intType, charType :: DataType
intType  = buildType tInt []
charType = buildType tChar []
unitType = buildType tUnit []

types :: [DataType]
types = [intType, charType, unitType]

buildType :: Type -> [Assumption] -> DataType
buildType (TCon (Tycon n k)) as = DT { dName = n, dKind = k, dConstrs = as }
buildType t _ = error $ "Cannot build a type which is not a Tycon, " ++ show t ++ "cannot be built."
