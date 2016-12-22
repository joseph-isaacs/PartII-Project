module CodeGen.TypeToJType where

import CoreAST.Types

import CodeGen.JTypes

import Codec.JVM.Types(jobject,obj,FieldType)

import Data.String(fromString,IsString)

toJType :: Type -> FieldType

toJType (TVar _) = jobject

toJType t@(TCon (Tycon i _))
   | t  == tChar = jCharType
   | t == tInt   = jIntType
   | t == tArrow = jobject
   | otherwise   = jobject

toJType (TAp t _) = toJType t

toJType t@(TGen _) = error $ "Cannot understand TGen " ++ show t
