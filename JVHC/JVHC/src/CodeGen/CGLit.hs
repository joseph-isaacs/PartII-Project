{-# LANGUAGE OverloadedStrings #-}

module CodeGen.CGLit where

import CoreAST.Literal
import CoreAST.Types

import Data.Monoid((<>))
import Data.Char(ord)
import Data.Int(Int32)

import CodeGen.JTypes
import CodeGen.JInterfaces
import CodeGen.CGMonad
import CodeGen.CodeGen

import Codec.JVM


cgLit :: CodeGen Literal
cgLit (LitInt i) = return (mkLit intThunkType value intThunkConstructor jint
                          ,(supplierInterfaceType,1))
  where value = fromInteger i

cgLit (LitChar c) = return (mkLit charThunkType value charThunkConstructor jchar
                           ,(supplierInterfaceType ,1))
  where value = (fromIntegral . ord) c

cgLit (LitString _) = error "code gen for string not supported"

mkLit :: JType     ->  -- lit type
         Int32     ->  -- value
         MethodRef ->  -- lit constructor
         FieldType ->  -- primative type
         Code
mkLit t val con pt =
     new t
  <> dup t
  <> iconst pt val
  <> invokespecial con
  <> gconv jobject intThunkType

