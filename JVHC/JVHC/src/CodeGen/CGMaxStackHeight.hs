{-# LANGUAGE OverloadedStrings #-}

module CodeGen.CGMaxStackHeight where

import Data.Monoid((<>))

import CodeGen.JInterfaces
import CodeGen.CGHelper
import CodeGen.JTypes
import CodeGen.JVMVersion

import Codec.JVM

import CodeGen.CGMonad

stackHeightCounterName = "BuildIn/StackHeightCounter"

debugMaxStack :: CG Code
debugMaxStack =
  do isCount <- isCountMaxStackHeight
     return $ if isCount then c else mempty
  where
    c = invokestatic (mkMethodRef stackHeightCounterName "checkStackHeight" [] void)


printMaxStackCode :: CG Code
printMaxStackCode =
  do isCount <- isCountMaxStackHeight
     return $ if isCount then printCode else mempty
  where printCode =  invokestatic (mkMethodRef stackHeightCounterName "printStackMaxHeight" [] void)
