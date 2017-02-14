{-# LANGUAGE OverloadedStrings #-}

module CodeGen.CGThunkCounter where

import Data.Monoid((<>))

import CodeGen.JInterfaces
import CodeGen.CGHelper
import CodeGen.JTypes
import CodeGen.JVMVersion

import Codec.JVM

import CodeGen.CGMonad

debugThunkMadeInvoke :: CG Code
debugThunkMadeInvoke =
  do isCount <- isCountThunks
     return $ if isCount then c else mempty
  where
    c = invokestatic (mkMethodRef "BuildIn/ThunkCounter" "thunkMade" [] void)

debugThunkUnmadeInvoke :: CG Code
debugThunkUnmadeInvoke =
  do isCount <- isCountThunks
     return $ if isCount then c else mempty
  where
    c = invokestatic (mkMethodRef "BuildIn/ThunkCounter" "thunkDec" [] void)

wrapWithTimer :: Code -> CG Code
wrapWithTimer c =
  do isCount <- isCountThunks
     return $ if isCount then wrapped else c
  where wrapped = getSystemTime
               <> gconv jlong jint
               <> c
               <> swap jint supplierInterfaceType
               <> getSystemTime
               <> gconv jlong jint
               <> isub
               <> invokestatic setThunkTime
        setThunkTime = (mkMethodRef "BuildIn/ThunkCounter" "setThunkTime" [jint] void)

printThunkCode :: CG Code
printThunkCode =
  do isCount <- isCountThunks
     return $ if isCount then printCode else mempty
  where printCode =  invokestatic (mkMethodRef "BuildIn/ThunkCounter" "printThunkCount" [] void)
