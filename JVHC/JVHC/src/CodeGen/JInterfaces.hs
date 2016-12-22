{-# LANGUAGE OverloadedStrings #-}

module CodeGen.JInterfaces where

import CodeGen.JTypes
import Data.Text

import Codec.JVM (obj,invokeinterface, mkMethodRef, jobject, ret,Code)

functionInterface :: Text
functionInterface = "java/util/function/Function"

functionApply :: Text
functionApply = "apply"

invokeFunction :: Code
invokeFunction = invokeinterface (mkMethodRef functionInterface functionApply [jobject] (ret jobject))

thunkName :: Text
thunkName = "BuildIn/Thunk"

thunkGetName :: Text
thunkGetName = supplierName

supplierName :: Text
supplierName = "get"

supplierInterfaceName :: Text
supplierInterfaceName = "java/util/function/Supplier"

supplierInterfaceType :: JType
supplierInterfaceType = obj supplierInterfaceName

invokeSupplier :: Code
invokeSupplier = invokeinterface (mkMethodRef supplierInterfaceName supplierName [] (ret jobject))

thunk :: JType
thunk = obj thunkName
