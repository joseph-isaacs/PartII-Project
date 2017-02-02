{-# LANGUAGE OverloadedStrings #-}

module CoreAST.BuildInFunctions where

import CoreAST.Types
import CoreAST.Kind

data BuildInFunction = BuildInFunction { fnName :: String, fnType :: Type, fnClassName :: String }

plusFun :: BuildInFunction
plusFun = BuildInFunction { fnName = "plus", fnType = tInt `fn` (tInt `fn` tInt), fnClassName = "BuildIn/plus0" }

subFun = BuildInFunction { fnName = "sub", fnType = tInt `fn` (tInt `fn` tInt), fnClassName = "BuildIn/subtract0" }


modFun = BuildInFunction { fnName = "mod", fnType = tInt `fn` (tInt `fn` tInt), fnClassName = "BuildIn/mod0" }

multFun = BuildInFunction { fnName = "multiply", fnType = tInt `fn` (tInt `fn` tInt), fnClassName = "BuildIn/multiply0" }

negFun = BuildInFunction { fnName = "neg", fnType = tInt `fn` tInt, fnClassName = "BuildIn/neg" }

intEqFun = BuildInFunction { fnName = "intEq", fnType = tInt `fn` (tInt `fn` tBool), fnClassName = "BuildIn/intEq0" }

intLTFun = BuildInFunction { fnName = "intLT", fnType = tInt `fn` (tInt `fn` tBool), fnClassName = "BuildIn/intLT0" }

true  = BuildInFunction { fnName = "True", fnType = tBool, fnClassName = "BuildIn/True" }
false = BuildInFunction { fnName = "False", fnType = tBool, fnClassName = "BuildIn/False" }

tva :: Type
tva = starTVar "a"

tvb :: Type
tvb = starTVar "b"

unitFun = BuildInFunction { fnName = "unit", fnType = tUnit, fnClassName = "BuildIn/Unit" }

decFun = BuildInFunction { fnName = "dec", fnType = tInt `fn` tInt, fnClassName = "BuildIn/dec" }

returnFun = BuildInFunction { fnName = "return", fnType = tva  `fn` io tva, fnClassName = "BuildIn/Return" }

bindFun = BuildInFunction { fnName = "bind", fnType = io tva `fn` ((tva `fn` io tvb) `fn` io tvb), fnClassName = "BuildIn/Bind0" }

intToChar = BuildInFunction { fnName = "toChar", fnType = tInt `fn` tChar, fnClassName = "BuildIn/intToChar" }

putCharFun = BuildInFunction { fnName = "putChar", fnType = tChar `fn` io tUnit, fnClassName = "BuildIn/putChar" }

putIntFun = BuildInFunction { fnName = "putInt", fnType = tInt `fn` io tUnit, fnClassName = "BuildIn/putInt" }

putNewLineFun = BuildInFunction { fnName = "putNewLine", fnType = io tUnit, fnClassName = "BuildIn/putNewLine" }

getCharFun = BuildInFunction { fnName = "getChar", fnType = io tChar, fnClassName = "BuildIn/getChar" }

seqFun = BuildInFunction { fnName = "seq", fnType = tva `fn` (tvb `fn` tvb), fnClassName = "BuildIn/Seq0" }

buildIn = [plusFun,returnFun,bindFun,putCharFun,getCharFun,unitFun,putNewLineFun,intToChar,putIntFun, negFun,multFun, seqFun,subFun, decFun, modFun, intEqFun, intLTFun, true, false ]
