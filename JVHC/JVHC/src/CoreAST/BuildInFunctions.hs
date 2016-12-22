{-# OverloadedStrings #-}

module CoreAST.BuildInFunctions where

import CoreAST.Types
import CoreAST.Kind

data BuildInFunction = BuildInFunction { fnName :: String, fnType :: Type, fnClassName :: String }

plusFun :: BuildInFunction
plusFun = BuildInFunction { fnName = "plus", fnType = tInt `fn` (tInt `fn` tInt), fnClassName = "BuildIn/plus0" }

tva :: Type
tva = starTVar "a"

tvb :: Type
tvb = starTVar "b"

unitFun = BuildInFunction { fnName = "unit", fnType = tUnit, fnClassName = "BuildIn/Unit" }

returnFun = BuildInFunction { fnName = "return", fnType = tva  `fn` io tva, fnClassName = "BuildIn/Return" }

bindFun = BuildInFunction { fnName = "bind", fnType = io tva `fn` ((tva `fn` io tvb) `fn` io tvb), fnClassName = "BuildIn/Bind0" }

putCharFun = BuildInFunction { fnName = "putChar", fnType = tChar `fn` io tUnit, fnClassName = "BuildIn/putChar" }

putNewLineFun = BuildInFunction { fnName = "putNewLine", fnType = io tUnit, fnClassName = "BuildIn/putNewLine" }

getCharFun = BuildInFunction { fnName = "getChar", fnType = io tChar, fnClassName = "BuildIn/getChar" }

buildIn = [plusFun,returnFun,bindFun,putCharFun,getCharFun,unitFun,putNewLineFun]
