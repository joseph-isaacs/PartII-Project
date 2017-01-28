module CodeGen.CodePrint where

import Data.Monoid((<>))
import Data.String

import CodeGen.JTypes
import CodeGen.CGMonad

import Codec.JVM
import Codec.JVM.Const

printString :: String -> CG Code
printString s =
  do db <- isDebug
     return $ if db
               then (getPrintStream
                 <> gldc jstring (cstring (fromString s))
                 <> invokePrintLn[jstring])
               else mempty

