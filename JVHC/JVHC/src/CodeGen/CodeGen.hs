module CodeGen.CodeGen where

import CodeGen.CGMonad(CG)
import CodeGen.JTypes(JType)

import Codec.JVM(Code)

type CodeGen a = a -> CG (Code,JType)
