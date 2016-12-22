module CodeGen.CodeGen where

import CodeGen.CGMonad
import CodeGen.JTypes
import CoreAST.Types

import Codec.JVM(Code)

type CodeGen a = a -> CG (Code,(JType,Type))
