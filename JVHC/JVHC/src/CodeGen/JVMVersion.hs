module CodeGen.JVMVersion where

import Codec.JVM(java8,mkClassFile)

jvmVersion = java8

mkClassFileV = mkClassFile jvmVersion
