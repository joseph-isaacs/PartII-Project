{-# LANGUAGE OverloadedStrings #-}

module CodeGen.JTypes where


import Codec.JVM
import Data.Text

type JType = FieldType

charFieldName :: Text
charFieldName = "java/lang/Character"

jCharType :: FieldType
jCharType = obj charFieldName

charConstructor :: MethodRef
charConstructor = mkConstructorRef charFieldName [jchar]

jCharValue :: Text
jCharValue = "charValue"

integerFieldName :: Text
integerFieldName = "java/lang/Integer"

jIntType :: JType
jIntType = obj integerFieldName

jIntValue :: Text
jIntValue = "intValue"

integerConstructor :: MethodRef
integerConstructor = mkConstructorRef integerFieldName [jint]

intThunkName :: Text
intThunkName = "BuildIn/IntThunk"

intThunkType :: JType
intThunkType = obj intThunkName

intThunkConstructor :: MethodRef
intThunkConstructor = mkConstructorRef intThunkName [jint]

charThunkName :: Text
charThunkName = "BuildIn/CharThunk"

charThunkType :: JType
charThunkType = obj charThunkName

charThunkConstructor :: MethodRef
charThunkConstructor = mkConstructorRef charThunkName [jchar]

mkConstructorRef :: Text -> [FieldType] -> MethodRef
mkConstructorRef fn input = mkMethodRef fn "<init>" input void

objThunkName :: Text
objThunkName = "BuildIn/ObjThunk"

objThunkType :: JType
objThunkType = obj objThunkName

objThunkConstructor :: MethodRef
objThunkConstructor = mkConstructorRef objThunkName [jobject]

stringName :: Text
stringName = "java/lang/String"

stringType :: JType
stringType = obj stringName

runTimeException :: Text
runTimeException = "java/lang/RuntimeException"

runTimeExcpType :: JType
runTimeExcpType = obj runTimeException

ioName :: Text
ioName = "BuildIn/IO"

ioJType :: JType
ioJType = obj "BuildIn/IO"

mainName :: Text
mainName = "main"

mainType :: JType
mainType = obj mainName

envName :: Text
envName = "Env"

envType :: JType
envType = obj envName
