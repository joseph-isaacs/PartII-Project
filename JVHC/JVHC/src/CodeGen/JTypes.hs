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

jLongType :: JType
jLongType = obj jLongName

jLongName :: Text
jLongName = "java/lang/Long"

getLongValue :: Code
getLongValue = invokevirtual (mkMethodRef jLongName "longValue" [] (ret jlong))

longConstructor :: Code
longConstructor = invokespecial $ mkConstructorRef jLongName [jlong]

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

getPrintStream :: Code
getPrintStream =  getstatic (mkFieldRef "java/lang/System" "out" printStreamType)

invokePrintLn :: [JType] -> Code
invokePrintLn ty =  invokevirtual (mkMethodRef "java/io/PrintStream" "println" ty void)

printStreamType :: JType
printStreamType = obj printStreamName

printStreamName :: Text
printStreamName = "java/io/PrintStream"
