{-# LANGUAGE OverloadedStrings #-}

module CodeGen.CGHelper where


import CodeGen.JInterfaces
import CodeGen.JTypes
import CodeGen.JVMVersion

import Codec.JVM


import Control.Monad(liftM)
import Data.Text(Text)
import Data.String(fromString)
import Data.Monoid((<>))


lamAccessors :: [AccessFlag]
lamAccessors = [Public,Super]


fieldAccessor :: [AccessFlag]
fieldAccessor = [Protected]

newDup :: FieldType -> Code
newDup f = new f <> dup f

mkLambdaClass :: Text ->   -- Class Name
                 Text ->   -- Parent Name
                 JType ->  -- Binder JType
                 JType ->  -- Body   JType
                 JType ->  -- Parent JType
                 Code  ->  -- Lam Body Code
                 ClassFile

mkLambdaClass name pname tyB tyE tyP eC =
 mkClassFile jvmVersion lamAccessors name
             Nothing [functionInterface] fields methods
    where
      fields = [ mkFieldDef [Protected] name tyB
               , mkFieldDef [Protected] pname   tyP
               ]
      methods = [ mkConstructorDef name jobjectC [tyP] (
                     fieldSetterCode 1 name pname tyP
                  <> gload jobject 0
                   ),
                  mkApplyFun name (
                        fieldSetterCode 1 name name tyB
                     <> gload jobject 0
                     <> eC
                     <> gconv tyE jobject)]

mkApplyFun :: Text -> Code -> MethodDef
mkApplyFun name code =  mkMethodDef name [Public] "apply" [jobject] (ret jobject) (
                        code
                     <> greturn jobject)

fieldSetterCode :: Int ->   -- Arg Pos
              Text ->  -- Class Name
              Text ->  -- Field Name
              JType -> -- Field Type
              Code
fieldSetterCode pos cName fName fType =
     gload jobject 0
  <> gload jobject pos
  <> gconv jobject fType
  <> putfield (mkFieldRef cName fName fType)

-- Will put the argument in on the top of the stack with the first arguement at the bottom
mkCurriedFunction :: Text    ->  -- Name
                     [JType] ->  -- Arguments in order, will be named [0..]
                     Code    ->  -- Inner function code
                     [(Text,ClassFile)]

mkCurriedFunction name args code = fst $ foldr driver ([],code) input
 where driver :: (JType, Integer) -> ([(Text,ClassFile)],Code) -> ([(Text,ClassFile)],Code)
       driver (t,n) (cf,code) = (classF : cf,newCode)
          where
            classF = (className, mkClassFileV lamAccessors className Nothing [functionInterface] fields methods)
            parentName = name `mappend` (if n > 0 then (fromString $ show (n-1)) else "")
            parentField = mkFieldDef fieldAccessor parentName (obj parentName)
            parentType = obj parentName
            className = name `mappend` ((fromString . show) n)
            classType = obj className
            argField = mkFieldDef fieldAccessor className thunk
            fields = (if n > 0 then [parentField] else []) ++ [argField]
            ctrArg = mkConstructorDef className jobjectC [parentType] (fieldSetterCode 1 className parentName parentType)
            ctrDef = mkDefaultConstructor className jobjectC
            applyFun = mkApplyFun className (fieldSetterCode 1 className className thunk
                                          <> code)
            methods  = (if (n > 0) then [ctrArg] else [ctrDef]) ++ [applyFun]
            newCode = new classType
                   <> dup classType
                   <> gload parentType 0
                   <> invokespecial (mkMethodRef className "<init>" [parentType] void)
       input = zip args [0..]

