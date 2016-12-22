{-# LANGUAGE OverloadedStrings #-}

module CodeGen.CGCustomDataType where

import CoreAST.DataCon
import CoreAST.Types

import CodeGen.JVMVersion
import CodeGen.JTypes
import CodeGen.TypeToJType
import CodeGen.JInterfaces
import CodeGen.CGHelper

import Data.String(fromString)
import Data.Text(Text)
import Data.Monoid((<>), mempty)

import Codec.JVM(mkClassFile,AccessFlag,AccessFlag(..),jobjectC,jobject,MethodDef
                ,ClassFile,FieldDef,mkFieldDef,Code,mkConstructorDef,mkDefaultConstructor,ret
                ,gload,swap,gconv,obj,putfield,getfield,mkFieldRef,new,dup,invokespecial,void,mkMethodRef)

mkDataType :: TyCon -> [(Text,ClassFile)]
mkDataType (MkTyCon {mtyCon = tyCon, constrs = cons}) =
  (mkInterface tyCon :  map mkDataConType cons) ++ (concatMap mkDataConstructors cons)

-- This will make n curried constructors where n is (length f) the final constructor will
-- return a data type, the other will return the next construct in the set.
mkDataConstructors :: DataCon -> [(Text,ClassFile)]
mkDataConstructors (MkDataCon { dName = name, tName = iface, fields = f }) =
    mkCurriedFunction ctrName jFields code
  where jFields = map toJType f
        dataName = fromString name
        dataType = obj dataName
        code = ( newDup objThunkType
              <> newDup dataType
              <> (mconcat . reverse $  (map (getter lenF)) input)
              <> invokespecial (mkMethodRef dataName "<init>" (replicate (length f) thunk) void))
              <> invokespecial objThunkConstructor
        lenF = length f - 1
        input = zip f [lenF,lenF - 1..]
        ctrName = fromString name
        getter l (t,c) = gload jobject 0
                    <> foldr (\n acc -> (getfield (mkFieldRef (cn n) (cn (n-1)) (obj (cn (n-1))) )) <> acc)
                          start [l,l-1..c+1]
          where cn v = ctrName `mappend` if v >= 0 then ((fromString . show) v) else (error (show v))
                start = getfield (mkFieldRef (cn c) (cn c) thunk)



-- Must make a single constructor taking all the types
mkDataConType :: DataCon -> (Text,ClassFile)
mkDataConType (MkDataCon { dName = name, tName = iface, fields = f }) =
  (n
  ,mkClassFile
    jvmVersion
    lamAccessors
    n
    Nothing
    [fromString iface]
    (mkDataFields n f)
    [mkConstructorMethodDef n (map toJType f)])
      where n = fromString name

mkDataFields :: Text -> [Type] -> [FieldDef]
mkDataFields name ts = map (\(_,n) -> mkFieldDef fieldAccessor (toFieldName name n) thunk) (zip ts [0..])

mkConstructorMethodDef :: Text   ->  -- Name
                          [JType] ->  -- Arguments
                          MethodDef

mkConstructorMethodDef name args =
  mkConstructorDef name jobjectC (map (\_ -> thunk) args) (mconcat $ map setter (zip args [0..]))
    where setter (_,n) =
                 (gload jobject 0
               <> gload jobject (n+1)
               <> putfield (mkFieldRef name  (toFieldName name n) thunk))

toFieldName :: (Show a, Num a) => Text -> a -> Text
toFieldName n num = n `mappend` fromString (show num)

mkInterface :: Tycon -> (Text,ClassFile)
mkInterface (Tycon n _) =
  (fromString n, mkClassFile jvmVersion [Interface,Abstract,Public] (fromString n) Nothing [] [] [])
