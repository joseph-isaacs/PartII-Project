module Desugar.TestDType where

import Parsing.ParsingAST

import Test.HUnit
import Desugar.DAType
import Desugar.DTypes

import CoreAST.Types
import CoreAST.Kind
import Infer.Id


tIO = Tycon "IO" (Kfun Star Star)

typeList = ([("IO", tIO),("Int", Tycon "Int" Star)])
assumptions = []
tia = (typeList,assumptions)

dsType = dAType tia

test1 = "Test Desugar (:: Int -> Int)" ~: (dsType
                                       (TATypeArrow (TGTyCon "Int") (TGTyCon "Int")))
                            ~=? Just (tInt `fn` tInt)

test2 = "Test Desugar [IO a -> IO b]"  ~: (dsType (TATypeArrow (TATypeAp (TGTyCon "IO") (TTyVar "a")) (TATypeAp (TGTyCon "IO") (TTyVar "b"))))
                            ~=? Just  ((TAp (TCon tIO) (TVar (Tyvar "a" Star))) `fn` (TAp (TCon tIO) (TVar (Tyvar "b" Star))))

tests = test [test1,test2]
