module SampleProg.InlineProg(testProg) where

import SampleProg.ProgMaker

fooProg = "foo fooX   = plus fooX 1"
barProg = "bar barX   = sub barX 3"
evenProg = "even :: Int -> Bool;\
          \ even evenX = intEq (mod evenX 2) 0"

ifThenElseProg = "ifThenElse :: Bool -> a -> a -> a;\
                \ ifThenElse ifEB ifET ifEF = case ifEB of {\
                \  True  -> ifET;\
                \   False -> ifEF\
                \ }"

testNProg =  "testN :: Int -> Int;\
            \ testN tN = ifThenElse (intLT tN 0)\
            \   tN\
            \   (ifThenElse (even tN)\
            \     (testN (foo tN))\
            \     (testN (bar tN)))"

testProg = functionsToProgFrag [fooProg, barProg, evenProg, ifThenElseProg, testNProg]
