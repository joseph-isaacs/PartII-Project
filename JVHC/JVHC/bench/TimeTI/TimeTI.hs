module TimeTI.TimeTI where

import System.CPUTime

import Control.Monad

import CoreAST.BuildInFunctions

import Desugar.DTopDecls(dTopDecls,splitDataType)

import Infer.TIProgram
import Infer.BuildInFunctionTypes

import Pipeline.Compiler


runTypeTiming :: Fractional a => String -> String -> IO a
runTypeTiming name programSource =
  do (bg,des)  <- (desugar . lexAndparse) programSource
     let sdt = splitDataType des
         ass = snd sdt ++ buildInAssumptions
     startTime <- liftM fromIntegral getCPUTime
     let (x1,_) = tiProgram ass [bg]
     seq x1 (return ())
     endTime   <- getCPUTime
     return $ fromIntegral (endTime - startTime) / (10^12)

pair2 = "{  u = let {f0 x f = f x x} in\
              \ let {f1 a = f0 a} in\
              \ let {f2 y = f1 (f1 y)} in\
                     \ \\z -> f2 (\\b -> b) z }"

pair3 = "{  u = let {f0 x f = f x x} in\
              \ let {f1 a = f0 a} in\
              \ let {f2 y = f1 (f1 y)} in\
              \ let {f3 j = f2 (f2 j)} in\
                   \ \\z -> f3 (\\b -> b) z }"


pair4 = "{  u = let {f0 x f = f x x} in\
              \ let {f1 a = f0 a} in\
              \ let {f2 y = f1 (f1 y)} in\
              \ let {f3 j = f2 (f2 j)} in\
              \ let {f4 k = f3 (f3 k)} in\
                   \ \\z -> f4 (\\b -> b) z }"

pair5= "{  u = let {f0 x f = f x x} in\
             \ let {f1 a = f0 a} in\
             \ let {f2 y = f1 (f1 y)} in\
             \ let {f3 j = f2 (f2 j)} in\
             \ let {f4 k = f3 (f3 k)} in\
             \ let {f5 l = f4 (f4 l)} in\
                   \ \\z -> f5 (\\b -> b) z }"

pair6= "{  u = let {f0 x f = f x x} in\
             \ let {f1 a = f0 a} in\
             \ let {f2 y = f1 (f1 y)} in\
             \ let {f3 j = f2 (f2 j)} in\
             \ let {f4 k = f3 (f3 k)} in\
             \ let {f5 l = f4 (f4 l)} in\
             \ let {f6 m = f5 (f5 m)} in\
                   \ \\z -> f6 (\\b -> b) z }"

pair7= "{  u = let {f0 x f = f x x} in\
             \ let {f1 a = f0 a} in\
             \ let {f2 y = f1 (f1 y)} in\
             \ let {f3 j = f2 (f2 j)} in\
             \ let {f4 k = f3 (f3 k)} in\
             \ let {f5 l = f4 (f4 l)} in\
             \ let {f6 m = f5 (f5 m)} in\
             \ let {f7 n = f6 (f6 n)} in\
                   \ \\z -> f7 (\\b -> b) z }"

