module Infer.TIProgram where

import CoreAST.CoreExpr
import CoreAST.Types

import Desugar.DExpr

import Infer.Assumption
import Infer.TIMain
import Infer.TIM
import Infer.Subst
import Infer.Id


tiProgram :: [Assumption] -> Program -> (CoreExprDefs,[Assumption])
tiProgram as prog = runTI $ (
  do (ict,a) <- tiSeq tiBindGroup as prog
     s <- getSubst
     let ict' = map (apply s) ict
     return (ict',a))

