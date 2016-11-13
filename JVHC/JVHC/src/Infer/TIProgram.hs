module Infer.TIProgram where

import Infer.Assumption
import Infer.TIMain
import Infer.TIM

type Program = [BindGroup]

tiProgram :: [Assumption] -> Program -> [Assumption]
tiProgram as prog = runTI $ tiSeq tiBindGroup as prog

