module SampleProg.ProgMaker where

import Data.List(intersperse)

functionsToProg :: [String] -> String
functionsToProg p = unlines  ("{" : (intersperse ";" p) ++ ["}"])
