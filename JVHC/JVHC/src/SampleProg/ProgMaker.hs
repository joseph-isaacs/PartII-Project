module SampleProg.ProgMaker where

import Data.List(intersperse)

functionsToProg :: [String] -> String
functionsToProg p = "{" ++ functionsToProgFrag p ++ "}"

functionsToProgFrag :: [String] -> String
functionsToProgFrag ps = unlines (intersperse ";" ps)

mkPutIntMainMethod :: Int -> String -> String
mkPutIntMainMethod times fn = "main = putInt (" ++ fn ++ " " ++ show times ++ ")"
