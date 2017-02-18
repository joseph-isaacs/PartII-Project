module Printing.CSVPrint where

import Data.CSV
import Data.List

type IsOp a = (Bool,a)


toCSV :: (Show b, Show a) => [(IsOp a,[b])] -> String
toCSV vals = genCsvFile $ transpose (map mergeTitle vals)
  where mergeTitle ((op,nm),l) = (showFilt nm ++ (if op then " Op" else " NoOp")) : (map showFilt l)
        showFilt :: Show a => a -> String
        showFilt = filter (/= '"') . show

