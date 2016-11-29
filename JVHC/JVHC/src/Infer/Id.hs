module Infer.Id where

type Id = String
enumId :: Int ->  Id
enumId n = "*" ++ (show n) ++ "*"

enumId2 :: Int -> Id
enumId2 n = "&" ++ (show n) ++ "&"
