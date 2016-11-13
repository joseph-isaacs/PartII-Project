module Infer.Id where

type Id = String
enumId :: Int ->  Id
enumId n = "_internal" ++ (show n)
