module TimeGHCProgram.RunFibo where

import Criterion.Main

import Control.DeepSeq

import TimeProgram.RunFibo

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n | n > 1 = fib (n-1) + fib (n-2)

benchFib :: Integer -> Benchmark
benchFib n = bench (show n) $ (nf fib) n

benchmark :: Benchmark
benchmark = bgroup "Fib" (map benchFib input)

main = defaultMain [benchmark]
