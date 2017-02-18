module TimeGHCProgram.RunTestN where

import Criterion.Main

-- import TimeProgram.RunInlineBench

numb :: [Int]
numb = 50 : map ( (\x -> x -10000 ) . floor . (*) 40000 . log . log) [4,6..80]

foo x = x + 1
bar x   = x - 3

even' x = x `mod` 2 == 0

testN n = if n < 0
             then n
             else if even' n
                    then testN (foo n)
                    else testN (bar n)

benchTestN :: Int -> Benchmark
benchTestN n = bench (show n) $ (nf testN) n

benchmark :: Benchmark
benchmark = bgroup "TestN" (map benchTestN numb)

main = defaultMain [benchmark]
