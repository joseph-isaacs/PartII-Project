import Prelude hiding (even)

foo x = x + 1
bar x = x - 3
even x = x `mod` 2 == 0

ifThenElse b e e' = case b of
                      True -> e
                      False -> e'

testN n = ifThenElse (n < 0) n (ifThenElse (even n) (testN (foo n)) (testN (bar n)))

main = putStr $ show $ testN 300
