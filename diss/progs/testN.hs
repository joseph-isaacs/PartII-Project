foo x = x + 1
bar x   = x - 3

even' x = x `mod` 2 == 0

testN n = if n < 0
             then n
             else if even' n
                    then testN (foo n)
                    else testN (bar n)
