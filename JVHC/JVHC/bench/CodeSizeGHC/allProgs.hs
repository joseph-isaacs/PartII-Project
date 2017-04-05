import Prelude hiding(odd,even,List,take,foldr)

data List a = Cons a (List a)
            | Nil

even n = case n of
            0 -> 0;
            n -> odd (n-1)

odd n = case n of
          n -> even (n-1)

fib n = case n of
    0 -> 1
    1 -> 1
    n -> fib (n-1) + fib (n-2)



nat x = Cons x (nat (x+1))

take n l = case n of
             0 -> Nil
             n -> case l of
               Cons h t -> Cons h (take (n-1) t)
               d -> Nil

comp fF fR aaa = fF (fR aaa)

sumList y = case y of
              Cons h t -> (sumList t) + h
              Nil -> 0


map fMapF = foldr (comp Cons fMapF) Nil

foldr fFun acc fList = case fList of
                         Cons fh ft -> fFun fh (foldr fFun acc ft)
                         Nil -> acc

foo x = x + 1
bar x = x - 3
even' x = x `mod` 2 == 0

ifThenElse b e e' = case b of
                      True -> e
                      False -> e'

testN n = ifThenElse (n < 0) n (ifThenElse (even' n) (testN (foo n)) (testN (bar n)))

main = putStr $ show $ testN 3000
