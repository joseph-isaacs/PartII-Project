import Prelude hiding(List,take,foldr)

data List a = Cons a (List a)
            | Nil

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

main = putStr $ show $ sumList (take 100 (nat 0))
