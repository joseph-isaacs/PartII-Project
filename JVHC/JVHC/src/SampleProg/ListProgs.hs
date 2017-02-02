module ListProgs where


listDataType = "data List a = Cons a (List a) | Nil"

nat = "nat :: Int -> List Int;\
      \ nat x = Cons x (nat (plus x 1))"

take' = "take :: Int -> List a -> List a;\
      \ take number listS = case number of {\
      \  0 -> Nil;\
      \  un -> case listS of {\
      \     Cons listH listTail -> Cons listH (take (dec un) listTail);\
      \     dgs -> Nil\
      \  }\
      \ }"

comp = "comp :: (b -> c) -> (a -> b) -> a -> c;\
      \ comp fF fR aaa = fF (fR aaa)"

sumList = "sumList :: List Int -> Int;\
         \ sumList y = case y of { Cons h t -> plus (sumList t) h ; Nil -> 0 }"


map =   "map :: (a -> b) -> List a -> List b;\
       \ map fMapF = foldr (comp Cons fMapF) Nil"

foldr = "foldr :: (a -> b -> b) -> b -> List a -> b;\
       \ foldr fFun acc fList = case fList of {\
       \   Cons fh ft -> fFun fh (foldr fFun acc ft);\
       \   Nil -> acc\
       \ }"


