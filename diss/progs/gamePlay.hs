data Tree a = Node a [Tree a]
  deriving Show

type Position = (Int,Int)

moves :: Position -> [Position]
moves (a,b) = [(a+1,b),(a,b+1)]

repTree f x = Node x (map (repTree f) (f x))

gameTree x = repTree moves x

bredthFirst :: Tree a -> [a]
bredthFirst x = bredthFirst' [x]
  where
    bredthFirst' ((Node n lfs: ts)) =
      n : bredthFirst' (ts ++ lfs)
