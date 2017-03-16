sumN :: Int -> Int
sumN n = case n of
  0 -> 0
  m -> m + (sumN (m-1))

badProg :: Int -> Int
badProg n = let n' = sumN n
              in plus n' (plus n' n')"
