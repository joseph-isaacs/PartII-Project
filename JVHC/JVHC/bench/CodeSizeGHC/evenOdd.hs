import Prelude hiding(odd,even)

even n = case n of
            0 -> 0;
            n -> odd (n-1)

odd n = case n of
          n -> even (n-1)

main = putStr $ show $ even 30
