
u = let f0 x f = f x x in
    let f1 a = f0 a in
    let f2 y = f1 (f1 y) in
      \z -> f2 (\b -> b) z

main = undefined
