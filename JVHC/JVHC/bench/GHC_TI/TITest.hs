
u = let f0 x f = f x x in
    let f1 a = f0 a in
    let f2 y = f1 (f1 y) in
    let f3 y = f2 (f2 y) in
    let f4 y = f3 (f3 y) in
    let f5 y = f4 (f4 y) in
    let f6 y = f5 (f5 y) in
      \z -> f6 (\b -> b) z

main = undefined
