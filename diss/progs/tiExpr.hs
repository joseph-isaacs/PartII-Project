let f0 x f = f x x in
  let f1 x = f0 x in
    let f2 x = f1 (f1 x) in
      \z -> f2 (\b -> b) z