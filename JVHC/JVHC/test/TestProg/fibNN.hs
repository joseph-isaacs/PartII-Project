{
  fib fibn = case fibn of {
    0 -> 1;
    1 -> 1;
    n -> plus (fib (dec n)) (fib (dec (dec n)))
   };

main = putInt (fib 100)
}
