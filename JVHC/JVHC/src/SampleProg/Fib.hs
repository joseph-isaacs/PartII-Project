module SampleProg.Fib where

fib :: String
fib = "fib fibn = case fibn of {\
       \     0 -> 1;\
       \     1 -> 1;\
       \     n -> plus (fib (dec n)) (fib (dec (dec n)))\
       \  }"
