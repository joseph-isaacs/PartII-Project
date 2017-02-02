module SampleProg.EvenOdd where

evenOdd :: String
evenOdd = "even eN = case eN of {\
          \    0 -> 0;\
          \    enn -> odd (dec enn)\
          \  };\
          \ odd odN = case odN of {\
          \   oddn -> even (dec oddn)\
          \  }"


