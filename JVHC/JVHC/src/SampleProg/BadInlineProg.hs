module SampleProg.BadInlineProg(badTestProg) where

import SampleProg.ProgMaker

sumNProg :: String
sumNProg = "sumN :: Int -> Int;\
          \ sumN sMn = case sMn of {\
          \   0 -> 0;\
          \   casesumNn -> plus (sumN (dec casesumNn)) casesumNn\
          \ }"

badProg :: String
badProg = "badProg :: Int -> Int;\
         \ badProg bPp =\
         \   let { bPx = sumN bPp }\
         \   in plus bPx (plus bPx bPx)"

badTestProg :: String
badTestProg = functionsToProgFrag [sumNProg,badProg]
