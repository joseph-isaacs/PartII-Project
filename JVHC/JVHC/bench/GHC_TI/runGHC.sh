#! /bin/bash

for i in {1..20}
do 
  start_ns=$(gdate +%s%N)
  ghc TITest.hs > /dev/null
  end_ns=$(gdate +%s%N)
  real_time=$((($end_ns - $start_ns) ))
  echo $real_time
  rm -rf TITest TITest.o TITest.hi
done

