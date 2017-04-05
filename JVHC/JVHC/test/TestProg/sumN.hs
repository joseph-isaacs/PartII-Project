{
  sumN numberN = case numberN of {
    0 -> 0;
     tt -> plus numberN (sumN (dec numberN))
   };
