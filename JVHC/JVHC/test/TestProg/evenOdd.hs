{
  -- sumN numberN = case numberN of {
  --   0 -> 0;
  --    tt -> plus numberN (sumN (dec numberN))
  --  };

 even eN = case eN of {
   0 -> 0;
   enn -> odd (dec enn)
 };

 odd odN = case odN of {
   oddn -> even (dec oddn)
 };
 main = putInt (odd 2)
}
