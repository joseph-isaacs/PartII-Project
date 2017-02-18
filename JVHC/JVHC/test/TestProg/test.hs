{
   -- data List a = Cons a (List a) | Nil;

   -- list1 = Cons 1 Nil;
   -- nat :: Int -> List Int;
   -- nat natx = Cons natx (nat (plus natx 1));

  -- take :: Int -> List a -> List a;
  -- take number listS = case number of {
   --  0 -> Nil;
   --  un -> case listS of {
   --          Cons listH listTail -> Cons listH (take (dec un) listTail);
   --          dgs -> Nil
   --  }
  -- };

  -- -- comp :: (b -> c) -> (a -> b) -> a -> c;
  -- -- comp fF fR aaa = fF (fR aaa);

  -- sumList :: List Int -> Int;
  -- sumList sumy = case sumy of {
   --  Cons sumh sumt -> plus (sumList sumt) sumh;
   --  Nil -> 0
  -- };


--  map mapFun l = case l of { Cons first second -> Cons (mapFun first) (map mapFun second); Nil -> Nil };
  -- map :: (a -> b) -> List a -> List b;
  -- map fMapF = foldr (comp Cons fMapF) Nil;
  -- foldr :: (a -> b -> b) -> b -> List a -> b;
  -- foldr fFun acc fList = case fList of { Cons fh ft -> fFun fh (foldr fFun acc ft); Nil -> acc };


   -- bind_ ma mb = bind ma (\uu -> mb);

  fib fibN = case fibN of {
    0 -> 1;
    1 -> 1;
    n -> plus (fib (dec (dec n))) (fib (dec n))
  };

  -- sumN numberN = case numberN of {
   --  0 -> 0;
   --   tt -> plus numberN (sumN (dec numberN))
   -- };

  -- -- head2 headArg = case headArg of {
  -- --   hardArgCase -> case hardArgCase of {
  -- --     hardArgCCase -> case 1 of {
  -- --       1 -> case hardArgCCase of {
  -- --         Cons headHed headTaal -> case headHed of {
  -- --           assdfasd -> assdfasd
  -- --         }
  -- --       }
  -- --     }
  -- --   }
  -- -- };

   -- head ght = case ght of {
   --   Cons hhht ttt2 -> hhht
   -- };

  -- isZero zd = case zd of {
   --  0 -> 1;
   --  4 -> 1;
   --  nnn -> 0
  -- };

-- boolToInt btiB = case btiB of {
  -- True  -> 1;
  -- False -> 0
-- };


-- odd odN = case odN of {
--   oddn -> even (dec oddn)
-- };
--



-- foo fooX   = plus fooX 1;
-- bar barX   = sub barX 3;
-- even :: Int -> Bool;
-- even evenX = intEq (mod evenX 2) 0;

-- ifThenElse :: Bool -> a -> a -> a;
-- ifThenElse ifEB ifET ifEF = case ifEB of {
--   True  -> ifET;
--   False -> ifEF
-- };

--  testN :: Int -> Int;
--  testN tN = ifThenElse (intLT tN 0)
--                 tN
--                 (ifThenElse (even tN)
--                   (testN (foo tN))
--                   (testN (bar tN)));

sumN :: Int -> Int;
sumN sMn = case sMn of {
  0 -> 0;
  casesumNn -> plus (sumN (dec casesumNn)) casesumNn
};

badProg :: Int -> Int;
badProg bPp = let { bPx = sumN bPp } in plus bPx (plus bPx bPx);

-- testN tN = case (intLT tN 0) of {
--     False  -> tN

-- };
--
id idX = id idX;

main :: IO Unit;
main = putInt ((badProg 50))



}
