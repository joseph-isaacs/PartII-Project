{
  data List a = Cons a (List a) | Nil;

  list1 = Cons 1 Nil;

  -- nat x = Cons x (nat (plus x 1));

  -- take number listS = case number of {
  --   0 -> Cons 1 Nil;
  --   un -> case listS of {
  --           Cons listH listTail -> Cons listH (take (sub un 1) listTail);
  --           dgs -> Nil
  --   }
  -- };

-- comp fF fR aaa = fF (fR aaa);

--  multiply a b = case a of {
--    0 -> 0;
--    1 -> b;
--    uuu -> plus b (multiply (plus (neg 1) uuu) b)
--  };

  -- sumList y = case y of { Cons h t -> plus (sumList t) h ; Nil -> 0 };


--  map mapFun l = case l of { Cons first second -> Cons (mapFun first) (map mapFun second); Nil -> Nil };

  -- map fMapF = foldr (comp Cons fMapF) Nil;
  -- foldr fFun acc fList = case fList of { Cons fh ft -> fFun fh (foldr fFun acc ft); Nil -> acc };

--  se fstSE sndSE = fstSE;
  -- id =    (\asd -> asd) (\yyyyy -> yyyyy) id2 id2 id2;
--    case Cons 1 Nil of { Cons asdf asdfasd -> \idF -> idF };
  -- id2 ttr = ttr;

--  undef = undef;

 -- listTT = Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil))));

  -- list1 = Cons 1 Nil;

--  list100 = take 100 (nat 0);

  -- bind_ ma mb = bind ma (\uu -> mb);

--  undef = undef;
--
--

  -- fib fibN = case fibN of {
  --   0 -> 1;
  --   1 -> 1;
  --   n -> plus (fib (sub n 1)) (fib (sub n 2))
  -- };

  -- sumN numberN = case numberN of {
  --   0 -> 0;
  --    tt -> plus numberN (sumN (dec numberN))
  --  };

  -- head2 headArg = case headArg of {
  --   hardArgCase -> case hardArgCase of {
  --     hardArgCCase -> case 1 of {
  --       1 -> case hardArgCCase of {
  --         Cons headHed headTaal -> case headHed of {
  --           assdfasd -> assdfasd
  --         }
  --       }
  --     }
  --   }
  -- };

--  head ght = case ght of {
--    Cons hhht ttt2 ->  hhht
--  };
--
-- id x = x;

even eN = case eN of {
  0 -> 0;
  enn -> odd (dec enn)
};

odd odN = case odN of {
  oddn -> even (dec oddn)
};

--  main = bind_ (bind_ (bind getChar (\ee -> putChar ee)) (putChar 'n')) putNewLine
--  main = putInt (sumList (take 970 (nat 0)))
--  main = putInt (head (map (plus 1)(nat 1)))
--  main = putInt (id 500)
--  main = putInt (seq id (multiply 2 3))
--  main = putInt (plus (head (Cons   1 Nil)) 1)
-- main = putInt (head (take 40 list1))
main = putInt (odd 3)
--main = putInt 2

}
