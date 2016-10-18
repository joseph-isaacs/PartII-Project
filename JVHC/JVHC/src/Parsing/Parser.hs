{-# OPTIONS_GHC -w #-}
{-# OPTIONS -cpp #-}
module Parser (jvhcParse) where

import qualified Lexer as L
import Lexer (LToken)
import AST
import qualified Data.Array as Happy_Data_Array
import qualified System.IO as Happy_System_IO
import qualified System.IO.Unsafe as Happy_System_IO_Unsafe
import qualified Debug.Trace as Happy_Debug_Trace
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn 
	= HappyTerminal (L.LToken)
	| HappyErrorToken Int
	| HappyAbsSyn4 (Body)
	| HappyAbsSyn5 ([ TopDecl ])
	| HappyAbsSyn6 (TopDecl)
	| HappyAbsSyn7 (SimpleType)
	| HappyAbsSyn8 ([TVar])
	| HappyAbsSyn9 ([Constr])
	| HappyAbsSyn10 (Constr)
	| HappyAbsSyn11 ([AType])
	| HappyAbsSyn12 (AType)
	| HappyAbsSyn15 (Decl)
	| HappyAbsSyn16 (([VarID], AType))
	| HappyAbsSyn17 ([VarID])
	| HappyAbsSyn18 (FunLHS)
	| HappyAbsSyn20 (Exp)
	| HappyAbsSyn22 (ExpC)
	| HappyAbsSyn25 ([Alt])
	| HappyAbsSyn26 (Alt)
	| HappyAbsSyn27 (Pat)
	| HappyAbsSyn28 ([Pat])

happyActOffsets :: Happy_Data_Array.Array Int Int
happyActOffsets = Happy_Data_Array.listArray (0,89) ([139,139,45,116,137,136,0,0,135,133,0,0,132,11,55,0,0,129,128,0,11,0,131,0,0,19,33,45,0,0,0,42,0,33,0,0,0,0,0,7,19,121,0,0,130,19,0,119,117,0,0,0,127,0,126,33,0,108,47,118,125,0,124,0,33,0,0,0,19,123,122,0,33,111,0,0,-9,56,0,19,76,52,60,19,-9,0,0,0,0,0
	])

happyGotoOffsets :: Happy_Data_Array.Array Int Int
happyGotoOffsets = Happy_Data_Array.listArray (0,89) ([66,0,5,0,0,0,0,0,0,28,0,0,50,97,36,0,0,25,8,0,95,0,0,30,0,83,102,1,0,0,0,-7,0,99,0,0,0,0,0,98,79,0,0,0,0,75,0,0,110,0,0,0,0,0,0,106,0,0,29,0,0,0,0,0,96,0,0,0,71,0,0,0,41,3,0,0,54,0,0,67,0,0,0,63,49,0,0,0,0,0
	])

happyDefActions :: Happy_Data_Array.Array Int Int
happyDefActions = Happy_Data_Array.listArray (0,89) ([0,0,0,0,0,-4,-6,-22,0,0,-28,-47,0,-49,-50,-51,-27,0,-29,-46,-49,-50,0,-9,-23,0,0,0,-2,-3,-19,-20,-24,0,-16,-15,-31,-32,-36,-37,0,0,-40,-39,0,0,-41,-7,0,-48,-30,-25,-26,-5,-11,-14,-8,0,0,0,0,-38,0,-18,0,-21,-17,-42,0,0,0,-12,-14,0,-10,-13,0,0,-33,0,0,-44,0,0,0,-35,-34,-43,-45
	])

happyCheck :: Happy_Data_Array.Array Int Int
happyCheck = Happy_Data_Array.listArray (0,165) ([-1,8,1,2,13,14,1,2,5,6,3,20,11,12,13,14,11,12,13,14,13,14,3,15,23,14,25,20,23,10,25,20,13,14,4,16,3,18,13,20,11,12,13,14,16,3,13,14,7,8,8,15,23,3,25,13,14,12,13,14,13,14,7,11,9,20,0,20,8,14,21,22,23,17,25,21,22,23,2,25,17,18,19,20,17,18,19,20,17,18,19,20,17,18,19,20,17,18,19,20,17,18,19,20,8,9,10,8,9,10,8,9,10,7,8,5,6,19,20,24,25,24,25,1,13,2,8,19,4,4,13,1,6,14,7,14,5,21,5,2,1,-1,14,14,9,13,-1,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
	])

happyTable :: Happy_Data_Array.Array Int Int
happyTable = Happy_Data_Array.listArray (0,165) ([0,63,29,5,14,22,4,5,74,54,41,16,6,7,8,9,6,7,8,9,43,44,41,50,10,22,11,47,10,42,11,16,43,44,47,45,34,46,51,47,69,7,8,9,24,34,35,36,75,72,65,16,10,22,11,35,36,13,14,15,14,15,18,85,-26,16,3,16,84,19,87,81,82,80,11,80,81,82,86,11,88,37,38,39,86,37,38,39,78,37,38,39,57,37,38,39,60,37,38,39,36,37,38,39,30,31,65,30,31,62,30,31,32,71,72,53,54,61,39,49,20,19,20,77,56,78,69,71,67,68,56,59,74,57,18,60,49,-1,26,29,3,0,19,53,27,24,0,28,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

happyReduceArr = Happy_Data_Array.array (1, 50) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50)
	]

happy_n_terms = 22 :: Int
happy_n_nonterms = 26 :: Int

happyReduce_1 = happySpecReduce_3  0 happyReduction_1
happyReduction_1 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (TTopDecls happy_var_2
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3  1 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 : happy_var_3
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  1 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 ([happy_var_1]
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happyReduce 4 2 happyReduction_4
happyReduction_4 ((HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (TData happy_var_2 $ reverse happy_var_4
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_1  2 happyReduction_5
happyReduction_5 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn6
		 (TDecl happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_2  3 happyReduction_6
happyReduction_6 (HappyAbsSyn8  happy_var_2)
	(HappyTerminal (L.Conid      happy_var_1))
	 =  HappyAbsSyn7
		 (TSimpleType happy_var_1 $ reverse happy_var_2
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  4 happyReduction_7
happyReduction_7 (HappyTerminal (L.Varid      happy_var_2))
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_2 : happy_var_1
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_0  4 happyReduction_8
happyReduction_8  =  HappyAbsSyn8
		 ([]
	)

happyReduce_9 = happySpecReduce_3  5 happyReduction_9
happyReduction_9 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 : happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  5 happyReduction_10
happyReduction_10 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  6 happyReduction_11
happyReduction_11 (HappyAbsSyn11  happy_var_2)
	(HappyTerminal (L.Conid      happy_var_1))
	 =  HappyAbsSyn10
		 (TConstr happy_var_1 happy_var_2
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  7 happyReduction_12
happyReduction_12 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 : happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_0  7 happyReduction_13
happyReduction_13  =  HappyAbsSyn11
		 ([]
	)

happyReduce_14 = happySpecReduce_1  8 happyReduction_14
happyReduction_14 (HappyTerminal (L.Varid      happy_var_1))
	 =  HappyAbsSyn12
		 (TTyVar happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  8 happyReduction_15
happyReduction_15 (HappyTerminal (L.Conid      happy_var_1))
	 =  HappyAbsSyn12
		 (TGTyCon happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  8 happyReduction_16
happyReduction_16 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_2  9 happyReduction_17
happyReduction_17 (HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_2 : happy_var_1
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  9 happyReduction_18
happyReduction_18 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  10 happyReduction_19
happyReduction_19 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn12
		 (mkNestedT happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  10 happyReduction_20
happyReduction_20 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn12
		 (TATypeArrow (mkNestedT happy_var_1) happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  11 happyReduction_21
happyReduction_21 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 ((\(v,t) -> TGenDecl v t)happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_2  11 happyReduction_22
happyReduction_22 (HappyAbsSyn20  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn15
		 (TFunDecl happy_var_1 happy_var_2
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  12 happyReduction_23
happyReduction_23 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 ((happy_var_1,happy_var_3)
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  13 happyReduction_24
happyReduction_24 (HappyAbsSyn17  happy_var_3)
	_
	(HappyTerminal (L.Varid      happy_var_1))
	 =  HappyAbsSyn17
		 (happy_var_1 : happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  13 happyReduction_25
happyReduction_25 (HappyTerminal (L.Varid      happy_var_1))
	 =  HappyAbsSyn17
		 ([happy_var_1]
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_2  14 happyReduction_26
happyReduction_26 (HappyAbsSyn17  happy_var_2)
	(HappyTerminal (L.Varid      happy_var_1))
	 =  HappyAbsSyn18
		 (TVarPat (TVarID happy_var_1) happy_var_2
	)
happyReduction_26 _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  14 happyReduction_27
happyReduction_27 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn18
		 (TVarPat happy_var_1 []
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  15 happyReduction_28
happyReduction_28 (HappyTerminal (L.Varid      happy_var_1))
	 =  HappyAbsSyn17
		 ([happy_var_1]
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_2  15 happyReduction_29
happyReduction_29 (HappyAbsSyn17  happy_var_2)
	(HappyTerminal (L.Varid      happy_var_1))
	 =  HappyAbsSyn17
		 (happy_var_1 : happy_var_2
	)
happyReduction_29 _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_2  16 happyReduction_30
happyReduction_30 (HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (happy_var_2
	)
happyReduction_30 _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  17 happyReduction_31
happyReduction_31 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn20
		 (TExp        happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happyReduce 4 18 happyReduction_32
happyReduction_32 ((HappyAbsSyn20  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.Varid      happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (TELambda happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_33 = happyReduce 6 18 happyReduction_33
happyReduction_33 ((HappyAbsSyn20  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (TELet happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_34 = happyReduce 6 18 happyReduction_34
happyReduction_34 (_ `HappyStk`
	(HappyAbsSyn25  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (TECase happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_35 = happySpecReduce_1  18 happyReduction_35
happyReduction_35 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  19 happyReduction_36
happyReduction_36 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_2  19 happyReduction_37
happyReduction_37 (HappyAbsSyn22  happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (TEApp (TExp happy_var_1) (TExp happy_var_2)
	)
happyReduction_37 _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  20 happyReduction_38
happyReduction_38 (HappyTerminal (L.Varid      happy_var_1))
	 =  HappyAbsSyn22
		 (TEVar happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  20 happyReduction_39
happyReduction_39 (HappyTerminal (L.Conid      happy_var_1))
	 =  HappyAbsSyn22
		 (TEConstr happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  20 happyReduction_40
happyReduction_40 (HappyTerminal (L.Literal    happy_var_1))
	 =  HappyAbsSyn22
		 (TELiteral happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  20 happyReduction_41
happyReduction_41 _
	(HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn22
		 (TEExp happy_var_2
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  21 happyReduction_42
happyReduction_42 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1 : happy_var_3
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  21 happyReduction_43
happyReduction_43 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn25
		 ([happy_var_1]
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  22 happyReduction_44
happyReduction_44 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn26
		 (TAlt happy_var_1 happy_var_3
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_2  23 happyReduction_45
happyReduction_45 (HappyAbsSyn28  happy_var_2)
	(HappyTerminal (L.Conid      happy_var_1))
	 =  HappyAbsSyn27
		 (TPat happy_var_1 (reverse happy_var_2)
	)
happyReduction_45 _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  23 happyReduction_46
happyReduction_46 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_2  24 happyReduction_47
happyReduction_47 (HappyAbsSyn28  happy_var_2)
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn28
		 (happy_var_1 : happy_var_2
	)
happyReduction_47 _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_0  24 happyReduction_48
happyReduction_48  =  HappyAbsSyn28
		 ([]
	)

happyReduce_49 = happySpecReduce_1  25 happyReduction_49
happyReduction_49 (HappyTerminal (L.Varid      happy_var_1))
	 =  HappyAbsSyn27
		 (TVarID   happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  25 happyReduction_50
happyReduction_50 (HappyTerminal (L.Literal    happy_var_1))
	 =  HappyAbsSyn27
		 (TLiteral happy_var_1
	)
happyReduction_50 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	happyDoAction 21 notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	L.Special  L.LCurly -> cont 1;
	L.Special  L.RCurly -> cont 2;
	L.Special  L.LParen -> cont 3;
	L.Special  L.RParen -> cont 4;
	L.ReservedOP  L.Equal -> cont 5;
	L.ReservedOP  L.Pipe -> cont 6;
	L.Special  L.Comma -> cont 7;
	L.ReservedOP L.RArrow -> cont 8;
	L.ReservedOP L.DColon -> cont 9;
	L.ReservedOP L.BSlash -> cont 10;
	L.Special L.SemiColon -> cont 11;
	L.ReserveID  L.Data -> cont 12;
	L.Conid      happy_dollar_dollar -> cont 13;
	L.Varid      happy_dollar_dollar -> cont 14;
	L.Conid      happy_dollar_dollar -> cont 15;
	L.ReserveID  L.Let -> cont 16;
	L.ReserveID  L.In -> cont 17;
	L.ReserveID  L.Case -> cont 18;
	L.ReserveID  L.Of -> cont 19;
	L.Literal    happy_dollar_dollar -> cont 20;
	_ -> happyError' (tk:tks)
	}

happyError_ 21 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = return
    (<*>) = ap
instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(L.LToken)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

jvhcParse tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse 0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [LToken] -> a
parseError t = error $ (show t) ++  "Parse Error"

mkNestedT :: [AType] -> AType
mkNestedT = TATypeNested . reverse
{-# LINE 1 "templates/GenericTemplate.hs" #-}


















-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 






data Happy_IntList = HappyCons Int Happy_IntList











happyTrace string expr = Happy_System_IO_Unsafe.unsafePerformIO $ do
    Happy_System_IO.hPutStr Happy_System_IO.stderr string
    return expr




infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (0), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (0) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = (happyTrace ("state: " ++ show (st) ++ 
                      
          ",\ttoken: " ++ show (i) ++
                      
          ",\taction: ")) $
          

          case action of
                (0)           -> (happyTrace ("fail.\n")) $
                                     happyFail i tk st
                (-1)          -> (happyTrace ("accept.\n")) $
                                     happyAccept i tk st
                n | (n < ((0) :: Int)) -> (happyTrace ("reduce (rule " ++ show rule
                                                               
                                                   ++ ")")) $
                                                   
                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = ((negate ((n + ((1) :: Int)))))
                n                 -> (happyTrace ("shift, enter state "
                                                 
                                     ++ show (new_state)
                                                 
                                     ++ "\n")) $
                                     

                                     happyShift new_state i tk st
                                     where new_state = (n - ((1) :: Int))
   where off    = indexShortOffAddr happyActOffsets st
         off_i  = (off + i)
         check  = if (off_i >= ((0) :: Int))
                  then (indexShortOffAddr happyCheck off_i ==  i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st


indexShortOffAddr arr off = arr Happy_Data_Array.! off








-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (0) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (0) tk st sts stk
     = happyFail (0) tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (0) tk st sts stk
     = happyFail (0) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (0) tk st sts stk
     = happyFail (0) tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (0) tk st sts stk
     = happyFail (0) tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (0) tk st sts stk
     = happyFail (0) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn (0) tk st sts stk
     = happyFail (0) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (0) tk st sts stk
     = happyFail (0) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = indexShortOffAddr happyGotoOffsets st1
             off_i = (off + nt)
             new_state = indexShortOffAddr happyTable off_i



          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   (happyTrace (", goto state " ++ show (new_state) ++ "\n")) $
   happyDoAction j tk new_state
   where off = indexShortOffAddr happyGotoOffsets st
         off_i = (off + nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery ((0) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (0) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (0) tk old_st (HappyCons ((action)) (sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction (0) tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction (0) tk action sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.

