{-# OPTIONS_GHC -w #-}
-- Work around https://github.com/simonmar/happy/issues/109
-- #undef __GLASGOW_HASKELL__
-- #define __GLASGOW_HASKELL__ 709

module Kreb.Lang.Parser where

import Prelude hiding (Word)

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Kreb.Lang.Lexer
import qualified Kreb.Lang.LexicalGrammar as Lex
import Kreb.Lang.Module
import Kreb.Lang.Type
import Kreb.Lang.Expr
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.9

data HappyAbsSyn t21 t22 t23 t24 t25
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn5 (Module)
	| HappyAbsSyn6 (Decl)
	| HappyAbsSyn9 ([(Atom, [Type])])
	| HappyAbsSyn10 ((Atom, [Type]))
	| HappyAbsSyn11 (Scheme)
	| HappyAbsSyn12 (Arrow)
	| HappyAbsSyn13 (Stack)
	| HappyAbsSyn14 (Type)
	| HappyAbsSyn15 (C Type)
	| HappyAbsSyn18 (Phrase)
	| HappyAbsSyn19 (Word)
	| HappyAbsSyn20 (BuiltIn)
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23
	| HappyAbsSyn24 t24
	| HappyAbsSyn25 t25

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,167) ([0,0,384,0,0,65280,8207,48,0,0,96,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6,0,0,4,0,0,0,16384,0,0,0,0,0,0,32768,2047,6160,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8190,24768,0,0,4096,0,0,0,2048,0,0,0,0,0,0,0,512,1024,0,0,0,0,0,0,0,128,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1025,0,0,128,0,0,0,0,0,0,0,0,256,0,0,8,0,0,0,2048,32,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,31780,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,256,0,0,0,2050,248,0,0,0,0,0,0,0,0,0,16384,256,31,0,0,0,0,0,4096,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,65280,8207,48,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,18434,248,0,0,1,0,0,32768,512,62,0,0,0,0,0,0,1024,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,256,31748,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4100,496,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pModule","%start_pPhrase","module","block","define_block","data_block","data_clauses","data_clauses_extra","scheme","arrow","stack","type_atom","type_prim","type_func","type_expr","phrase","word","builtin","listOf__block__","listOf__char__","listOf__data_clauses_extra__","listOf__tvar__","listOf__type_atom__","atom","'id'","'swap'","'apply'","'quote'","'compose'","'repeat'","'int_plus'","'int_times'","'string_concat'","custom_builtin","int","char","func","'@define'","'@data'","'@end'","'::'","'=='","'->'","'$'","'['","']'","'('","')'","'='","'|'","'.'","'\\''","'\"'","'forall'","'Int'","'Char'","'String'","'@Eff'","tvar","svar","%eof"]
        bit_start = st * 63
        bit_end = (st + 1) * 63
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..62]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (40) = happyShift action_7
action_0 (41) = happyShift action_8
action_0 (5) = happyGoto action_27
action_0 (6) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 (8) = happyGoto action_5
action_0 (21) = happyGoto action_6
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (26) = happyShift action_12
action_1 (27) = happyShift action_13
action_1 (28) = happyShift action_14
action_1 (29) = happyShift action_15
action_1 (30) = happyShift action_16
action_1 (31) = happyShift action_17
action_1 (32) = happyShift action_18
action_1 (33) = happyShift action_19
action_1 (34) = happyShift action_20
action_1 (35) = happyShift action_21
action_1 (36) = happyShift action_22
action_1 (37) = happyShift action_23
action_1 (47) = happyShift action_24
action_1 (54) = happyShift action_25
action_1 (55) = happyShift action_26
action_1 (18) = happyGoto action_9
action_1 (19) = happyGoto action_10
action_1 (20) = happyGoto action_11
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (40) = happyShift action_7
action_2 (41) = happyShift action_8
action_2 (6) = happyGoto action_3
action_2 (7) = happyGoto action_4
action_2 (8) = happyGoto action_5
action_2 (21) = happyGoto action_6
action_2 _ = happyFail (happyExpListPerState 2)

action_3 _ = happyReduce_47

action_4 _ = happyReduce_3

action_5 _ = happyReduce_4

action_6 (40) = happyShift action_7
action_6 (41) = happyShift action_8
action_6 (6) = happyGoto action_36
action_6 (7) = happyGoto action_4
action_6 (8) = happyGoto action_5
action_6 _ = happyReduce_2

action_7 (26) = happyShift action_35
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (39) = happyShift action_34
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (63) = happyAccept
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (26) = happyShift action_12
action_10 (27) = happyShift action_13
action_10 (28) = happyShift action_14
action_10 (29) = happyShift action_15
action_10 (30) = happyShift action_16
action_10 (31) = happyShift action_17
action_10 (32) = happyShift action_18
action_10 (33) = happyShift action_19
action_10 (34) = happyShift action_20
action_10 (35) = happyShift action_21
action_10 (36) = happyShift action_22
action_10 (37) = happyShift action_23
action_10 (47) = happyShift action_24
action_10 (54) = happyShift action_25
action_10 (55) = happyShift action_26
action_10 (18) = happyGoto action_33
action_10 (19) = happyGoto action_10
action_10 (20) = happyGoto action_11
action_10 _ = happyReduce_28

action_11 _ = happyReduce_31

action_12 _ = happyReduce_30

action_13 _ = happyReduce_40

action_14 _ = happyReduce_41

action_15 _ = happyReduce_42

action_16 _ = happyReduce_43

action_17 _ = happyReduce_44

action_18 _ = happyReduce_45

action_19 _ = happyReduce_37

action_20 _ = happyReduce_38

action_21 _ = happyReduce_39

action_22 _ = happyReduce_46

action_23 _ = happyReduce_34

action_24 (26) = happyShift action_12
action_24 (27) = happyShift action_13
action_24 (28) = happyShift action_14
action_24 (29) = happyShift action_15
action_24 (30) = happyShift action_16
action_24 (31) = happyShift action_17
action_24 (32) = happyShift action_18
action_24 (33) = happyShift action_19
action_24 (34) = happyShift action_20
action_24 (35) = happyShift action_21
action_24 (36) = happyShift action_22
action_24 (37) = happyShift action_23
action_24 (47) = happyShift action_24
action_24 (48) = happyShift action_32
action_24 (54) = happyShift action_25
action_24 (55) = happyShift action_26
action_24 (18) = happyGoto action_31
action_24 (19) = happyGoto action_10
action_24 (20) = happyGoto action_11
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (38) = happyShift action_30
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (38) = happyShift action_29
action_26 (22) = happyGoto action_28
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (63) = happyAccept
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (38) = happyShift action_44
action_28 (55) = happyShift action_45
action_28 _ = happyFail (happyExpListPerState 28)

action_29 _ = happyReduce_49

action_30 (54) = happyShift action_43
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (48) = happyShift action_42
action_31 _ = happyFail (happyExpListPerState 31)

action_32 _ = happyReduce_32

action_33 _ = happyReduce_29

action_34 (51) = happyShift action_40
action_34 (61) = happyShift action_41
action_34 (9) = happyGoto action_38
action_34 (24) = happyGoto action_39
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (43) = happyShift action_37
action_35 _ = happyFail (happyExpListPerState 35)

action_36 _ = happyReduce_48

action_37 (62) = happyShift action_53
action_37 (11) = happyGoto action_50
action_37 (12) = happyGoto action_51
action_37 (13) = happyGoto action_52
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (42) = happyShift action_49
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (51) = happyShift action_40
action_39 (61) = happyShift action_48
action_39 (9) = happyGoto action_47
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (39) = happyShift action_46
action_40 _ = happyFail (happyExpListPerState 40)

action_41 _ = happyReduce_53

action_42 _ = happyReduce_33

action_43 _ = happyReduce_35

action_44 _ = happyReduce_50

action_45 _ = happyReduce_36

action_46 (39) = happyShift action_57
action_46 (49) = happyShift action_58
action_46 (52) = happyShift action_70
action_46 (57) = happyShift action_59
action_46 (58) = happyShift action_60
action_46 (59) = happyShift action_61
action_46 (60) = happyShift action_62
action_46 (61) = happyShift action_63
action_46 (10) = happyGoto action_67
action_46 (14) = happyGoto action_54
action_46 (15) = happyGoto action_55
action_46 (23) = happyGoto action_68
action_46 (25) = happyGoto action_69
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (42) = happyShift action_66
action_47 _ = happyFail (happyExpListPerState 47)

action_48 _ = happyReduce_54

action_49 _ = happyReduce_6

action_50 (44) = happyShift action_65
action_50 _ = happyFail (happyExpListPerState 50)

action_51 _ = happyReduce_12

action_52 (45) = happyShift action_64
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (39) = happyShift action_57
action_53 (49) = happyShift action_58
action_53 (57) = happyShift action_59
action_53 (58) = happyShift action_60
action_53 (59) = happyShift action_61
action_53 (60) = happyShift action_62
action_53 (61) = happyShift action_63
action_53 (14) = happyGoto action_54
action_53 (15) = happyGoto action_55
action_53 (25) = happyGoto action_56
action_53 _ = happyReduce_14

action_54 _ = happyReduce_55

action_55 _ = happyReduce_17

action_56 (39) = happyShift action_57
action_56 (49) = happyShift action_58
action_56 (57) = happyShift action_59
action_56 (58) = happyShift action_60
action_56 (59) = happyShift action_61
action_56 (60) = happyShift action_62
action_56 (61) = happyShift action_63
action_56 (14) = happyGoto action_72
action_56 (15) = happyGoto action_55
action_56 _ = happyReduce_15

action_57 _ = happyReduce_18

action_58 (39) = happyShift action_80
action_58 (62) = happyShift action_53
action_58 (12) = happyGoto action_77
action_58 (13) = happyGoto action_52
action_58 (16) = happyGoto action_78
action_58 (17) = happyGoto action_79
action_58 _ = happyFail (happyExpListPerState 58)

action_59 _ = happyReduce_20

action_60 _ = happyReduce_21

action_61 _ = happyReduce_22

action_62 _ = happyReduce_23

action_63 _ = happyReduce_16

action_64 (62) = happyShift action_53
action_64 (13) = happyGoto action_76
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (26) = happyShift action_12
action_65 (27) = happyShift action_13
action_65 (28) = happyShift action_14
action_65 (29) = happyShift action_15
action_65 (30) = happyShift action_16
action_65 (31) = happyShift action_17
action_65 (32) = happyShift action_18
action_65 (33) = happyShift action_19
action_65 (34) = happyShift action_20
action_65 (35) = happyShift action_21
action_65 (36) = happyShift action_22
action_65 (37) = happyShift action_23
action_65 (47) = happyShift action_24
action_65 (54) = happyShift action_25
action_65 (55) = happyShift action_26
action_65 (18) = happyGoto action_75
action_65 (19) = happyGoto action_10
action_65 (20) = happyGoto action_11
action_65 _ = happyFail (happyExpListPerState 65)

action_66 _ = happyReduce_7

action_67 _ = happyReduce_51

action_68 (52) = happyShift action_70
action_68 (10) = happyGoto action_74
action_68 _ = happyReduce_8

action_69 (39) = happyShift action_57
action_69 (49) = happyShift action_58
action_69 (52) = happyShift action_70
action_69 (57) = happyShift action_59
action_69 (58) = happyShift action_60
action_69 (59) = happyShift action_61
action_69 (60) = happyShift action_62
action_69 (61) = happyShift action_63
action_69 (10) = happyGoto action_67
action_69 (14) = happyGoto action_72
action_69 (15) = happyGoto action_55
action_69 (23) = happyGoto action_73
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (39) = happyShift action_71
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (39) = happyShift action_57
action_71 (49) = happyShift action_58
action_71 (57) = happyShift action_59
action_71 (58) = happyShift action_60
action_71 (59) = happyShift action_61
action_71 (60) = happyShift action_62
action_71 (61) = happyShift action_63
action_71 (14) = happyGoto action_54
action_71 (15) = happyGoto action_55
action_71 (25) = happyGoto action_84
action_71 _ = happyReduce_10

action_72 _ = happyReduce_56

action_73 (52) = happyShift action_70
action_73 (10) = happyGoto action_74
action_73 _ = happyReduce_9

action_74 _ = happyReduce_52

action_75 (42) = happyShift action_83
action_75 _ = happyFail (happyExpListPerState 75)

action_76 _ = happyReduce_13

action_77 _ = happyReduce_27

action_78 (39) = happyShift action_57
action_78 (49) = happyShift action_58
action_78 (57) = happyShift action_59
action_78 (58) = happyShift action_60
action_78 (59) = happyShift action_61
action_78 (60) = happyShift action_62
action_78 (61) = happyShift action_63
action_78 (14) = happyGoto action_82
action_78 (15) = happyGoto action_55
action_78 _ = happyReduce_26

action_79 (50) = happyShift action_81
action_79 _ = happyFail (happyExpListPerState 79)

action_80 _ = happyReduce_24

action_81 _ = happyReduce_19

action_82 _ = happyReduce_25

action_83 _ = happyReduce_5

action_84 (39) = happyShift action_57
action_84 (49) = happyShift action_58
action_84 (57) = happyShift action_59
action_84 (58) = happyShift action_60
action_84 (59) = happyShift action_61
action_84 (60) = happyShift action_62
action_84 (61) = happyShift action_63
action_84 (14) = happyGoto action_72
action_84 (15) = happyGoto action_55
action_84 _ = happyReduce_11

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn5
		 (Module happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  6 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happyReduce 7 7 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenWord happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Definition (Atom (fst happy_var_2)) happy_var_6 happy_var_4
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 4 8 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	(HappyTerminal (TokenConst happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (DeclareData (fst happy_var_2, []) happy_var_3
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 5 8 happyReduction_7
happyReduction_7 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	(HappyAbsSyn24  happy_var_3) `HappyStk`
	(HappyTerminal (TokenConst happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (DeclareData (fst happy_var_2, map (V . fst) happy_var_3) happy_var_4
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_3  9 happyReduction_8
happyReduction_8 (HappyAbsSyn23  happy_var_3)
	(HappyTerminal (TokenConst happy_var_2))
	_
	 =  HappyAbsSyn9
		 ((Atom $ fst happy_var_2, []) : happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happyReduce 4 9 happyReduction_9
happyReduction_9 ((HappyAbsSyn23  happy_var_4) `HappyStk`
	(HappyAbsSyn25  happy_var_3) `HappyStk`
	(HappyTerminal (TokenConst happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 ((Atom $ fst happy_var_2, happy_var_3) : happy_var_4
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_2  10 happyReduction_10
happyReduction_10 (HappyTerminal (TokenConst happy_var_2))
	_
	 =  HappyAbsSyn10
		 ((Atom $ fst happy_var_2, [])
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  10 happyReduction_11
happyReduction_11 (HappyAbsSyn25  happy_var_3)
	(HappyTerminal (TokenConst happy_var_2))
	_
	 =  HappyAbsSyn10
		 ((Atom $ fst happy_var_2, happy_var_3)
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  11 happyReduction_12
happyReduction_12 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (quantify happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  12 happyReduction_13
happyReduction_13 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 (Arrow happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  13 happyReduction_14
happyReduction_14 (HappyTerminal (TokenVarS happy_var_1))
	 =  HappyAbsSyn13
		 (Stack (V (tail $ fst happy_var_1)) []
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_2  13 happyReduction_15
happyReduction_15 (HappyAbsSyn25  happy_var_2)
	(HappyTerminal (TokenVarS happy_var_1))
	 =  HappyAbsSyn13
		 (Stack (V (tail $ fst happy_var_1)) (reverse happy_var_2)
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  14 happyReduction_16
happyReduction_16 (HappyTerminal (TokenVarT happy_var_1))
	 =  HappyAbsSyn14
		 (TyVar $ V $ fst happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  14 happyReduction_17
happyReduction_17 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 (TyCon happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  14 happyReduction_18
happyReduction_18 (HappyTerminal (TokenConst happy_var_1))
	 =  HappyAbsSyn14
		 (TyCon $ C $ fst happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  14 happyReduction_19
happyReduction_19 _
	(HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (happy_var_2
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  15 happyReduction_20
happyReduction_20 _
	 =  HappyAbsSyn15
		 (C "Int"
	)

happyReduce_21 = happySpecReduce_1  15 happyReduction_21
happyReduction_21 _
	 =  HappyAbsSyn15
		 (C "Char"
	)

happyReduce_22 = happySpecReduce_1  15 happyReduction_22
happyReduction_22 _
	 =  HappyAbsSyn15
		 (C "String"
	)

happyReduce_23 = happySpecReduce_1  15 happyReduction_23
happyReduction_23 _
	 =  HappyAbsSyn15
		 (C "@Eff"
	)

happyReduce_24 = happySpecReduce_1  16 happyReduction_24
happyReduction_24 (HappyTerminal (TokenConst happy_var_1))
	 =  HappyAbsSyn14
		 (TyCon $ C $ fst happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_2  16 happyReduction_25
happyReduction_25 (HappyAbsSyn14  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (TyApp happy_var_1 happy_var_2
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  17 happyReduction_26
happyReduction_26 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  17 happyReduction_27
happyReduction_27 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn14
		 (TyArr happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  18 happyReduction_28
happyReduction_28 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 (Then happy_var_1 Silence
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_2  18 happyReduction_29
happyReduction_29 (HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 (Then happy_var_1 happy_var_2
	)
happyReduction_29 _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  19 happyReduction_30
happyReduction_30 (HappyTerminal (TokenWord happy_var_1))
	 =  HappyAbsSyn19
		 (Only (Atom (fst happy_var_1))
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  19 happyReduction_31
happyReduction_31 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 (BuiltIn happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_2  19 happyReduction_32
happyReduction_32 _
	_
	 =  HappyAbsSyn19
		 (Noop
	)

happyReduce_33 = happySpecReduce_3  19 happyReduction_33
happyReduction_33 _
	(HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (Quote happy_var_2
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  20 happyReduction_34
happyReduction_34 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn20
		 (BuiltIn_Int (read (fst happy_var_1))
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  20 happyReduction_35
happyReduction_35 _
	(HappyTerminal (TokenChar happy_var_2))
	_
	 =  HappyAbsSyn20
		 (BuiltIn_Char (readStrChr (fst happy_var_2))
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  20 happyReduction_36
happyReduction_36 _
	(HappyAbsSyn22  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (BuiltIn_String (reverse $ map (readStrChr . fst) happy_var_2)
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  20 happyReduction_37
happyReduction_37 _
	 =  HappyAbsSyn20
		 (BuiltIn_Int_Plus
	)

happyReduce_38 = happySpecReduce_1  20 happyReduction_38
happyReduction_38 _
	 =  HappyAbsSyn20
		 (BuiltIn_Int_Times
	)

happyReduce_39 = happySpecReduce_1  20 happyReduction_39
happyReduction_39 _
	 =  HappyAbsSyn20
		 (BuiltIn_String_Concat
	)

happyReduce_40 = happySpecReduce_1  20 happyReduction_40
happyReduction_40 _
	 =  HappyAbsSyn20
		 (BuiltIn_Id
	)

happyReduce_41 = happySpecReduce_1  20 happyReduction_41
happyReduction_41 _
	 =  HappyAbsSyn20
		 (BuiltIn_Swap
	)

happyReduce_42 = happySpecReduce_1  20 happyReduction_42
happyReduction_42 _
	 =  HappyAbsSyn20
		 (BuiltIn_Apply
	)

happyReduce_43 = happySpecReduce_1  20 happyReduction_43
happyReduction_43 _
	 =  HappyAbsSyn20
		 (BuiltIn_Quote
	)

happyReduce_44 = happySpecReduce_1  20 happyReduction_44
happyReduction_44 _
	 =  HappyAbsSyn20
		 (BuiltIn_Compose
	)

happyReduce_45 = happySpecReduce_1  20 happyReduction_45
happyReduction_45 _
	 =  HappyAbsSyn20
		 (BuiltIn_Repeat
	)

happyReduce_46 = happySpecReduce_1  20 happyReduction_46
happyReduction_46 (HappyTerminal (TokenBuiltIn happy_var_1))
	 =  HappyAbsSyn20
		 (BuiltIn_Ext (fst happy_var_1)
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  21 happyReduction_47
happyReduction_47 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn21
		 ([happy_var_1]
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_2  21 happyReduction_48
happyReduction_48 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 ((happy_var_2:happy_var_1)
	)
happyReduction_48 _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  22 happyReduction_49
happyReduction_49 (HappyTerminal (TokenChar happy_var_1))
	 =  HappyAbsSyn22
		 ([happy_var_1]
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_2  22 happyReduction_50
happyReduction_50 (HappyTerminal (TokenChar happy_var_2))
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 ((happy_var_2:happy_var_1)
	)
happyReduction_50 _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  23 happyReduction_51
happyReduction_51 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn23
		 ([happy_var_1]
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_2  23 happyReduction_52
happyReduction_52 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 ((happy_var_2:happy_var_1)
	)
happyReduction_52 _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_1  24 happyReduction_53
happyReduction_53 (HappyTerminal (TokenVarT happy_var_1))
	 =  HappyAbsSyn24
		 ([happy_var_1]
	)
happyReduction_53 _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_2  24 happyReduction_54
happyReduction_54 (HappyTerminal (TokenVarT happy_var_2))
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 ((happy_var_2:happy_var_1)
	)
happyReduction_54 _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  25 happyReduction_55
happyReduction_55 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn25
		 ([happy_var_1]
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_2  25 happyReduction_56
happyReduction_56 (HappyAbsSyn14  happy_var_2)
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn25
		 ((happy_var_2:happy_var_1)
	)
happyReduction_56 _ _  = notHappyAtAll 

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	EOF -> action 63 63 tk (HappyState action) sts stk;
	TokenWord happy_dollar_dollar -> cont 26;
	TokenBuiltIn ("#id", happy_dollar_dollar) -> cont 27;
	TokenBuiltIn ("#swap", happy_dollar_dollar) -> cont 28;
	TokenBuiltIn ("#apply", happy_dollar_dollar) -> cont 29;
	TokenBuiltIn ("#quote", happy_dollar_dollar) -> cont 30;
	TokenBuiltIn ("#compose", happy_dollar_dollar) -> cont 31;
	TokenBuiltIn ("#repeat", happy_dollar_dollar) -> cont 32;
	TokenBuiltIn ("#int_plus", happy_dollar_dollar) -> cont 33;
	TokenBuiltIn ("#int_times", happy_dollar_dollar) -> cont 34;
	TokenBuiltIn ("#string_concat", happy_dollar_dollar) -> cont 35;
	TokenBuiltIn happy_dollar_dollar -> cont 36;
	TokenInt happy_dollar_dollar -> cont 37;
	TokenChar happy_dollar_dollar -> cont 38;
	TokenConst happy_dollar_dollar -> cont 39;
	TokenKeyword Lex.KW_Define happy_dollar_dollar -> cont 40;
	TokenKeyword Lex.KW_Data happy_dollar_dollar -> cont 41;
	TokenKeyword Lex.KW_End happy_dollar_dollar -> cont 42;
	TokenSymbol Lex.SY_DoubleColon happy_dollar_dollar -> cont 43;
	TokenSymbol Lex.SY_DoubleEqual happy_dollar_dollar -> cont 44;
	TokenSymbol Lex.SY_MinusGreater happy_dollar_dollar -> cont 45;
	TokenSymbol Lex.SY_Dollar happy_dollar_dollar -> cont 46;
	TokenSymbol Lex.SY_OpenBrack happy_dollar_dollar -> cont 47;
	TokenSymbol Lex.SY_ClosedBrack happy_dollar_dollar -> cont 48;
	TokenSymbol Lex.SY_OpenParen happy_dollar_dollar -> cont 49;
	TokenSymbol Lex.SY_ClosedParen happy_dollar_dollar -> cont 50;
	TokenSymbol Lex.SY_Equal happy_dollar_dollar -> cont 51;
	TokenSymbol Lex.SY_Pipe happy_dollar_dollar -> cont 52;
	TokenSymbol Lex.SY_Dot happy_dollar_dollar -> cont 53;
	TokenSymbol Lex.SY_SingleQuote happy_dollar_dollar -> cont 54;
	TokenSymbol Lex.SY_DoubleQuote happy_dollar_dollar -> cont 55;
	TokenSymbol Lex.SY_ForAll happy_dollar_dollar -> cont 56;
	TokenTypeConst "Int" happy_dollar_dollar -> cont 57;
	TokenTypeConst "Char" happy_dollar_dollar -> cont 58;
	TokenTypeConst "String" happy_dollar_dollar -> cont 59;
	TokenEff happy_dollar_dollar -> cont 60;
	TokenVarT happy_dollar_dollar -> cont 61;
	TokenVarS happy_dollar_dollar -> cont 62;
	_ -> happyError' (tk, [])
	})

happyError_ explist 63 tk = happyError' (tk, explist)
happyError_ explist _ tk = happyError' (tk, explist)

happyThen :: () => Parser a -> (a -> Parser b) -> Parser b
happyThen = (>>=)
happyReturn :: () => a -> Parser a
happyReturn = (return)
happyThen1 :: () => Parser a -> (a -> Parser b) -> Parser b
happyThen1 = happyThen
happyReturn1 :: () => a -> Parser a
happyReturn1 = happyReturn
happyError' :: () => ((Token), [String]) -> Parser a
happyError' tk = (\(tokens, _) -> parseError tokens) tk
pModule = happySomeParser where
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn5 z -> happyReturn z; _other -> notHappyAtAll })

pPhrase = happySomeParser where
 happySomeParser = happyThen (happyParse action_1) (\x -> case x of {HappyAbsSyn18 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: Token -> Parser a
parseError = Parser . return . Left
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 15 "<built-in>" #-}
{-# LINE 1 "/Users/nathan/.stack/programs/x86_64-osx/ghc-8.6.4/lib/ghc-8.6.4/include/ghcversion.h" #-}
















{-# LINE 16 "<built-in>" #-}
{-# LINE 1 "/var/folders/td/sxyy9wl919740vddr49g8nth0000gn/T/ghc2783_0/ghc_2.h" #-}

































































































































































































{-# LINE 17 "<built-in>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 










{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList








{-# LINE 65 "templates/GenericTemplate.hs" #-}


{-# LINE 75 "templates/GenericTemplate.hs" #-}










infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action


{-# LINE 137 "templates/GenericTemplate.hs" #-}


{-# LINE 147 "templates/GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

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

