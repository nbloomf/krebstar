{-# OPTIONS_GHC -w #-}
-- Work around https://github.com/simonmar/happy/issues/109
-- #undef __GLASGOW_HASKELL__
-- #define __GLASGOW_HASKELL__ 709

module Lang.Data.Parser where

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Lang.Data.Lexer
import qualified Lang.Data.LexicalGrammar as Lex
import Lang.Data.AST
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.9

data HappyAbsSyn t19 t20
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn9 (AST)
	| HappyAbsSyn10 ([Block])
	| HappyAbsSyn11 (Block)
	| HappyAbsSyn13 (Arrow_)
	| HappyAbsSyn14 (Stack_)
	| HappyAbsSyn15 (MonoType_)
	| HappyAbsSyn16 (Primitive_)
	| HappyAbsSyn17 (AtomWord_)
	| HappyAbsSyn18 (Expr_)
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,84) ([0,64,0,2048,0,0,32,0,1024,0,256,1,16384,7808,0,16,0,0,0,0,0,0,0,0,256,0,2048,0,0,0,0,0,0,0,0,0,65,0,0,0,0,0,0,0,0,0,0,256,1,0,0,0,0,0,32896,0,0,0,0,59396,1,0,0,0,2,0,0,0,0,0,0,2,0,0,0,16416,15,0,4,0,0,0,16384,0,512,244,0,2,0,0,0,16384,0,8192,4032,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,32768,128,0,128,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pAST","%start_pDefineBlock","%start_pArrow","%start_pStack","%start_pExpr","%start_pType","ast","blocks","block","define_block","arrow","stack","type","prim","word","expr","listOf__block__","listOf__type__","atom","func","'@define'","'@end'","'::'","'=='","'->'","'$'","'['","']'","'('","')'","'Int'","'Char'","'String'","tvar","%eof"]
        bit_start = st * 37
        bit_end = (st + 1) * 37
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..36]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (23) = happyShift action_11
action_0 (9) = happyGoto action_29
action_0 (10) = happyGoto action_7
action_0 (11) = happyGoto action_8
action_0 (12) = happyGoto action_9
action_0 (19) = happyGoto action_10
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (23) = happyShift action_11
action_1 (12) = happyGoto action_28
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (28) = happyShift action_25
action_2 (13) = happyGoto action_26
action_2 (14) = happyGoto action_27
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (28) = happyShift action_25
action_3 (14) = happyGoto action_24
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (21) = happyShift action_22
action_4 (29) = happyShift action_23
action_4 (17) = happyGoto action_20
action_4 (18) = happyGoto action_21
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (22) = happyShift action_14
action_5 (31) = happyShift action_15
action_5 (33) = happyShift action_16
action_5 (34) = happyShift action_17
action_5 (35) = happyShift action_18
action_5 (36) = happyShift action_19
action_5 (15) = happyGoto action_12
action_5 (16) = happyGoto action_13
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (23) = happyShift action_11
action_6 (10) = happyGoto action_7
action_6 (11) = happyGoto action_8
action_6 (12) = happyGoto action_9
action_6 (19) = happyGoto action_10
action_6 _ = happyFail (happyExpListPerState 6)

action_7 _ = happyReduce_6

action_8 _ = happyReduce_25

action_9 _ = happyReduce_8

action_10 (23) = happyShift action_11
action_10 (11) = happyGoto action_38
action_10 (12) = happyGoto action_9
action_10 _ = happyReduce_7

action_11 (21) = happyShift action_37
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (37) = happyAccept
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_14

action_14 _ = happyReduce_15

action_15 (22) = happyShift action_36
action_15 (28) = happyShift action_25
action_15 (13) = happyGoto action_35
action_15 (14) = happyGoto action_27
action_15 _ = happyFail (happyExpListPerState 15)

action_16 _ = happyReduce_18

action_17 _ = happyReduce_19

action_18 _ = happyReduce_20

action_19 _ = happyReduce_13

action_20 (21) = happyShift action_22
action_20 (29) = happyShift action_23
action_20 (17) = happyGoto action_20
action_20 (18) = happyGoto action_34
action_20 _ = happyReduce_23

action_21 (37) = happyAccept
action_21 _ = happyFail (happyExpListPerState 21)

action_22 _ = happyReduce_21

action_23 (21) = happyShift action_22
action_23 (29) = happyShift action_23
action_23 (17) = happyGoto action_20
action_23 (18) = happyGoto action_33
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (37) = happyAccept
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (22) = happyShift action_14
action_25 (31) = happyShift action_15
action_25 (33) = happyShift action_16
action_25 (34) = happyShift action_17
action_25 (35) = happyShift action_18
action_25 (36) = happyShift action_19
action_25 (15) = happyGoto action_31
action_25 (16) = happyGoto action_13
action_25 (20) = happyGoto action_32
action_25 _ = happyReduce_11

action_26 (37) = happyAccept
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (27) = happyShift action_30
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (37) = happyAccept
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (37) = happyAccept
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (28) = happyShift action_25
action_30 (14) = happyGoto action_44
action_30 _ = happyFail (happyExpListPerState 30)

action_31 _ = happyReduce_27

action_32 (22) = happyShift action_14
action_32 (31) = happyShift action_15
action_32 (33) = happyShift action_16
action_32 (34) = happyShift action_17
action_32 (35) = happyShift action_18
action_32 (36) = happyShift action_19
action_32 (15) = happyGoto action_43
action_32 (16) = happyGoto action_13
action_32 _ = happyReduce_12

action_33 (30) = happyShift action_42
action_33 _ = happyFail (happyExpListPerState 33)

action_34 _ = happyReduce_24

action_35 (32) = happyShift action_41
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (22) = happyShift action_14
action_36 (31) = happyShift action_15
action_36 (33) = happyShift action_16
action_36 (34) = happyShift action_17
action_36 (35) = happyShift action_18
action_36 (36) = happyShift action_19
action_36 (15) = happyGoto action_31
action_36 (16) = happyGoto action_13
action_36 (20) = happyGoto action_40
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (25) = happyShift action_39
action_37 _ = happyFail (happyExpListPerState 37)

action_38 _ = happyReduce_26

action_39 (28) = happyShift action_25
action_39 (13) = happyGoto action_46
action_39 (14) = happyGoto action_27
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (22) = happyShift action_14
action_40 (31) = happyShift action_15
action_40 (32) = happyShift action_45
action_40 (33) = happyShift action_16
action_40 (34) = happyShift action_17
action_40 (35) = happyShift action_18
action_40 (36) = happyShift action_19
action_40 (15) = happyGoto action_43
action_40 (16) = happyGoto action_13
action_40 _ = happyFail (happyExpListPerState 40)

action_41 _ = happyReduce_17

action_42 _ = happyReduce_22

action_43 _ = happyReduce_28

action_44 _ = happyReduce_10

action_45 _ = happyReduce_16

action_46 (26) = happyShift action_47
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (21) = happyShift action_22
action_47 (29) = happyShift action_23
action_47 (17) = happyGoto action_20
action_47 (18) = happyGoto action_48
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (24) = happyShift action_49
action_48 _ = happyFail (happyExpListPerState 48)

action_49 _ = happyReduce_9

happyReduce_6 = happySpecReduce_1  9 happyReduction_6
happyReduction_6 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (AST happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  10 happyReduction_7
happyReduction_7 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  11 happyReduction_8
happyReduction_8 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happyReduce 7 12 happyReduction_9
happyReduction_9 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenWord happy_var_2)) `HappyStk`
	(HappyTerminal (TokenKeyword Lex.KW_Define happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (Def_Block happy_var_1 (fst happy_var_2) happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_3  13 happyReduction_10
happyReduction_10 (HappyAbsSyn14  happy_var_3)
	(HappyTerminal (TokenSymbol Lex.SY_MinusGreater happy_var_2))
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (Arrow_ happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  14 happyReduction_11
happyReduction_11 (HappyTerminal (TokenSymbol Lex.SY_Dollar happy_var_1))
	 =  HappyAbsSyn14
		 (Cons_ happy_var_1 []
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  14 happyReduction_12
happyReduction_12 (HappyAbsSyn20  happy_var_2)
	(HappyTerminal (TokenSymbol Lex.SY_Dollar happy_var_1))
	 =  HappyAbsSyn14
		 (Cons_ happy_var_1 happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  15 happyReduction_13
happyReduction_13 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn15
		 (Var_ happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  15 happyReduction_14
happyReduction_14 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 (Prim_ happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  15 happyReduction_15
happyReduction_15 (HappyTerminal (TokenConst happy_var_1))
	 =  HappyAbsSyn15
		 (Func_ happy_var_1 []
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happyReduce 4 15 happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	(HappyTerminal (TokenConst happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (Func_ happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_3  15 happyReduction_17
happyReduction_17 _
	(HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (Quote_ happy_var_2
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  16 happyReduction_18
happyReduction_18 _
	 =  HappyAbsSyn16
		 (Int_
	)

happyReduce_19 = happySpecReduce_1  16 happyReduction_19
happyReduction_19 _
	 =  HappyAbsSyn16
		 (Char_
	)

happyReduce_20 = happySpecReduce_1  16 happyReduction_20
happyReduction_20 _
	 =  HappyAbsSyn16
		 (String_
	)

happyReduce_21 = happySpecReduce_1  17 happyReduction_21
happyReduction_21 (HappyTerminal (TokenWord happy_var_1))
	 =  HappyAbsSyn17
		 (AtomWord_ happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  17 happyReduction_22
happyReduction_22 _
	(HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn17
		 (QuoteWord_ happy_var_2
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  18 happyReduction_23
happyReduction_23 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn18
		 (OnlyW_ happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_2  18 happyReduction_24
happyReduction_24 (HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn18
		 (ConsW_ happy_var_1 happy_var_2
	)
happyReduction_24 _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  19 happyReduction_25
happyReduction_25 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn19
		 ([happy_var_1]
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_2  19 happyReduction_26
happyReduction_26 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 ((happy_var_2:happy_var_1)
	)
happyReduction_26 _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  20 happyReduction_27
happyReduction_27 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn20
		 ([happy_var_1]
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_2  20 happyReduction_28
happyReduction_28 (HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 ((happy_var_2:happy_var_1)
	)
happyReduction_28 _ _  = notHappyAtAll 

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	EOF -> action 37 37 tk (HappyState action) sts stk;
	TokenWord happy_dollar_dollar -> cont 21;
	TokenConst happy_dollar_dollar -> cont 22;
	TokenKeyword Lex.KW_Define happy_dollar_dollar -> cont 23;
	TokenKeyword Lex.KW_End happy_dollar_dollar -> cont 24;
	TokenSymbol Lex.SY_DoubleColon happy_dollar_dollar -> cont 25;
	TokenSymbol Lex.SY_DoubleEqual happy_dollar_dollar -> cont 26;
	TokenSymbol Lex.SY_MinusGreater happy_dollar_dollar -> cont 27;
	TokenSymbol Lex.SY_Dollar happy_dollar_dollar -> cont 28;
	TokenSymbol Lex.SY_OpenBrack happy_dollar_dollar -> cont 29;
	TokenSymbol Lex.SY_ClosedBrack happy_dollar_dollar -> cont 30;
	TokenSymbol Lex.SY_OpenParen happy_dollar_dollar -> cont 31;
	TokenSymbol Lex.SY_ClosedParen happy_dollar_dollar -> cont 32;
	TokenTypeConst "Int" happy_dollar_dollar -> cont 33;
	TokenTypeConst "Char" happy_dollar_dollar -> cont 34;
	TokenTypeConst "String" happy_dollar_dollar -> cont 35;
	TokenVar happy_dollar_dollar -> cont 36;
	_ -> happyError' (tk, [])
	})

happyError_ explist 37 tk = happyError' (tk, explist)
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
pAST = happySomeParser where
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn9 z -> happyReturn z; _other -> notHappyAtAll })

pDefineBlock = happySomeParser where
 happySomeParser = happyThen (happyParse action_1) (\x -> case x of {HappyAbsSyn11 z -> happyReturn z; _other -> notHappyAtAll })

pArrow = happySomeParser where
 happySomeParser = happyThen (happyParse action_2) (\x -> case x of {HappyAbsSyn13 z -> happyReturn z; _other -> notHappyAtAll })

pStack = happySomeParser where
 happySomeParser = happyThen (happyParse action_3) (\x -> case x of {HappyAbsSyn14 z -> happyReturn z; _other -> notHappyAtAll })

pExpr = happySomeParser where
 happySomeParser = happyThen (happyParse action_4) (\x -> case x of {HappyAbsSyn18 z -> happyReturn z; _other -> notHappyAtAll })

pType = happySomeParser where
 happySomeParser = happyThen (happyParse action_5) (\x -> case x of {HappyAbsSyn15 z -> happyReturn z; _other -> notHappyAtAll })

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

