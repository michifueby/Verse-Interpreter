{-# OPTIONS_GHC -w #-}
module Util.Parsing.Grammars.Grammar where
import Util.Interface.AbstractSyntaxTree as Ast
import Util.Interface.Token.Token
import Util.Datatypes.Types as T
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 (VerseExp)
	| HappyAbsSyn7 (T.Type)
	| HappyAbsSyn8 ([T.Type])

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42 :: () => Prelude.Int -> ({-HappyReduction (HappyIdentity) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,97) ([28672,1025,0,1088,16,0,24,0,32768,0,0,0,0,0,0,0,256,0,0,68,1,0,0,0,0,0,0,0,0,8192,0,0,8192,0,0,0,0,5376,64,0,32,2,32768,2049,0,16384,32768,3,16388,0,0,0,0,272,4,16384,4100,0,4352,64,0,128,0,0,2,0,0,0,0,0,0,4096,256,0,0,0,16384,4101,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,56,128,0,0,6,0,0,4,0,0,2,0,0,0,0,32768,3,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseExp","parseExp","initialization","declaration","type","types","binOpStr","binOp","term","factor","num","name","str","'+'","'-'","'*'","'<'","'>'","'='","':'","','","'|'","'?'","';'","'('","')'","'['","']'","'}'","'{'","if","then","else","for","do","Tstring","Tint","Ttuple","false","%eof"]
        bit_start = st Prelude.* 42
        bit_end = (st Prelude.+ 1) Prelude.* 42
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..41]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (13) = happyShift action_5
action_0 (14) = happyShift action_12
action_0 (15) = happyShift action_13
action_0 (17) = happyShift action_6
action_0 (27) = happyShift action_14
action_0 (4) = happyGoto action_8
action_0 (5) = happyGoto action_9
action_0 (6) = happyGoto action_10
action_0 (9) = happyGoto action_11
action_0 (10) = happyGoto action_2
action_0 (11) = happyGoto action_3
action_0 (12) = happyGoto action_4
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (13) = happyShift action_5
action_1 (17) = happyShift action_6
action_1 (27) = happyShift action_7
action_1 (10) = happyGoto action_2
action_1 (11) = happyGoto action_3
action_1 (12) = happyGoto action_4
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (16) = happyShift action_21
action_2 (17) = happyShift action_22
action_2 _ = happyReduce_1

action_3 (18) = happyShift action_20
action_3 _ = happyReduce_18

action_4 _ = happyReduce_20

action_5 _ = happyReduce_23

action_6 (13) = happyShift action_19
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (13) = happyShift action_5
action_7 (17) = happyShift action_6
action_7 (27) = happyShift action_7
action_7 (10) = happyGoto action_16
action_7 (11) = happyGoto action_3
action_7 (12) = happyGoto action_4
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (42) = happyAccept
action_8 _ = happyFail (happyExpListPerState 8)

action_9 _ = happyReduce_3

action_10 _ = happyReduce_4

action_11 (16) = happyShift action_18
action_11 _ = happyReduce_2

action_12 (22) = happyShift action_17
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_15

action_14 (13) = happyShift action_5
action_14 (15) = happyShift action_13
action_14 (17) = happyShift action_6
action_14 (27) = happyShift action_14
action_14 (9) = happyGoto action_15
action_14 (10) = happyGoto action_16
action_14 (11) = happyGoto action_3
action_14 (12) = happyGoto action_4
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (16) = happyShift action_18
action_15 (28) = happyShift action_34
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (16) = happyShift action_21
action_16 (17) = happyShift action_22
action_16 (28) = happyShift action_33
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (21) = happyShift action_29
action_17 (38) = happyShift action_30
action_17 (39) = happyShift action_31
action_17 (40) = happyShift action_32
action_17 (7) = happyGoto action_28
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (15) = happyShift action_13
action_18 (27) = happyShift action_27
action_18 (9) = happyGoto action_26
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_22

action_20 (13) = happyShift action_5
action_20 (17) = happyShift action_6
action_20 (27) = happyShift action_7
action_20 (12) = happyGoto action_25
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (13) = happyShift action_5
action_21 (17) = happyShift action_6
action_21 (27) = happyShift action_7
action_21 (11) = happyGoto action_24
action_21 (12) = happyGoto action_4
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (13) = happyShift action_5
action_22 (17) = happyShift action_6
action_22 (27) = happyShift action_7
action_22 (11) = happyGoto action_23
action_22 (12) = happyGoto action_4
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (18) = happyShift action_20
action_23 _ = happyReduce_17

action_24 (18) = happyShift action_20
action_24 _ = happyReduce_16

action_25 _ = happyReduce_19

action_26 (16) = happyShift action_18
action_26 _ = happyReduce_13

action_27 (15) = happyShift action_13
action_27 (27) = happyShift action_27
action_27 (9) = happyGoto action_15
action_27 _ = happyFail (happyExpListPerState 27)

action_28 _ = happyReduce_7

action_29 (13) = happyShift action_5
action_29 (15) = happyShift action_13
action_29 (17) = happyShift action_6
action_29 (27) = happyShift action_14
action_29 (9) = happyGoto action_36
action_29 (10) = happyGoto action_37
action_29 (11) = happyGoto action_3
action_29 (12) = happyGoto action_4
action_29 _ = happyFail (happyExpListPerState 29)

action_30 _ = happyReduce_9

action_31 _ = happyReduce_8

action_32 (27) = happyShift action_35
action_32 _ = happyFail (happyExpListPerState 32)

action_33 _ = happyReduce_21

action_34 _ = happyReduce_14

action_35 (38) = happyShift action_30
action_35 (39) = happyShift action_31
action_35 (40) = happyShift action_32
action_35 (7) = happyGoto action_38
action_35 (8) = happyGoto action_39
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (16) = happyShift action_18
action_36 _ = happyReduce_6

action_37 (16) = happyShift action_21
action_37 (17) = happyShift action_22
action_37 _ = happyReduce_5

action_38 (23) = happyShift action_41
action_38 _ = happyReduce_11

action_39 (28) = happyShift action_40
action_39 _ = happyFail (happyExpListPerState 39)

action_40 _ = happyReduce_10

action_41 (38) = happyShift action_30
action_41 (39) = happyShift action_31
action_41 (40) = happyShift action_32
action_41 (7) = happyGoto action_38
action_41 (8) = happyGoto action_42
action_41 _ = happyFail (happyExpListPerState 41)

action_42 _ = happyReduce_12

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  4 happyReduction_3
happyReduction_3 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  4 happyReduction_4
happyReduction_4 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happyReduce 4 5 happyReduction_5
happyReduction_5 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Initialization happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 4 5 happyReduction_6
happyReduction_6 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Initialization happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_3  6 happyReduction_7
happyReduction_7 (HappyAbsSyn7  happy_var_3)
	_
	(HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn4
		 (Declaration happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  7 happyReduction_8
happyReduction_8 _
	 =  HappyAbsSyn7
		 (T.Int
	)

happyReduce_9 = happySpecReduce_1  7 happyReduction_9
happyReduction_9 _
	 =  HappyAbsSyn7
		 (T.String
	)

happyReduce_10 = happyReduce 4 7 happyReduction_10
happyReduction_10 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (T.TupleFixed happy_var_3
	) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_1  8 happyReduction_11
happyReduction_11 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn8
		 ([happy_var_1]
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  8 happyReduction_12
happyReduction_12 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn8
		 ([happy_var_1] ++ [happy_var_3]
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  9 happyReduction_13
happyReduction_13 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Add happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  9 happyReduction_14
happyReduction_14 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  9 happyReduction_15
happyReduction_15 (HappyTerminal (TokenString happy_var_1))
	 =  HappyAbsSyn4
		 (Ast.String happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  10 happyReduction_16
happyReduction_16 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Add happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  10 happyReduction_17
happyReduction_17 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Sub happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  10 happyReduction_18
happyReduction_18 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  11 happyReduction_19
happyReduction_19 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Mul happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  11 happyReduction_20
happyReduction_20 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  12 happyReduction_21
happyReduction_21 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_2  12 happyReduction_22
happyReduction_22 (HappyTerminal (TokenNumber happy_var_2))
	_
	 =  HappyAbsSyn4
		 (IntLit (-happy_var_2)
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  12 happyReduction_23
happyReduction_23 (HappyTerminal (TokenNumber happy_var_1))
	 =  HappyAbsSyn4
		 (IntLit happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 42 42 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenNumber happy_dollar_dollar -> cont 13;
	TokenName happy_dollar_dollar -> cont 14;
	TokenString happy_dollar_dollar -> cont 15;
	TokenPrimitives "+" -> cont 16;
	TokenPrimitives "-" -> cont 17;
	TokenPrimitives "*" -> cont 18;
	TokenPrimitives "<" -> cont 19;
	TokenPrimitives ">" -> cont 20;
	TokenPrimitives "=" -> cont 21;
	TokenPrimitives ":" -> cont 22;
	TokenPrimitives "," -> cont 23;
	TokenPrimitives "|" -> cont 24;
	TokenPrimitives "?" -> cont 25;
	TokenSemicolon -> cont 26;
	TokenLeft Rounded -> cont 27;
	TokenRight Rounded -> cont 28;
	TokenLeft Squared -> cont 29;
	TokenRight Squared -> cont 30;
	TokenRight Curly -> cont 31;
	TokenLeft Curly -> cont 32;
	TokenKey "if" -> cont 33;
	TokenKey "then" -> cont 34;
	TokenKey "else" -> cont 35;
	TokenKey "for" -> cont 36;
	TokenKey "do" -> cont 37;
	TokenKey "string" -> cont 38;
	TokenKey "int" -> cont 39;
	TokenKey "tuple" -> cont 40;
	TokenKey "false?" -> cont 41;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 42 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
parseExp tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError t = error $ "Syntaxfehler bei: " ++ show t
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
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
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
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





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
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
