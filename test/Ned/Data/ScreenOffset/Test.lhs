---
title: Ned.Data.ScreenOffset.Test
---



Contents
--------

* [Introduction](#introduction)
* [Generators](#generators)
    * [Test helperts](#test-helpers)
* [Tests](#tests)
    * [Span Width](#span-width)
    * [Type level numbers](#type-level-numbers)
    * [takeChunk properties](#takechunk-properties)
    * [takeChunks properties](#takechunks-properties)
    * [ScreenOffset is a monoid](#screenoffset-is-a-monoid)
    * [applyScreenOffset is a monoid action](#applyscreenoffset-is-a-monoid-action)
    * [Test suite](#test-suite)



Introduction
============

> {-# LANGUAGE
>     ScopedTypeVariables
>   , Rank2Types
> #-}
> 
> module Ned.Data.ScreenOffset.Test (
>     test_ScreenOffset
> ) where
> 
> import Data.Proxy
> import Data.Foldable
> import Control.Monad
> 
> import Test.QuickCheck
> import Test.Tasty
> import Test.Tasty.QuickCheck
> 
> import Kreb.Reflect
> import Kreb.Struct
> 
> import Ned.Data.ScreenOffset
> 
> import Kreb.Struct.FingerTree.Test
> import Kreb.Struct.RunLengthEncoding.Test



Generators
==========

> instance
>   Arbitrary Span
>   where
>     arbitrary = elements
>       [ Fixed0, Fixed1, Fixed2, Stretchy ]

> instance
>   ( IsWidth w, IsTab t
>   ) => Arbitrary (ScreenOffset w t)
>   where
>     arbitrary = do
>       let
>         run = do
>           Positive k <- arbitrary
>           a <- arbitrary
>           return (k,a)
>       p <- arbitrary
>       if p
>         then
>           mkNoNewlines
>             <$> listOf run
>         else do
>           Positive m <- arbitrary
>           mkWithNewlines
>             <$> listOf run <*> pure m <*> listOf run
> 
>     shrink x = case x of
>       NoNewlines zs -> do
>         cs <- shrink zs
>         return $ NoNewlines cs
>       WithNewlines xs k ys ->
>         [ NoNewlines xs, NoNewlines ys ] ++
>         do
>           as <- shrink xs
>           m <- [1..k]
>           bs <- shrink ys
>           return $ WithNewlines as m bs

> genAll
>   :: Span
>   -> NonNegative Int
>   -> Gen (RunLengthEncoding Span)
> genAll span (NonNegative k) = do
>   let
>     xs = if k <= 0
>       then []
>       else [(k, span)]
>   return $ fromFreqList xs

> genValidCoords
>   :: forall w
>    . ( IsWidth w )
>   => Proxy w
>   -> Gen (Int, Int)
> genValidCoords _ = do
>   let w = toWidth (Proxy :: Proxy w)
>   x <- choose (0,w-1)
>   Positive y <- arbitrary
>   return (x,y)



Test helpers
------------

> lastSpanWithPositiveWidth
>   :: RunLengthEncoding Span -> Maybe Span
> lastSpanWithPositiveWidth xs = do
>   (Run (_,a), ys) <- lastRun xs
>   case a of
>     Fixed0 -> do
>       (Run (_,b), _) <- lastRun ys
>       return b
>     _ -> return a

> (~<=)
>   :: ( Ord a, Show a )
>   => a -> a -> Property
> x ~<= y =
>   counterexample (show x ++ interpret res ++ show y) res
>   where
>     res = x <= y
>     interpret True  = " <= "
>     interpret False = " > "

> (~<)
>   :: ( Ord a, Show a )
>   => a -> a -> Property
> x ~< y =
>   counterexample (show x ++ interpret res ++ show y) res
>   where
>     res = x < y
>     interpret True  = " < "
>     interpret False = " >= "



Tests
=====

Span Width
----------

> test_SpanWidth :: TestTree
> test_SpanWidth =
>   testGroup "spanWidth"
>     [ test_SpanWidth_all_Fixed0
>     , test_SpanWidth_all_Fixed1
>     , test_SpanWidth_all_Fixed2
>     , test_SpanWidth_examples
>     ]

A list of spans with $k$ width 0 characters has width 0.

> prop_SpanWidth_all_Fixed0
>   :: NonNegative Int -- number of spans
>   -> Positive Int    -- tab width
>   -> Property
> prop_SpanWidth_all_Fixed0 m t =
>   forAll (genAll Fixed0 m) $
>     let
>       NonNegative k = m
>       Positive tab = t
>     in
>       \xs -> 0 === spanWidth tab xs
> 
> test_SpanWidth_all_Fixed0 :: TestTree
> test_SpanWidth_all_Fixed0 =
>   testProperty "all Fixed0"
>     prop_SpanWidth_all_Fixed0

A list of spans with $k$ width 1 characters has width $k$.

> prop_SpanWidth_all_Fixed1
>   :: NonNegative Int -- number of spans
>   -> Positive Int    -- tab width
>   -> Property
> prop_SpanWidth_all_Fixed1 m t =
>   forAll (genAll Fixed1 m) $
>     let
>       NonNegative k = m
>       Positive tab = t
>     in
>       \xs -> k === spanWidth tab xs
> 
> test_SpanWidth_all_Fixed1 :: TestTree
> test_SpanWidth_all_Fixed1 =
>   testProperty "all Fixed1"
>     prop_SpanWidth_all_Fixed1

A list of spans with $k$ width 2 characters has width $2k$.

> prop_SpanWidth_all_Fixed2
>   :: NonNegative Int -- number of spans
>   -> Positive Int    -- tab width
>   -> Property
> prop_SpanWidth_all_Fixed2 m t =
>   forAll (genAll Fixed2 m) $
>     let
>       NonNegative k = m
>       Positive tab = t
>     in
>       \xs -> (2*k) === spanWidth tab xs
> 
> test_SpanWidth_all_Fixed2 :: TestTree
> test_SpanWidth_all_Fixed2 =
>   testProperty "all Fixed2"
>     prop_SpanWidth_all_Fixed2

For good measure, some concrete examples to check our intuition about `spanWidth`.

> prop_SpanWidth_examples
>   :: Int -> RunLengthEncoding Span
>   -> Int
>   -> Property
> prop_SpanWidth_examples tab xs w =
>   property $
>     w == spanWidth tab xs
> 
> test_SpanWidth_examples :: TestTree
> test_SpanWidth_examples =
>   testGroup "spanWidth examples"
>     [ testCases
>       (uncurry3 prop_SpanWidth_examples)
>       [ ( "all width 1"
>         , ( 5
>           , fromFreqList
>               [ (5, Fixed1) ]
>           , 5
>           )
>         )
> 
>       , ( "mixed, no tabs"
>         , ( 5
>           , fromFreqList
>               [ (2, Fixed1), (3, Fixed0), (3, Fixed2) ]
>           , 8
>           )
>         )
> 
>       , ( "only tabs"
>         , ( 5
>           , fromFreqList
>               [ (4, Stretchy) ]
>           , 20
>           )
>         )
> 
>       , ( "interleaved tabs"
>         , ( 5
>           , fromFreqList
>               [ (1, Fixed1), (1, Stretchy)
>               , (1, Fixed1), (1, Stretchy) ]
>           , 10
>           )
>         )
> 
>       , ( "full width tabs"
>         , ( 6
>           , fromFreqList
>               [ (6, Fixed1), (1, Stretchy)
>               , (3, Fixed2), (1, Stretchy) ]
>           , 24
>           )
>         )
>       ]
>     ]



Type level numbers
------------------

> test_ScreenOffset_type_nats :: TestTree
> test_ScreenOffset_type_nats =
>   testGroup "Type level nat parameters"
>     [ test_ScreenOffset_withWidth_toWidth
>     , test_ScreenOffset_withTab_toTab
>     ]

> prop_ScreenOffset_withWidth_toWidth
>   :: NonNegative Int
>   -> Property
> 
> prop_ScreenOffset_withWidth_toWidth (NonNegative k) =
>   property $
>     k == withWidth k toWidth
> 
> test_ScreenOffset_withWidth_toWidth :: TestTree
> test_ScreenOffset_withWidth_toWidth =
>   testProperty "k == withWidth k toWidth"
>     prop_ScreenOffset_withWidth_toWidth

> prop_ScreenOffset_withTab_toTab
>   :: NonNegative Int
>   -> Property
> 
> prop_ScreenOffset_withTab_toTab (NonNegative k) =
>   property $
>     k == withTab k toTab
> 
> test_ScreenOffset_withTab_toTab :: TestTree
> test_ScreenOffset_withTab_toTab =
>   testProperty "k == withTab k toTab"
>     prop_ScreenOffset_withTab_toTab



takeChunk properties
--------------------

`takeChunk` is a hideously intricate function, so we'd better put some effort into testing it. Fortunately there are several simple properties it should satisfy.

> test_TakeChunk :: TestTree
> test_TakeChunk =
>   testGroup "takeChunk properties"
>     [ test_TakeChunk_width_succeeds
>     , test_TakeChunk_take_width_bound
>     , test_TakeChunk_take_not_empty
>     , test_TakeChunk_cat_factor
>     , test_TakeChunk_shrink_tail
>     , test_TakeChunk_length
>     , test_TakeChunk_rest_empty
>     , test_TakeChunk_examples
>     ]

If the input span width exceeds the screen width, `takeChunk` succeeds.

> prop_TakeChunk_width_succeeds
>    , cprop_TakeChunk_width_succeeds
>   :: Positive Int
>   -> Positive Int
>   -> RunLengthEncoding Span
>   -> Property
> 
> prop_TakeChunk_width_succeeds
>   (Positive width) (Positive tab) xs =
>   case takeChunk width tab xs of
>     Nothing -> (spanWidth tab xs) ~< width
>     Just _  -> property True
> 
> cprop_TakeChunk_width_succeeds
>   u@(Positive width) v@(Positive tab) xs =
>   (width >= 2)
>     && (not $ isEmptyRLE xs)
>     && (spanWidth tab xs > 0) ==>
>   cover 40 (width > tab) "width > tab" $
>   cover 50 (width > 30) "width > 30" $
>   prop_TakeChunk_width_succeeds u v xs
> 
> test_TakeChunk_width_succeeds :: TestTree
> test_TakeChunk_width_succeeds =
>   testProperty "If span width > screen width, takeChunk succeeds"
>     cprop_TakeChunk_width_succeeds

The width of the taken chunk is bounded, with the exact bound depending on the last span of positive width appearing in the chunk.

> prop_TakeChunk_take_width_bound
>    , cprop_TakeChunk_take_width_bound
>   :: Positive Int
>   -> Positive Int
>   -> RunLengthEncoding Span
>   -> Property
> 
> prop_TakeChunk_take_width_bound
>   (Positive width) (Positive tab) xs =
>   case takeChunk width tab xs of
>     Nothing -> property True
>     Just (as, _) ->
>       let w = spanWidth tab as in
>       case lastSpanWithPositiveWidth as of
>         Nothing       -> error "prop_TakeChunk_take_width_bound"
>         Just Stretchy -> w ~<= (width + tab - 1)
>         Just _        -> w ~<= width
> 
> cprop_TakeChunk_take_width_bound
>   u@(Positive width) v@(Positive tab) xs =
>   (width >= 2)
>     && (not $ isEmptyRLE xs)
>     && (spanWidth tab xs > 0) ==>
>   cover 40 (width > tab) "width > tab" $
>   cover 50 (width > 30) "width > 30" $
>   prop_TakeChunk_take_width_bound u v xs
> 
> test_TakeChunk_take_width_bound :: TestTree
> test_TakeChunk_take_width_bound =
>   testProperty "taken chunk width is bounded"
>     cprop_TakeChunk_take_width_bound

If the input is not empty and the width is at least 2, then the taken chunk is not empty.

> prop_TakeChunk_take_not_empty
>    , cprop_TakeChunk_take_not_empty
>   :: Positive Int
>   -> Positive Int
>   -> RunLengthEncoding Span
>   -> Property
> 
> prop_TakeChunk_take_not_empty
>   (Positive width) (Positive tab) xs =
>   property $
>     case takeChunk width tab xs of
>       Nothing -> True
>       Just (as, _) -> not $ isEmptyRLE as
> 
> cprop_TakeChunk_take_not_empty
>   u@(Positive width) v@(Positive tab) xs =
>   (width >= 2)
>     && (not $ isEmptyRLE xs)
>     && (spanWidth tab xs > 0) ==>
>   cover 40 (width > tab) "width > tab" $
>   cover 50 (width > 30) "width > 30" $
>   prop_TakeChunk_take_not_empty u v xs
> 
> test_TakeChunk_take_not_empty :: TestTree
> test_TakeChunk_take_not_empty =
>   testProperty "taken chunk is not empty"
>     cprop_TakeChunk_take_not_empty

The result of `takeChunk` is a cat factorization of the original list.

> prop_TakeChunk_cat_factor
>    , cprop_TakeChunk_cat_factor
>   :: Positive Int
>   -> Positive Int
>   -> RunLengthEncoding Span
>   -> Property
> 
> prop_TakeChunk_cat_factor
>   (Positive width) (Positive tab) xs =
>   property $
>     case takeChunk width tab xs of
>       Nothing -> True
>       Just (as, bs) -> xs == (as <> bs)
> 
> cprop_TakeChunk_cat_factor
>   u@(Positive width) v@(Positive tab) xs =
>   (width >= 2)
>     && (not $ isEmptyRLE xs)
>     && (spanWidth tab xs > 0) ==>
>   cover 40 (width > tab) "width > tab" $
>   cover 50 (width > 30) "width > 30" $
>   prop_TakeChunk_cat_factor u v xs
> 
> test_TakeChunk_cat_factor :: TestTree
> test_TakeChunk_cat_factor =
>   testProperty "takeChunk is a cat factorization"
>     cprop_TakeChunk_cat_factor

If `takeChunk` succeeds, then the chunked tail is shorter than the original list.

> prop_TakeChunk_shrink_tail
>    , cprop_TakeChunk_shrink_tail
>   :: Positive Int
>   -> Positive Int
>   -> RunLengthEncoding Span
>   -> Property
> 
> prop_TakeChunk_shrink_tail
>   (Positive width) (Positive tab) xs =
>   property $
>     case takeChunk width tab xs of
>       Nothing -> True
>       Just (_,bs) ->
>         (runLength $ value bs) < (runLength $ value xs)
> 
> cprop_TakeChunk_shrink_tail
>   u@(Positive width) v@(Positive tab) xs =
>   (width >= 2)
>     && (not $ isEmptyRLE xs)
>     && (spanWidth tab xs > 0) ==>
>   cover 40 (width > tab) "width > tab" $
>   cover 50 (width > 30) "width > 30" $
>   prop_TakeChunk_shrink_tail u v xs
> 
> test_TakeChunk_shrink_tail :: TestTree
> test_TakeChunk_shrink_tail =
>   testProperty "takeChunk shrinks its input"
>     cprop_TakeChunk_shrink_tail

`takeChunk` preserves length

> prop_TakeChunk_length
>    , cprop_TakeChunk_length
>   :: Positive Int
>   -> Positive Int
>   -> RunLengthEncoding Span
>   -> Property
> 
> prop_TakeChunk_length
>   (Positive width) (Positive tab) xs =
>   property $
>     case takeChunk width tab xs of
>       Nothing -> True
>       Just (as, bs) -> (runLength $ value xs) ==
>         (runLength $ value as) + (runLength $ value bs)
> 
> cprop_TakeChunk_length
>   u@(Positive width) v@(Positive tab) xs =
>   (width >= 2)
>     && (not $ isEmptyRLE xs)
>     && (spanWidth tab xs > 0) ==>
>   cover 40 (width > tab) "width > tab" $
>   cover 50 (width > 30) "width > 30" $
>   prop_TakeChunk_length
> 
> test_TakeChunk_length :: TestTree
> test_TakeChunk_length =
>   testProperty "takeChunk preserves length"
>     cprop_TakeChunk_length

If the remainder of a chunked list is empty, then the taken chunk should be _full_ -- its width should equal that of the terminal window or, if the last block with positive width is stretchy, should be between the terminal window width and width plus tab.

> prop_TakeChunk_rest_empty
>    , cprop_TakeChunk_rest_empty
>   :: Positive Int
>   -> Positive Int
>   -> RunLengthEncoding Span
>   -> Property
> 
> prop_TakeChunk_rest_empty
>   (Positive width) (Positive tab) xs =
>   property $
>     case takeChunk width tab xs of
>       Nothing -> property True
>       Just (as, bs) -> if not $ isEmptyRLE bs
>         then property True
>         else 
>           let w = spanWidth tab as in
>           case lastSpanWithPositiveWidth as of
>             Nothing       -> error "prop_TakeChunk_take_width_bound"
>             Just Stretchy -> (width ~<= w) .&&. (w ~<= (width + tab - 1))
>             Just _        -> w === width
> 
> cprop_TakeChunk_rest_empty
>   u@(Positive width) v@(Positive tab) xs =
>   (width >= 2)
>     && (not $ isEmptyRLE xs)
>     && (spanWidth tab xs > 0) ==>
>   cover 40 (width > tab) "width > tab" $
>   cover 50 (width > 30) "width > 30" $
>   prop_TakeChunk_rest_empty u v xs
> 
> test_TakeChunk_rest_empty :: TestTree
> test_TakeChunk_rest_empty =
>   testProperty "remainder is only empty if taken chunk is full"
>     cprop_TakeChunk_rest_empty

For good measure, some examples to check our intuition about `takeChunk`.

> prop_TakeChunk_examples
>   :: Int -> Int -> RunLengthEncoding Span
>   -> Maybe (RunLengthEncoding Span, RunLengthEncoding Span)
>   -> Property
> prop_TakeChunk_examples w t xs result =
>   property $
>     result === takeChunk w t xs
> 
> test_TakeChunk_examples :: TestTree
> test_TakeChunk_examples =
>   testGroup "takeChunk examples"
>     [ testCases
>       (uncurry4 prop_TakeChunk_examples)
>       [ ( "#1"
>         , ( 2
>           , 1
>           , fromFreqList
>               [ (1,Stretchy), (2,Fixed1) ]
>           , Just
>             ( fromFreqList
>                 [(1,Stretchy), (1,Fixed1)]
>             , fromFreqList
>                 [(1,Fixed1)]
>             )
>           )
>         )
> 
>       , ( "#2"
>         , ( 2
>           , 1
>           , fromFreqList
>               [ (1,Fixed1), (1,Fixed0) ]
>           , Nothing
>           )
>         )
> 
>       , ( "#3"
>         , ( 3
>           , 2
>           , fromFreqList
>               [ (5,Stretchy) ]
>           , Just
>               ( fromFreqList [(2,Stretchy)]
>               , fromFreqList [(3,Stretchy)]
>               )
>           )
>         )
> 
>       ]
>     ]



takeChunks properties
---------------------

> test_TakeChunks :: TestTree
> test_TakeChunks =
>   localOption (QuickCheckTests 500) $
>   testGroup "takeChunks properties"
>     [ test_TakeChunks_cat_factor
>     , test_TakeChunks_take_size_upper_bound
>     , test_TakeChunks_take_size_lower_bound
>     , test_TakeChunks_rest_size_upper_bound
>     ]

`takeChunks` should give a cat factorization.

> prop_TakeChunks_cat_factor
>    , cprop_TakeChunks_cat_factor
>   :: Positive Int
>   -> Positive Int
>   -> RunLengthEncoding Span
>   -> Property
> 
> prop_TakeChunks_cat_factor
>   (Positive width) (Positive tab) xs =
>   property $
>     let (uss, vs) = takeChunks width tab xs in
>     xs == mconcat (uss ++ [vs])
> 
> cprop_TakeChunks_cat_factor
>   u@(Positive width) v@(Positive tab) xs =
>   (width >= 2) && (not $ isEmptyRLE xs) ==>
>   cover 40 (width > tab) "width > tab" $
>   cover 50 (width > 30) "width > 30" $
>   cover 50 (runLength (value xs) > 100) "|xs| > 100" $
>   prop_TakeChunks_cat_factor u v xs
> 
> test_TakeChunks_cat_factor :: TestTree
> test_TakeChunks_cat_factor =
>   testProperty "takeChunks yields a cat factorization"
>     cprop_TakeChunks_cat_factor

The widths of the taken chunks are bounded above.

> prop_TakeChunks_take_size_upper_bound
>    , cprop_TakeChunks_take_size_upper_bound
>   :: Positive Int
>   -> Positive Int
>   -> RunLengthEncoding Span
>   -> Property
> 
> prop_TakeChunks_take_size_upper_bound
>   (Positive width) (Positive tab) xs =
>   let (as,_) = takeChunks width tab xs in
>     conjoin $ map checkBound as
>   where
>     checkBound zs =
>       let w = spanWidth tab zs in
>       case lastSpanWithPositiveWidth zs of
>         Nothing       -> error "prop_TakeChunks_take_size_upper_bound"
>         Just Stretchy -> w ~<= (width + tab - 1)
>         Just _        -> w ~<= width
> 
> cprop_TakeChunks_take_size_upper_bound
>   u@(Positive width) v@(Positive tab) xs =
>   (width >= 2) && (not $ isEmptyRLE xs) ==>
>   cover 40 (width > tab) "width > tab" $
>   cover 50 (width > 30) "width > 30" $
>   cover 50 (runLength (value xs) > 100) "|xs| > 100" $
>   prop_TakeChunks_take_size_upper_bound u v xs
> 
> test_TakeChunks_take_size_upper_bound :: TestTree
> test_TakeChunks_take_size_upper_bound =
>   testProperty "taken chunk size is bounded above"
>     cprop_TakeChunks_take_size_upper_bound

The widths of the taken chunks are bounded below.

> prop_TakeChunks_take_size_lower_bound
>    , cprop_TakeChunks_take_size_lower_bound
>   :: Positive Int
>   -> Positive Int
>   -> RunLengthEncoding Span
>   -> Property
> 
> prop_TakeChunks_take_size_lower_bound
>   (Positive width) (Positive tab) xs =
>   let (as,_) = takeChunks width tab xs in
>     conjoin $ map checkBound as
>   where
>     checkBound zs =
>       let w = spanWidth tab zs in
>       case lastSpanWithPositiveWidth zs of
>         Nothing     -> error "prop_TakeChunks_take_size_lower_bound (1)"
>         Just Fixed0 -> error "prop_TakeChunks_take_size_lower_bound (2)"
>         Just _      -> (width - 1) ~<= w
> 
> cprop_TakeChunks_take_size_lower_bound
>   u@(Positive width) v@(Positive tab) xs =
>   (width >= 2) && (not $ isEmptyRLE xs) ==>
>   cover 40 (width > tab) "width > tab" $
>   cover 50 (width > 30) "width > 30" $
>   cover 50 (runLength (value xs) > 100) "|xs| > 100" $
>   prop_TakeChunks_take_size_lower_bound u v xs
> 
> test_TakeChunks_take_size_lower_bound :: TestTree
> test_TakeChunks_take_size_lower_bound =
>   testProperty "taken chunk size is bounded above"
>     cprop_TakeChunks_take_size_lower_bound

The width of the remainder is bounded above.

> prop_TakeChunks_rest_size_upper_bound
>    , cprop_TakeChunks_rest_size_upper_bound
>   :: Positive Int
>   -> Positive Int
>   -> RunLengthEncoding Span
>   -> Property
> 
> prop_TakeChunks_rest_size_upper_bound
>   (Positive width) (Positive tab) xs =
>   let
>     (_,bs) = takeChunks width tab xs
>     w = spanWidth tab bs
>   in
>     case lastSpanWithPositiveWidth bs of
>       Nothing       -> property True
>       Just Stretchy -> w ~<= (width + tab - 1)
>       Just _        -> w ~<= width
> 
> cprop_TakeChunks_rest_size_upper_bound
>   u@(Positive width) v@(Positive tab) xs =
>   (width >= 2) && (not $ isEmptyRLE xs) ==>
>   cover 40 (width > tab) "width > tab" $
>   cover 50 (width > 30) "width > 30" $
>   cover 50 (runLength (value xs) > 100) "|xs| > 100" $
>   prop_TakeChunks_rest_size_upper_bound u v xs
> 
> test_TakeChunks_rest_size_upper_bound :: TestTree
> test_TakeChunks_rest_size_upper_bound =
>   testProperty "rest size is bounded above"
>     cprop_TakeChunks_rest_size_upper_bound



ScreenOffset is a monoid
------------------------

> test_ScreenOffset_Monoid :: TestTree
> test_ScreenOffset_Monoid =
>   testGroup "ScreenOffset is a monoid"
>     [ test_ScreenOffset_Monoid_neutral_left
>     , test_ScreenOffset_Monoid_neutral_right
>     , localOption (QuickCheckTests 500) $
>         test_ScreenOffset_Monoid_associativity
>     , test_ScreenOffset_Monoid_unique_right_identity
>     , test_ScreenOffset_Monoid_unique_left_identity
>     ]

Left identity law:

> prop_ScreenOffset_Monoid_neutral_left
>   :: ( IsWidth w, IsTab t )
>   => Proxy w -> Proxy t
>   -> ScreenOffset w t
>   -> Property
> 
> prop_ScreenOffset_Monoid_neutral_left _ _ x =
>   x === (mempty <> x)
> 
> test_ScreenOffset_Monoid_neutral_left :: TestTree
> test_ScreenOffset_Monoid_neutral_left =
>   testGroup "Left identity law"
>     [ testProperty "30/8" $
>         prop_ScreenOffset_Monoid_neutral_left nat30 nat8
>     , testProperty "30/4" $
>         prop_ScreenOffset_Monoid_neutral_left nat30 nat4
>     , testProperty "15/2" $
>         prop_ScreenOffset_Monoid_neutral_left nat15 nat2
>     , testProperty "8/2" $
>         prop_ScreenOffset_Monoid_neutral_left nat8 nat2
>     ]

Right identity law:

> prop_ScreenOffset_Monoid_neutral_right
>   :: ( IsWidth w, IsTab t )
>   => Proxy w -> Proxy t
>   -> ScreenOffset w t
>   -> Property
> 
> prop_ScreenOffset_Monoid_neutral_right _ _ x =
>   x === (x <> mempty)
> 
> test_ScreenOffset_Monoid_neutral_right :: TestTree
> test_ScreenOffset_Monoid_neutral_right =
>   testGroup "Right identity law"
>     [ testProperty "30/8" $
>         prop_ScreenOffset_Monoid_neutral_right nat30 nat8
>     , testProperty "30/4" $
>         prop_ScreenOffset_Monoid_neutral_right nat30 nat4
>     , testProperty "15/2" $
>         prop_ScreenOffset_Monoid_neutral_right nat15 nat2
>     , testProperty "8/2" $
>         prop_ScreenOffset_Monoid_neutral_right nat8 nat2
>     ]

Associativity law:

> prop_ScreenOffset_Monoid_associativity
>   :: ( IsWidth w, IsTab t )
>   => Proxy w -> Proxy t
>   -> ScreenOffset w t
>   -> ScreenOffset w t
>   -> ScreenOffset w t
>   -> Property
> 
> prop_ScreenOffset_Monoid_associativity _ _ a b c =
>   (a <> (b <> c)) === ((a <> b) <> c)
> 
> test_ScreenOffset_Monoid_associativity :: TestTree
> test_ScreenOffset_Monoid_associativity =
>   testGroup "Associativity law"
>     [ testProperty "30/8" $
>         prop_ScreenOffset_Monoid_associativity nat30 nat8
>     , testProperty "30/4" $
>         prop_ScreenOffset_Monoid_associativity nat30 nat4
>     , testProperty "15/2" $
>         prop_ScreenOffset_Monoid_associativity nat15 nat2
>     , testProperty "8/2" $
>         prop_ScreenOffset_Monoid_associativity nat8 nat2
>     ]

If $ab = a$, then $b = 1$.

> prop_ScreenOffset_Monoid_unique_right_identity
>   :: ( IsWidth w, IsTab t )
>   => Proxy w -> Proxy t
>   -> ScreenOffset w t
>   -> ScreenOffset w t
>   -> Property
> 
> prop_ScreenOffset_Monoid_unique_right_identity _ _ a b =
>   if (a <> b) == b
>     then mempty === a
>     else property True
> 
> test_ScreenOffset_Monoid_unique_right_identity :: TestTree
> test_ScreenOffset_Monoid_unique_right_identity =
>   testGroup "Right identity is unique"
>     [ testProperty "30/8" $
>         prop_ScreenOffset_Monoid_unique_right_identity nat30 nat8
>     , testProperty "30/4" $
>         prop_ScreenOffset_Monoid_unique_right_identity nat30 nat4
>     , testProperty "15/2" $
>         prop_ScreenOffset_Monoid_unique_right_identity nat15 nat2
>     , testProperty "8/2" $
>         prop_ScreenOffset_Monoid_unique_right_identity nat8 nat2
>     ]

If $ba = a$, then $b = 1$.

> prop_ScreenOffset_Monoid_unique_left_identity
>   :: ( IsWidth w, IsTab t )
>   => Proxy w -> Proxy t
>   -> ScreenOffset w t
>   -> ScreenOffset w t
>   -> Property
> 
> prop_ScreenOffset_Monoid_unique_left_identity _ _ a b =
>   if (b <> a) == b
>     then mempty === a
>     else property True
> 
> test_ScreenOffset_Monoid_unique_left_identity :: TestTree
> test_ScreenOffset_Monoid_unique_left_identity =
>   testGroup "Left identity is unique"
>     [ testProperty "30/8" $
>         prop_ScreenOffset_Monoid_unique_left_identity nat30 nat8
>     , testProperty "30/4" $
>         prop_ScreenOffset_Monoid_unique_left_identity nat30 nat4
>     , testProperty "15/2" $
>         prop_ScreenOffset_Monoid_unique_left_identity nat15 nat2
>     , testProperty "8/2" $
>         prop_ScreenOffset_Monoid_unique_left_identity nat8 nat2
>     ]



applyScreenOffset is a monoid action
------------------------------------

A monoid action of $M$ on a set $A$ is equivalent to a monoid homomorphism from $M$ to the monoid of transformations on $A$. At least I think it is!

> test_ScreenOffset_MonoidAction :: TestTree
> test_ScreenOffset_MonoidAction =
>   testGroup "ScreenOffset monoid action"
>     [ test_ScreenOffset_MonoidAction_identity
>     , localOption (QuickCheckTests 500) $
>         test_ScreenOffset_MonoidAction_product
>     , localOption (QuickCheckTests 500) $
>         test_ScreenOffset_MonoidAction_welldefined
>     , test_ScreenOffset_applyBlockOffset_examples
>     ]

The action preserves the identity:

> prop_ScreenOffset_MonoidAction_identity
>   :: forall w t
>    . ( IsWidth w, IsTab t )
>   => Proxy w -> Proxy t
>   -> ScreenOffset w t
>   -> (Int, Int)
>   -> Property
> 
> prop_ScreenOffset_MonoidAction_identity _ _ off pos =
>   pos
>     === applyScreenOffset (mempty :: ScreenOffset w t) pos
> 
> test_ScreenOffset_MonoidAction_identity :: TestTree
> test_ScreenOffset_MonoidAction_identity =
>   testGroup "Identity law"
>     [ testProperty "30/8" $
>         prop_ScreenOffset_MonoidAction_identity nat30 nat8
>     , testProperty "30/4" $
>         prop_ScreenOffset_MonoidAction_identity nat30 nat4
>     , testProperty "15/2" $
>         prop_ScreenOffset_MonoidAction_identity nat15 nat2
>     , testProperty "8/2" $
>         prop_ScreenOffset_MonoidAction_identity nat8 nat2
>     ]

And the action preserves products:

> prop_ScreenOffset_MonoidAction_product
>   :: forall w t
>    . ( IsWidth w, IsTab t )
>   => Proxy w -> Proxy t
>   -> ScreenOffset w t
>   -> ScreenOffset w t
>   -> Property
> 
> prop_ScreenOffset_MonoidAction_product pw _ x y =
>   forAll (genValidCoords pw) $ \(u,v) ->
>   (applyScreenOffset (x <> y) (u,v))
>     === (applyScreenOffset y $ applyScreenOffset x (u,v))
> 
> test_ScreenOffset_MonoidAction_product :: TestTree
> test_ScreenOffset_MonoidAction_product =
>   testGroup "product law"
>     [ testProperty "30/8" $
>         prop_ScreenOffset_MonoidAction_product nat30 nat8
>     , testProperty "30/4" $
>         prop_ScreenOffset_MonoidAction_product nat30 nat4
>     , testProperty "15/2" $
>         prop_ScreenOffset_MonoidAction_product nat15 nat2
>     , testProperty "8/2" $
>         prop_ScreenOffset_MonoidAction_product nat8 nat2
>     , testProperty "3/1" $
>         prop_ScreenOffset_MonoidAction_product nat3 nat1
>     ]

The action is well defined on valid coordinates:

> prop_ScreenOffset_MonoidAction_welldefined
>   :: forall w t
>    . ( IsWidth w, IsTab t )
>   => Proxy w -> Proxy t
>   -> ScreenOffset w t
>   -> Property
> 
> prop_ScreenOffset_MonoidAction_welldefined pw _ off =
>   let w = toWidth pw in
>   forAll (genValidCoords pw) $ \pos@(_,k) ->
>     let (x,y) = applyScreenOffset off pos in
>     (0 <= x) .&&. (x < w) .&&. (k <= y)
> 
> test_ScreenOffset_MonoidAction_welldefined :: TestTree
> test_ScreenOffset_MonoidAction_welldefined =
>   testGroup "welldefined law"
>     [ testProperty "30/8" $
>         prop_ScreenOffset_MonoidAction_welldefined nat30 nat8
>     , testProperty "30/4" $
>         prop_ScreenOffset_MonoidAction_welldefined nat30 nat4
>     , testProperty "15/2" $
>         prop_ScreenOffset_MonoidAction_welldefined nat15 nat2
>     , testProperty "8/2" $
>         prop_ScreenOffset_MonoidAction_welldefined nat8 nat2
>     ]

> prop_ScreenOffset_applyBlockOffset_examples
>   :: Int -> Int
>   -> RunLengthEncoding Span -> (Int, Int)
>   -> (Int, Int)
>   -> Property
> 
> prop_ScreenOffset_applyBlockOffset_examples w t rle x y =
>   y === applyBlockOffset w t rle x
> 
> test_ScreenOffset_applyBlockOffset_examples :: TestTree
> test_ScreenOffset_applyBlockOffset_examples =
>   testGroup "applyBlockOffset examples"
>     [ testCases
>       (uncurry3 $ prop_ScreenOffset_applyBlockOffset_examples 8 2)
>       [ ( "#1"
>         , ( fromFreqList [(10,Fixed1)]
>           , (0,0)
>           , (2,1)
>           )
>         )
> 
>       , ( "#2"
>         , ( fromFreqList [(11,Fixed1)]
>           , (0,0)
>           , (3,1)
>           )
>         )
>       ]
>     ]

> prop_ScreenOffset_eq_examples
>   :: forall w t
>    . ( IsWidth w, IsTab t )
>   => ScreenOffset w t
>   -> ScreenOffset w t
>   -> Property
> 
> prop_ScreenOffset_eq_examples x y =
>   x === y
> 
> test_ScreenOffset_eq_examples :: TestTree
> test_ScreenOffset_eq_examples =
>   testGroup "Examples"
>     [ let
>         z = defNoNewlines nat3 nat1 [(1, Fixed1)]
>       in
>         testProperty "#1" $
>           prop_ScreenOffset_eq_examples
>             (defNoNewlines nat3 nat1 [(4, Fixed1)])
>             (z <> z <> z <> z)
> 
>     , let
>         z = defNoNewlines nat3 nat1 [(1, Fixed1)]
>         w = defWithNewlines nat3 nat1 [] 1 [(1, Fixed1)]
>       in
>         testGroup "#2"
>           [ testProperty "zzw" $
>             prop_ScreenOffset_eq_examples
>               (defWithNewlines nat3 nat1 [(2, Fixed1)] 1 [(1, Fixed1)])
>               (z <> z <> w)
> 
>           , testProperty "zzzw" $
>             prop_ScreenOffset_eq_examples
>               (defWithNewlines nat3 nat1 [(3, Fixed1)] 1 [(1, Fixed1)])
>               (z <> z <> z <> w)
>           ]
>     ]



Test Suite
----------

> test_ScreenOffset :: TestTree
> test_ScreenOffset =
>   testGroup "ScreenOffset"
>     [ test_SpanWidth
>     , test_ScreenOffset_type_nats
>     , test_TakeChunk
>     , test_TakeChunks
>     , test_ScreenOffset_Monoid
>     , test_ScreenOffset_MonoidAction
>     , test_ScreenOffset_eq_examples
>     ]
