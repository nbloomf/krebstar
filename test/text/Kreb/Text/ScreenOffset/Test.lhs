---
title: Kreb.Text.ScreenOffset.Test
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
> module Kreb.Text.ScreenOffset.Test (
>     test_ScreenOffset
> ) where
> 
> import Data.Proxy
> import Data.Foldable
> import Control.Monad
> 
> import Test.Tasty
> 
> import Kreb.Check
> import Kreb.Reflect
> import Kreb.Struct
> 
> import Kreb.Text.ScreenOffset



Generators
==========

> genAll
>   :: Span
>   -> NonNegative Int
>   -> Seeded (RunLengthEncoding Span)
> genAll span (NonNegative k) = do
>   let
>     xs = if k <= 0
>       then []
>       else [(fromIntegral k, span)]
>   return $ fromFreqList xs

> genValidCoords
>   :: forall w
>    . ( IsWidth w )
>   => Proxy w
>   -> Seeded (Int, Int)
> genValidCoords _ = do
>   let w = toWidth (Proxy :: Proxy w)
>   x <- randIn (0,w-1)
>   Positive y <- arb
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
>   -> Check
> prop_SpanWidth_all_Fixed0 m t =
>   forEach (genAll Fixed0 m) prune $
>     let
>       NonNegative k = m
>       Positive tab = t
>     in
>       \xs -> 0 == spanWidth tab xs
> 
> test_SpanWidth_all_Fixed0 :: TestTree
> test_SpanWidth_all_Fixed0 =
>   testKreb "all Fixed0"
>     prop_SpanWidth_all_Fixed0

A list of spans with $k$ width 1 characters has width $k$.

> prop_SpanWidth_all_Fixed1
>   :: NonNegative Int -- number of spans
>   -> Positive Int    -- tab width
>   -> Check
> prop_SpanWidth_all_Fixed1 m t =
>   forEach (genAll Fixed1 m) prune $
>     let
>       NonNegative k = m
>       Positive tab = t
>     in
>       \xs -> k == spanWidth tab xs
> 
> test_SpanWidth_all_Fixed1 :: TestTree
> test_SpanWidth_all_Fixed1 =
>   testKreb "all Fixed1"
>     prop_SpanWidth_all_Fixed1

A list of spans with $k$ width 2 characters has width $2k$.

> prop_SpanWidth_all_Fixed2
>   :: NonNegative Int -- number of spans
>   -> Positive Int    -- tab width
>   -> Check
> prop_SpanWidth_all_Fixed2 m t =
>   forEach (genAll Fixed2 m) prune $
>     let
>       NonNegative k = m
>       Positive tab = t
>     in
>       \xs -> (2*k) == spanWidth tab xs
> 
> test_SpanWidth_all_Fixed2 :: TestTree
> test_SpanWidth_all_Fixed2 =
>   testKreb "all Fixed2"
>     prop_SpanWidth_all_Fixed2

For good measure, some concrete examples to check our intuition about `spanWidth`.

> prop_SpanWidth_examples
>   :: Int -> RunLengthEncoding Span
>   -> Int
>   -> Check
> prop_SpanWidth_examples tab xs w =
>   check $
>     w == spanWidth tab xs
> 
> test_SpanWidth_examples :: TestTree
> test_SpanWidth_examples =
>   testGroup "spanWidth examples"
>     [ testKrebCases "examples"
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
>     [ testKreb "k == withWidth k toWidth"
>         prop_ScreenOffset_withWidth_toWidth
>     , testKreb "k == withTab k toTab"
>         prop_ScreenOffset_withTab_toTab
>     ]

> prop_ScreenOffset_withWidth_toWidth
>   :: NonNegative Int
>   -> Check
> prop_ScreenOffset_withWidth_toWidth (NonNegative k) =
>   check $
>     k == withWidth k toWidth

> prop_ScreenOffset_withTab_toTab
>   :: NonNegative Int
>   -> Check
> prop_ScreenOffset_withTab_toTab (NonNegative k) =
>   check $
>     k == withTab k toTab



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

If `takeChunk` succeeds then the input span width exceeds the screen width.

> prop_TakeChunk_width_succeeds
>   :: Positive Int
>   -> Positive Int
>   -> RunLengthEncoding Span
>   -> Check
> prop_TakeChunk_width_succeeds
>   (Positive width) (Positive tab) xs =
>   provisio
>     [ ("input not empty", not $ isEmptyRLE xs)
>     , ("width >= 2", width >= 2)
>     , ("spanWidth > 0", spanWidth tab xs > 0)
>     ] $
>   case takeChunk width tab xs of
>     Nothing -> claimLT (spanWidth tab xs) width
>     Just _  -> accept
>  
> test_TakeChunk_width_succeeds :: TestTree
> test_TakeChunk_width_succeeds =
>   testKreb "If span width > screen width, takeChunk succeeds"
>     prop_TakeChunk_width_succeeds

The width of the taken chunk is bounded, with the exact bound depending on the last span of positive width appearing in the chunk.

> prop_TakeChunk_take_width_bound
>   :: Positive Int
>   -> Positive Int
>   -> RunLengthEncoding Span
>   -> Check
> prop_TakeChunk_take_width_bound
>   (Positive width) (Positive tab) xs =
>   provisio
>     [ ("input not empty", not $ isEmptyRLE xs)
>     , ("width >= 2", width >= 2)
>     , ("spanWidth > 0", spanWidth tab xs > 0)
>     ] $
>   case takeChunk width tab xs of
>     Nothing -> accept
>     Just (as, _) ->
>       let w = spanWidth tab as in
>       case lastSpanWithPositiveWidth as of
>         Nothing       -> reject "prop_TakeChunk_take_width_bound"
>         Just Stretchy -> claimLEQ w (width + tab - 1)
>         Just _        -> claimLEQ w width
> 
> test_TakeChunk_take_width_bound :: TestTree
> test_TakeChunk_take_width_bound =
>   testKreb "taken chunk width is bounded"
>     prop_TakeChunk_take_width_bound

If the input is not empty and the width is at least 2, then the taken chunk is not empty.

> prop_TakeChunk_take_not_empty
>   :: Positive Int
>   -> Positive Int
>   -> RunLengthEncoding Span
>   -> Check
> prop_TakeChunk_take_not_empty
>   (Positive width) (Positive tab) xs =
>   provisio
>     [ ("input not empty", not $ isEmptyRLE xs)
>     , ("width >= 2", width >= 2)
>     , ("spanWidth > 0", spanWidth tab xs > 0)
>     ] $
>     case takeChunk width tab xs of
>       Nothing -> True
>       Just (as, _) -> not $ isEmptyRLE as
> 
> test_TakeChunk_take_not_empty :: TestTree
> test_TakeChunk_take_not_empty =
>   testKreb "taken chunk is not empty"
>     prop_TakeChunk_take_not_empty

The result of `takeChunk` is a cat factorization of the original list.

> prop_TakeChunk_cat_factor
>   :: Positive Int
>   -> Positive Int
>   -> RunLengthEncoding Span
>   -> Check
> prop_TakeChunk_cat_factor
>   (Positive width) (Positive tab) xs =
>   check $
>     case takeChunk width tab xs of
>       Nothing -> True
>       Just (as, bs) -> xs == (as <> bs)
> 
> test_TakeChunk_cat_factor :: TestTree
> test_TakeChunk_cat_factor =
>   testKreb "takeChunk is a cat factorization"
>     prop_TakeChunk_cat_factor

If `takeChunk` succeeds, then the chunked tail is shorter than the original list.

> prop_TakeChunk_shrink_tail
>   :: Positive Int
>   -> Positive Int
>   -> RunLengthEncoding Span
>   -> Check
> prop_TakeChunk_shrink_tail
>   (Positive width) (Positive tab) xs =
>   provisio
>     [ ("input not empty", not $ isEmptyRLE xs)
>     , ("width >= 2", width >= 2)
>     , ("spanWidth > 0", spanWidth tab xs > 0)
>     ] $
>   check $
>     case takeChunk width tab xs of
>       Nothing -> True
>       Just (_,bs) ->
>         (runLength $ value bs) < (runLength $ value xs)
> 
> test_TakeChunk_shrink_tail :: TestTree
> test_TakeChunk_shrink_tail =
>   testKreb "takeChunk shrinks its input"
>     prop_TakeChunk_shrink_tail

`takeChunk` preserves length

> prop_TakeChunk_length
>   :: Positive Int
>   -> Positive Int
>   -> RunLengthEncoding Span
>   -> Check
> prop_TakeChunk_length
>   (Positive width) (Positive tab) xs =
>   check $
>     case takeChunk width tab xs of
>       Nothing -> True
>       Just (as, bs) -> (runLength $ value xs) ==
>         (runLength $ value as) + (runLength $ value bs)
> 
> test_TakeChunk_length :: TestTree
> test_TakeChunk_length =
>   testKreb "takeChunk preserves length"
>     prop_TakeChunk_length

If the remainder of a chunked list is empty, then the taken chunk should be _full_ -- its width should equal that of the terminal window or, if the last block with positive width is stretchy, should be between the terminal window width and width plus tab.

> prop_TakeChunk_rest_empty
>   :: Positive Int
>   -> Positive Int
>   -> RunLengthEncoding Span
>   -> Check
> prop_TakeChunk_rest_empty
>   (Positive width) (Positive tab) xs =
>   check $
>     case takeChunk width tab xs of
>       Nothing -> accept
>       Just (as, bs) -> if not $ isEmptyRLE bs
>         then accept
>         else 
>           let w = spanWidth tab as in
>           case lastSpanWithPositiveWidth as of
>             Nothing       -> error "prop_TakeChunk_take_width_bound"
>             Just Stretchy -> (claimLEQ width w) .&&. (claimLEQ w (width + tab - 1))
>             Just _        -> claimEqual w width
> 
> test_TakeChunk_rest_empty :: TestTree
> test_TakeChunk_rest_empty =
>   testKreb "remainder is only empty if taken chunk is full"
>     prop_TakeChunk_rest_empty

For good measure, some examples to check our intuition about `takeChunk`.

> prop_TakeChunk_examples
>   :: Int -> Int -> RunLengthEncoding Span
>   -> Maybe (RunLengthEncoding Span, RunLengthEncoding Span)
>   -> Check
> prop_TakeChunk_examples w t xs result =
>   claimEqual result (takeChunk w t xs)
> 
> test_TakeChunk_examples :: TestTree
> test_TakeChunk_examples =
>   testGroup "takeChunk examples"
>     [ testKrebCases "examples"
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
>   testGroup "takeChunks properties"
>     [ test_TakeChunks_cat_factor
>     , test_TakeChunks_take_size_upper_bound
>     , test_TakeChunks_take_size_lower_bound
>     , test_TakeChunks_rest_size_upper_bound
>     ]

`takeChunks` should give a cat factorization.

> prop_TakeChunks_cat_factor
>   :: Positive Int
>   -> Positive Int
>   -> RunLengthEncoding Span
>   -> Check
> prop_TakeChunks_cat_factor
>   (Positive width) (Positive tab) xs =
>   provisio
>     [ ("input not empty", not $ isEmptyRLE xs)
>     , ("width >= 2", width >= 2)
>     ] $
>   check $
>     let (uss, vs) = takeChunks width tab xs in
>     xs == mconcat (uss ++ [vs])
> 
> test_TakeChunks_cat_factor :: TestTree
> test_TakeChunks_cat_factor =
>   testKreb "takeChunks yields a cat factorization"
>     prop_TakeChunks_cat_factor

The widths of the taken chunks are bounded above.

> prop_TakeChunks_take_size_upper_bound
>   :: Positive Int
>   -> Positive Int
>   -> RunLengthEncoding Span
>   -> Check
> prop_TakeChunks_take_size_upper_bound
>   (Positive width) (Positive tab) xs =
>   provisio
>     [ ("input not empty", not $ isEmptyRLE xs)
>     , ("width >= 2", width >= 2)
>     ] $
>   let (as,_) = takeChunks width tab xs in
>     claimAll $ map checkBound as
>   where
>     checkBound zs =
>       let w = spanWidth tab zs in
>       case lastSpanWithPositiveWidth zs of
>         Nothing       -> error "prop_TakeChunks_take_size_upper_bound"
>         Just Stretchy -> claimLEQ w (width + tab - 1)
>         Just _        -> claimLEQ w width
> 
> test_TakeChunks_take_size_upper_bound :: TestTree
> test_TakeChunks_take_size_upper_bound =
>   testKreb "taken chunk size is bounded above"
>     prop_TakeChunks_take_size_upper_bound

The widths of the taken chunks are bounded below.

> prop_TakeChunks_take_size_lower_bound
>   :: Positive Int
>   -> Positive Int
>   -> RunLengthEncoding Span
>   -> Check
> prop_TakeChunks_take_size_lower_bound
>   (Positive width) (Positive tab) xs =
>   provisio
>     [ ("input not empty", not $ isEmptyRLE xs)
>     , ("width >= 2", width >= 2)
>     ] $
>   let (as,_) = takeChunks width tab xs in
>     claimAll $ map checkBound as
>   where
>     checkBound zs =
>       let w = spanWidth tab zs in
>       case lastSpanWithPositiveWidth zs of
>         Nothing     -> error "prop_TakeChunks_take_size_lower_bound (1)"
>         Just Fixed0 -> error "prop_TakeChunks_take_size_lower_bound (2)"
>         Just _      -> claimLEQ (width - 1) w
> 
> test_TakeChunks_take_size_lower_bound :: TestTree
> test_TakeChunks_take_size_lower_bound =
>   testKreb "taken chunk size is bounded below"
>     prop_TakeChunks_take_size_lower_bound

The width of the remainder is bounded above.

> prop_TakeChunks_rest_size_upper_bound
>   :: Positive Int
>   -> Positive Int
>   -> RunLengthEncoding Span
>   -> Check
> prop_TakeChunks_rest_size_upper_bound
>   (Positive width) (Positive tab) xs =
>   provisio
>     [ ("input not empty", not $ isEmptyRLE xs)
>     , ("width >= 2", width >= 2)
>     ] $
>   let
>     (_,bs) = takeChunks width tab xs
>     w = spanWidth tab bs
>   in
>     case lastSpanWithPositiveWidth bs of
>       Nothing       -> accept
>       Just Stretchy -> claimLEQ w (width + tab - 1)
>       Just _        -> claimLEQ w width
> 
> test_TakeChunks_rest_size_upper_bound :: TestTree
> test_TakeChunks_rest_size_upper_bound =
>   testKreb "rest size is bounded above"
>     prop_TakeChunks_rest_size_upper_bound



ScreenOffset is a monoid
------------------------

> test_ScreenOffset_Monoid :: TestTree
> test_ScreenOffset_Monoid =
>   testGroup "ScreenOffset is a monoid"
>     [ test_ScreenOffset_Monoid_laws "30/8" nat30 nat8
>     , test_ScreenOffset_Monoid_laws "30/4" nat30 nat4
>     , test_ScreenOffset_Monoid_laws "15/2" nat15 nat2
>     , test_ScreenOffset_Monoid_laws "8/2" nat8 nat2
>     , test_ScreenOffset_Monoid_laws "3/1" nat3 nat1
> 
>     , test_ScreenOffset_applyBlockOffset_examples
>     ]

> test_ScreenOffset_Monoid_laws
>   :: forall w t
>    . ( IsWidth w, IsTab t )
>   => String -> Proxy w -> Proxy t
>   -> TestTree
> test_ScreenOffset_Monoid_laws name pw pt =
>   let
>     w = toWidth pw
>     e = mempty :: ScreenOffset w t
>     op = (<>) :: ScreenOffset w t -> ScreenOffset w t -> ScreenOffset w t
>   in testGroup name
>     [ test_Semigroup_laws (Proxy :: Proxy (ScreenOffset w t))
>     , test_Monoid_laws (Proxy :: Proxy (ScreenOffset w t))
>     , testKreb "left identity is unique" $
>         check_prop_unique_left_neutral_for e op
>     , testKreb "right identity is unique" $
>         check_prop_unique_right_neutral_for e op
>     , test_Monoid_right_action_laws
>         (flip applyScreenOffset :: (Int, Int) -> ScreenOffset w t -> (Int, Int))
> 
>     , testKreb "well defined on valid coords" $
>         \(off :: ScreenOffset w t) ->
>           forEach (genValidCoords pw) prune $ \pos@(_,k) ->
>             let (x,y) = applyScreenOffset off pos in
>             (0 <= x) .&&. (x < w) .&&. (k <= y)
>     ]

> prop_ScreenOffset_applyBlockOffset_examples
>   :: Int -> Int
>   -> ( RunLengthEncoding Span
>      , (Int, Int)
>      , (Int, Int)
>      )
>   -> Check
> prop_ScreenOffset_applyBlockOffset_examples w t (rle, x, y) =
>   claimEqual y (applyBlockOffset w t rle x)
> 
> test_ScreenOffset_applyBlockOffset_examples :: TestTree
> test_ScreenOffset_applyBlockOffset_examples =
>   testGroup "applyBlockOffset examples"
>     [ testKrebCases "8/2"
>       (prop_ScreenOffset_applyBlockOffset_examples 8 2)
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
>   -> Check
> prop_ScreenOffset_eq_examples x y =
>   claimEqual x y
> 
> test_ScreenOffset_eq_examples :: TestTree
> test_ScreenOffset_eq_examples =
>   testGroup "Examples"
>     [ let
>         z = defNoNewlines nat3 nat1 [(1, Fixed1)]
>       in
>         testKreb "#1" $
>           prop_ScreenOffset_eq_examples
>             (defNoNewlines nat3 nat1 [(4, Fixed1)])
>             (z <> z <> z <> z)
> 
>     , let
>         z = defNoNewlines nat3 nat1 [(1, Fixed1)]
>         w = defWithNewlines nat3 nat1 [] 1 [(1, Fixed1)]
>       in
>         testGroup "#2"
>           [ testKreb "zzw" $
>             prop_ScreenOffset_eq_examples
>               (defWithNewlines nat3 nat1 [(2, Fixed1)] 1 [(1, Fixed1)])
>               (z <> z <> w)
> 
>           , testKreb "zzzw" $
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
>     , test_ScreenOffset_eq_examples
>     ]
