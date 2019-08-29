---
title: Ned.Data.MeasureText.Test
---



Contents
--------

* [Introduction](#introduction)
* [Generators](#generators)
* [Tests](#tests)
    * [LineCol is a monoid](#linecol-is-a-monoid)
    * [MeasureText is a monoid](#measuretext-is-a-monoid)
    * [Test suite](#test-suite)



Introduction
============

> {-# LANGUAGE
>     ScopedTypeVariables
> #-}
> 
> module Ned.Data.MeasureText.Test (
>     test_MeasureText
> ) where
> 
> import Data.Proxy
> import Data.List
> 
> import Test.QuickCheck
> import Test.Tasty
> import Test.Tasty.QuickCheck
> 
> import Kreb.Reflect.Nat
> import Ned.Data.ScreenOffset
> import Ned.Data.MeasureText
> 
> import Ned.Data.ScreenOffset.Test



Generators
==========

> instance
>   Arbitrary LineCol
>   where
>     arbitrary = do
>       NonNegative l <- arbitrary
>       NonNegative c <- arbitrary
>       return $ LineCol
>         { lineNum = l
>         , colNum  = c
>         }

> instance
>   ( IsWidth w, IsTab t
>   ) => Arbitrary (MeasureText w t)
>   where
>     arbitrary = do
>       NonNegative c <- arbitrary
>       NonNegative b <- arbitrary
>       NonNegative h <- arbitrary
>       NonNegative k <- arbitrary
>       lc <- arbitrary
>       so <- arbitrary
>       sc <- arbitrary
>       let w = toWidth (Proxy :: Proxy w)
>       q <- arbitrary
>       return $ MeasureText
>         { charCount          = c
>         , byteCount          = b
>         , logicalOffset      = lc
>         , logicalCoords      = LineCol h k
>         , screenOffset       = so
>         , screenCoords       = sc
>         , hasEOF             = False
>         , hasTrailingNewline = q
>         }



Tests
=====

LineCol is a monoid
-------------------

> test_LineCol_Monoid :: TestTree
> test_LineCol_Monoid =
>   testGroup "LineCol is a monoid"
>     [ testProperty "Left identity" $
>         \(x :: LineCol) ->
>           x === (mempty <> x)
>     , testProperty "Right identity" $
>         \(x :: LineCol) ->
>           x === (x <> mempty)
>     , testProperty "Associativity" $
>         \(x :: LineCol, y, z) ->
>           (x <> (y <> z)) === ((x <> y) <> z)
>     ]



MeasureText is a monoid
-----------------------

> test_MeasureText_Monoid :: TestTree
> test_MeasureText_Monoid =
>   testGroup "MeasureText is a monoid"
>     [ test_MeasureText_Monoid_left_identity
>     , test_MeasureText_Monoid_right_identity
>     , test_MeasureText_Monoid_associativity
>     ]

Left identity law:

> prop_MeasureText_Monoid_left_identity
>   :: forall w t
>    . ( IsWidth w, IsTab t )
>   => Proxy w -> Proxy t
>   -> MeasureText w t
>   -> Property
> 
> prop_MeasureText_Monoid_left_identity _ _ x =
>   property $ x === mempty <> x
> 
> test_MeasureText_Monoid_left_identity :: TestTree
> test_MeasureText_Monoid_left_identity =
>   testGroup "Left identity law"
>     [ testProperty "30/8" $
>         prop_MeasureText_Monoid_left_identity nat30 nat8
>     , testProperty "30/4" $
>         prop_MeasureText_Monoid_left_identity nat30 nat4
>     , testProperty "15/2" $
>         prop_MeasureText_Monoid_left_identity nat15 nat2
>     , testProperty "8/2" $
>         prop_MeasureText_Monoid_left_identity nat8 nat2
>     ]

Right identity law:

> prop_MeasureText_Monoid_right_identity
>   :: forall w t
>    . ( IsWidth w, IsTab t )
>   => Proxy w -> Proxy t
>   -> MeasureText w t
>   -> Property
> 
> prop_MeasureText_Monoid_right_identity _ _ x =
>   property $ x === mempty <> x
> 
> test_MeasureText_Monoid_right_identity :: TestTree
> test_MeasureText_Monoid_right_identity =
>   testGroup "right identity law"
>     [ testProperty "30/8" $
>         prop_MeasureText_Monoid_right_identity nat30 nat8
>     , testProperty "30/4" $
>         prop_MeasureText_Monoid_right_identity nat30 nat4
>     , testProperty "15/2" $
>         prop_MeasureText_Monoid_right_identity nat15 nat2
>     , testProperty "8/2" $
>         prop_MeasureText_Monoid_right_identity nat8 nat2
>     ]

Associativity law:

> prop_MeasureText_Monoid_associativity
>   :: forall w t
>    . ( IsWidth w, IsTab t )
>   => Proxy w -> Proxy t
>   -> MeasureText w t
>   -> MeasureText w t
>   -> MeasureText w t
>   -> Property
> 
> prop_MeasureText_Monoid_associativity _ _ x y z =
>   property $
>     ((x <> y) <> z)
>       === (x <> (y <> z))
> 
> test_MeasureText_Monoid_associativity :: TestTree
> test_MeasureText_Monoid_associativity =
>   testGroup "associativity law"
>     [ testProperty "30/8" $
>         prop_MeasureText_Monoid_associativity nat30 nat8
>     , testProperty "30/4" $
>         prop_MeasureText_Monoid_associativity nat30 nat4
>     , testProperty "15/2" $
>         prop_MeasureText_Monoid_associativity nat15 nat2
>     , testProperty "8/2" $
>         prop_MeasureText_Monoid_associativity nat8 nat2
>     ]



Test Suite
----------

> test_MeasureText :: TestTree
> test_MeasureText =
>   testGroup "MeasureText"
>     [ test_LineCol_Monoid
>     , localOption (QuickCheckTests 1000)
>         $ test_MeasureText_Monoid
>     ]
