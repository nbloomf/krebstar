---
title: Kreb.Text.MeasureText.Test
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
> module Kreb.Text.MeasureText.Test (
>     test_MeasureText
> ) where
> 
> import Data.Proxy
> import Data.List
> 
> import Test.Tasty
> 
> import Kreb.Check
> import Kreb.Reflect.Nat
> import Kreb.Text.ScreenOffset
> import Kreb.Text.MeasureText
> import Kreb.Text.Rune
> 
> import Kreb.Text.ScreenOffset.Test




LineCol is a monoid
-------------------

> test_LineCol_Monoid :: TestTree
> test_LineCol_Monoid =
>   testGroup "LineCol is a monoid"
>     [ test_Semigroup_laws (Proxy :: Proxy LineCol)
>     , test_Monoid_laws (Proxy :: Proxy LineCol)
>     ]



MeasureText is a monoid
-----------------------

> test_MeasureText_Monoid :: TestTree
> test_MeasureText_Monoid =
>   testGroup "MeasureText is a monoid"
>     [ prop_MeasureText_Monoid_laws "30/8/10" nat30 nat8 nat10
>     , prop_MeasureText_Monoid_laws "30/4/10" nat30 nat4 nat10
>     , prop_MeasureText_Monoid_laws "15/2/10" nat15 nat2 nat10
>     , prop_MeasureText_Monoid_laws "8/2/10"  nat8  nat2 nat10
>     , prop_MeasureText_Monoid_laws "3/1/10"  nat3  nat1 nat10
> 
>     , prop_MeasureText_Monoid_laws "30/8/100" nat30 nat8 nat100
>     , prop_MeasureText_Monoid_laws "30/4/100" nat30 nat4 nat100
>     , prop_MeasureText_Monoid_laws "15/2/100" nat15 nat2 nat100
>     , prop_MeasureText_Monoid_laws "8/2/100"  nat8  nat2 nat100
>     , prop_MeasureText_Monoid_laws "3/1/100"  nat3  nat1 nat100
>     ]

> prop_MeasureText_Monoid_laws
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => String -> Proxy w -> Proxy t -> Proxy d
>   -> TestTree
> prop_MeasureText_Monoid_laws name _ _ _ =
>   testGroup name
>     [ test_Semigroup_laws (Proxy :: Proxy (MeasureText w t d))
>     , test_Monoid_laws (Proxy :: Proxy (MeasureText w t d))
>     ]



Test Suite
----------

> test_MeasureText :: TestTree
> test_MeasureText =
>   testGroup "MeasureText"
>     [ test_LineCol_Monoid
>     , test_MeasureText_Monoid
>     ]
