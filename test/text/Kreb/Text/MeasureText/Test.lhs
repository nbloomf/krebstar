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
>     [ prop_MeasureText_Monoid_laws "30/8" nat30 nat8
>     , prop_MeasureText_Monoid_laws "30/4" nat30 nat4
>     , prop_MeasureText_Monoid_laws "15/2" nat15 nat2
>     , prop_MeasureText_Monoid_laws "8/2" nat8 nat2
>     , prop_MeasureText_Monoid_laws "3/1" nat3 nat1
>     ]

> prop_MeasureText_Monoid_laws
>   :: forall w t
>    . ( IsWidth w, IsTab t )
>   => String -> Proxy w -> Proxy t
>   -> TestTree
> prop_MeasureText_Monoid_laws name _ _ =
>   testGroup name
>     [ test_Semigroup_laws (Proxy :: Proxy (MeasureText w t))
>     , test_Monoid_laws (Proxy :: Proxy (MeasureText w t))
>     ]



Test Suite
----------

> test_MeasureText :: TestTree
> test_MeasureText =
>   testGroup "MeasureText"
>     [ test_LineCol_Monoid
>     , test_MeasureText_Monoid
>     ]
