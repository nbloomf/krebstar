---
title: Ned.Data.ReflectNat.Test
---



Contents
--------

* [Introduction](#introduction)
* [Test](#tests)
    * [Test suite](#test-suite)



Introduction
============

> module Ned.Data.ReflectNat.Test (
>     test_ReflectNat
> ) where
> 
> import Data.Proxy
> 
> import Test.QuickCheck
> import Test.Tasty
> import Test.Tasty.QuickCheck
> 
> import Ned.Data.ReflectNat



Tests
=====

At this stage there's not much we can do to test `ReflectNat`. An important test, though, is that reification and reflection should be "inverses" in this weird sense.

> prop_ReflectNat_roundtrip
>   :: Int -> Property
> prop_ReflectNat_roundtrip k =
>   (k >= 0) ==>
>   k === reifyNat k reflectNat


Test Suite
----------

> test_ReflectNat :: TestTree
> test_ReflectNat =
>   testGroup "ReflectNat"
>     [ testProperty "k == reifyNat k reflectNat"
>         prop_ReflectNat_roundtrip
>     ]
