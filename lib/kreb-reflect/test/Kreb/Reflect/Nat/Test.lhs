---
title: Kreb.Reflect.Nat.Test
---



Contents
--------

* [Introduction](#introduction)
* [Test](#tests)
    * [Test suite](#test-suite)



Introduction
============

> module Kreb.Reflect.Nat.Test (
>     test_ReflectNat
> ) where
> 
> import Test.Tasty
> 
> import Kreb.Prop
> import Kreb.Reflect.Nat



Tests
=====

At this stage there's not much we can do to test `ReflectNat`. An important test, though, is that reification and reflection should be "inverses" in this weird sense.

> check_ReflectNat_roundtrip
>   :: NonNegative Int -> Check
> check_ReflectNat_roundtrip (NonNegative k) =
>   claimEqual k (reifyNat k reflectNat)


Test Suite
----------

> test_ReflectNat :: TestTree
> test_ReflectNat =
>   testGroup "ReflectNat"
>     [ krebProp "k == reifyNat k reflectNat"
>         check_ReflectNat_roundtrip
>     ]
