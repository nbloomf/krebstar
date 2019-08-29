---
title: Kreb.Struct.Seq.Test
---



Contents
--------

* [Introduction](#introduction)
* [Generators](#generators)
* [Seq Tests](#seq-tests)



Introduction
============

There's not much to say about `Seq`.

> {-# LANGUAGE
>     UndecidableInstances
>   , FlexibleContexts
> #-}
> 
> module Kreb.Struct.Seq.Test (
>     test_Seq
> ) where
> 
> import Data.Proxy
> 
> import Test.QuickCheck
> import Test.Tasty
> import Test.Tasty.QuickCheck
> 
> import Kreb.Struct.FingerTree
> import Kreb.Struct.FingerTreeZip
> import Kreb.Struct.Seq
> 
> import Kreb.Check
> 
> import Kreb.Struct.FingerTree.Test
> import Kreb.Struct.FingerTreeZip.Test



Generators
==========

> instance
>   ( Arbitrary a, Valued Count a
>   ) => Arbitrary (Seq a) where
>   arbitrary = mkTapeFocus
>     <$> arbitrary
>     <*> arbitrary
>     <*> arbitrary
> 
> pSeq :: Proxy Seq
> pSeq = Proxy



Seq Tests
=========

We get all our `Seq` tests for free.

> test_Seq :: TestTree
> test_Seq =
>   testGroup "Seq"
>     [ testGroup "Char"
>       [ test_Tape pChar pSeq ]
>     ]
