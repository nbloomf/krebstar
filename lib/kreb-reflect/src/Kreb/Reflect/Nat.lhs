---
title: Reflecting Natural Numbers
author: nbloomf
---

::: frontmatter



:::



Contents
--------

* [Introduction](#introduction)
* [Type level natural numbers](#type-level-natural-numbers)



Introduction
============

This module implements reified type-level natural numbers which we can configure dynamically at runtime. This code is cribbed from the paper _Functional Pearl: Implicit Configuration_ by Kiselyov and Shan, which I highly recommend. We're barely scratching the surface of the techniques that paper covers.

We'll be using this code later on to encode natural numbers in the type of a text buffer -- specifically, the line width of the screen for wrapping and the width of the tab stops.

> module Kreb.Reflect.Nat (
>     ReflectNat(..)
>   , reifyNat
>   , showNat
>   , Zero()
>   , Incr()
>   , Doub()
>   , Nat()
> 
>   , nat0
>   , nat1
>   , nat2
>   , nat3
>   , nat4
>   , nat5
>   , nat6
>   , nat7
>   , nat8
>   , nat9
>   , nat10
>   , nat11
>   , nat12
>   , nat13
>   , nat14
>   , nat15
>   , nat16
>   , nat17
>   , nat18
>   , nat19
>   , nat20
>   , nat21
>   , nat22
>   , nat23
>   , nat24
>   , nat25
>   , nat26
>   , nat27
>   , nat28
>   , nat29
>   , nat30
>   , nat31
>   , nat32
>   , nat33
>   , nat34
>   , nat35
>   , nat36
>   , nat37
>   , nat38

>   , nat50

>   , nat100

>   , Nat16
> ) where
> 
> import Data.Proxy



Type level natural numbers
--------------------------

First we give a basic binary encoding of natural numbers as types. Note that these types have no constructors, so they have no interesting values.

> data Zero
> data Incr a
> data Doub a

Next we have a class representing types which can be reflected back to a unique `Int` value.

> class ReflectNat a where
>   reflectNat :: Proxy a -> Int

And the natural number type constructors can be made instances of `ReflectNat` in the obvious way:

> instance
>   ReflectNat Zero
>   where
>     reflectNat _ = 0
> 
> instance
>   ( ReflectNat a
>   ) => ReflectNat (Incr a)
>   where
>     reflectNat :: Proxy (Incr a) -> Int
>     reflectNat _ =
>       1 + (reflectNat (Proxy :: Proxy a))
> 
> instance
>   ( ReflectNat a
>   ) => ReflectNat (Doub a)
>   where
>     reflectNat :: Proxy (Doub a) -> Int
>     reflectNat _ =
>       2 * (reflectNat (Proxy :: Proxy a))

Now for the magic step.

Given a function with signature `Proxy p -> a`, where `p` is one of our _type_ naturals, we want to be able to apply this function to a specific type that is only known at runtime. The process of turning a runtime `Int` into a type is called _reification_, and this rank 2 type can do it.

> reifyNat
>   :: Int
>   -> (forall p. ReflectNat p => Proxy p -> a)
>   -> a
> reifyNat m cont = case quotRem m 2 of
>   (0,0) -> cont (Proxy :: Proxy Zero)
>   (k,0) -> reifyNat k $
>     \(_ :: Proxy u) -> cont (Proxy :: Proxy (Doub u))
>   (k,1) -> reifyNat k $
>     \(_ :: Proxy u) -> cont (Proxy :: Proxy (Incr (Doub u)))
>   _     -> cont (Proxy :: Proxy Zero)

Note that neither the caller of `reifyNat`, nor the callback `cont` argument, need to know exactly what `p` is. The actual reified type is completely hidden. So we get the best of two worlds: we can write type-safe code depending on natural number parameters, but can _use_ this code as if the types aren't there. Of course this strategy comes with tradeoffs, but it opens up a large new design space.

> data Nat p

> showNat
>   :: forall p. ( ReflectNat p ) => Proxy (Nat p) -> String
> showNat _ = "nat" ++ show (reflectNat (Proxy :: Proxy p))

Some constants:

> zero :: Proxy (Nat Zero)
> zero = Proxy
> 
> incr :: Proxy (Nat p) -> Proxy (Nat (Incr p))
> incr _ = Proxy
> 
> doub :: Proxy (Nat p) -> Proxy (Nat (Doub p))
> doub _ = Proxy

> nat0  = zero
> nat1  = incr zero
> nat2  = doub nat1
> nat3  = incr nat2
> nat4  = doub nat2
> nat5  = incr nat4
> nat6  = doub nat3
> nat7  = incr nat6
> nat8  = doub nat4
> nat9  = incr nat8
> nat10 = doub nat5
> nat11 = incr nat10
> nat12 = doub nat6
> nat13 = incr nat12
> nat14 = doub nat7
> nat15 = incr nat14
> nat16 = doub nat8
> nat17 = incr nat16
> nat18 = doub nat9
> nat19 = incr nat18
> nat20 = doub nat10
> nat21 = incr nat20
> nat22 = doub nat11
> nat23 = incr nat22
> nat24 = doub nat12
> nat25 = incr nat24
> nat26 = doub nat13
> nat27 = incr nat26
> nat28 = doub nat14
> nat29 = incr nat28
> nat30 = doub nat15
> nat31 = incr nat30
> nat32 = doub nat16
> nat33 = incr nat32
> nat34 = doub nat17
> nat35 = incr nat34
> nat36 = doub nat18
> nat37 = incr nat36
> nat38 = doub nat19

> nat50 = doub nat25

> nat100 = doub nat50

> type Nat16 = Doub (Doub (Doub (Doub (Incr Zero))))
