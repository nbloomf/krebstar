---
title: Kreb.Struct.OnePointedList
---



Contents
--------

* [Introduction](#introduction)
* [Exposed API](#exposed-api)
* [The Zipper](#the-zipper)
* [The Tape class](#the-tape-class)
* [Testing and debugging](#testing-and-debugging)



Introduction
============

In [Ned.Data.FingerTree](src/Ned/Data/FingerTree.html) we developed a list-like data structure with efficient amortized cons and splitting. Our goal is to use this structure as the basis for a text buffer. But before we get there we need something extra: a _zipper_. Zippers are a powerful technique for turning an algebraic data type into a new type with a distinguished 'pointer' position, or in our case, a _cursor_, where we have constant-time access to data. Think about how when editing text there's a special position in the character stream where any new edits happen; this is the cursor position, and the edits there need to be as efficient as possible.

This code uses the following extensions:

> {-# LANGUAGE
>     MultiParamTypeClasses
>   , FlexibleInstances
>   , ScopedTypeVariables
> #-}

These are necessary so we can use the `Valued` type class from `FingerTree` as a constraint, and because the `Tape` class introduced here has two parameters.



Exposed API
===========

> module Kreb.Struct.OnePointedList (
>   -- ** OnePointedList
>     OnePointedList(..)
>   , splitFTZ
>   , integrateFTZ
>   , remeasure
>   , empty
>   , valueAtHeadFTZ
>   , fmapList

>   , singleton
> 
>   -- ** The Tape Class
>   , mkTapeFTZ
>   , mkTapeFocusFTZ
>   , unTapeFTZ

>   , isEmpty
>   , isAtInit
>   , isAtLast

>   , moveToInit
>   , moveToLast
>   , movePointLeft
>   , movePointRight
> 
>   , readInit
>   , alterInit
>   , insertInit
>   , deleteInit
> 
>   , readLast
>   , alterLast
>   , insertLast
>   , deleteLast
> 
>   , readPoint
>   , alterPoint
>   , insertPointLeft
>   , insertPointRight 
>   , deletePointLeft
>   , deletePointRight
> 
>   -- ** Testing and Debugging
>   , toListDebugFTZ
>   , depthFTZ
>   , showInternalFTZ
>   , validateFTZ
> ) where
> 
> import qualified Data.Foldable as Fold
> import Data.List (unwords)
> 
> import Kreb.Check ( Arb(..), Prune(..), CoArb(..), pickFrom2 )
> import Kreb.Struct.FingerTree



The Zipper
==========

The general strategy for constructing a zipper over a type involves expressing it as a functor, calculusifying it, and taking the derivative. Our `FingerTree` type is a little odd though, because of its non-uniform recursion. Actually differentiating the type directly is surely possible, but complicated enough that I don't feel bad cheating a little bit here. Our `FingerTree` is isomorphic to the type of lists, so the derivative is isomorphic to pairs of lists. We can split this into two cases, where the list is either empty or not, like so.

> data OnePointedList m a
>   = Vacant
>   | Point (FingerTree m a, a, FingerTree m a)
>   deriving (Eq, Show)
> 
> instance
>   ( Arb a, Valued m a
>   ) => Arb (OnePointedList m a)
>   where
>     arb = pickFrom2
>       ( pure Vacant
>       , Point <$> arb
>       )
> 
> instance
>   ( Prune a, Valued m a
>   ) => Prune (OnePointedList m a)
>   where
>     prune w = case w of
>       Vacant -> []
>       Point z -> concat 
>         [ [ Vacant ]
>         , Point <$> prune z
>         ]
> 
> empty :: OnePointedList m a
> empty = Vacant
> 
> fmapList
>   :: forall m1 m2 a1 a2
>    . ( Valued m1 a1, Valued m2 a2 )
>   => (a1 -> a2) -> OnePointedList m1 a1 -> OnePointedList m2 a2
> fmapList f w = case w of
>   Vacant -> Vacant
>   Point (as, x, bs) ->
>     Point (fmapFT f as, f x, fmapFT f bs)

We can turn a zippered finger tree back into a normal finger tree, by "integrating". :)

> integrateFTZ
>   :: ( Valued m a )
>   => OnePointedList m a -> FingerTree m a
> integrateFTZ w = case w of
>   Vacant -> mempty
>   Point (as, x, bs) -> as <> (cons x bs)

And the signature of `splitFT` is very close to returning a `OnePointedList`.

> splitFTZ
>   :: ( Valued m a )
>   => (m -> Bool)
>   -> FingerTree m a
>   -> OnePointedList m a
> splitFTZ p xs = case splitFT p xs of
>   Nothing -> Vacant
>   Just z -> Point z

Like `FingerTree`, zippered finger trees inherit a `Valued` instance:

> instance
>   ( Valued m a
>   ) => Valued m (OnePointedList m a)
>   where
>     value w = case w of
>       Vacant -> mempty
>       Point (as, x, bs) -> mconcat
>         [ value as, value x, value bs ]

Now for a couple of handy value related functions. First we will often need the accumulated monoid value at the read head.

> valueAtHeadFTZ
>   :: ( Valued m a )
>   => OnePointedList m a -> Maybe m
> valueAtHeadFTZ w = case w of
>   Vacant -> Nothing
>   Point (as, x, _) -> Just (value as <> value x)

Next, just as we could adjust the value monoid type on the fly for finger trees, we can for zipped trees.

> remeasure
>   :: ( Valued m1 a, Valued m2 a )
>   => OnePointedList m1 a -> OnePointedList m2 a
> remeasure w = case w of
>   Vacant -> Vacant
>   Point (as,x,bs) ->
>     Point (remeasureFT as, x, remeasureFT bs)



The Tape class
==============

Note that the zippered finger tree is like a list with a pointer into a specific index, where access is cheap. Moreover the properties of finger trees allow moving the pointer left and right and seeking to the beginning or end of the list is also cheap. This is something like magnetic _tape_, where we have a single read head that can see only a small part of the tape at once and by spooling the tape can move the head around from left to right.

(The tape analogy isn't perfect because efficient splitting on finger trees gives us something like logarithmic random access, but it goes far enough.)

We'll want to implement a couple of different linear structures on top of `OnePointedList` by choosing a specific value monoid, but in each case we'll have this linear read-head like structure. So we'll wrap an interface for this into the `Tape` type class.

A `Tape` has three special positions where access is cheap: the _initial_ element, the _last_ element, and the _head_ where the pointer points. We can move the head to the initial or last position, and can move the read head left or right. For any position we can read, alter, or delete the value there, with two variants of these for the read head depending on what direction the head should shift after the action is done.

Due to limitations in the semantics of type classes, we can't actually give an instance of `Tape` for `OnePointedList`. (The problem is that weird multi parameter type class, `Valued`.) Instead, we can give ad-hoc implementations of these functions with `Valued` constraints in the signature and use those to give `Tape` instances for individual types based on `OnePointedList` with the value type fixed.

> singleton
>   :: ( Valued m a )
>   => a -> OnePointedList m a
> singleton x =
>   Point (mempty, x, mempty)

> mkTapeFTZ
>   :: ( Valued m a )
>   => [a] -> OnePointedList m a
> mkTapeFTZ xs =
>   case uncons (fromListFT xs) of
>     Nothing -> Vacant
>     Just (a,as) -> Point (mempty, a, as)
> 
> mkTapeFocusFTZ
>   :: ( Valued m a )
>   => [a] -> a -> [a] -> OnePointedList m a
> mkTapeFocusFTZ as x bs =
>   Point (fromListFT as, x, fromListFT bs)

> unTapeFTZ
>   :: ( Valued m a )
>   => OnePointedList m a -> [a]
> unTapeFTZ w = case w of
>   Vacant -> []
>   Point (as,x,bs) -> Fold.concat
>     [ Fold.toList as, [x], Fold.toList bs ]

> isEmpty
>   :: OnePointedList m a -> Bool
> isEmpty w = case w of
>   Vacant -> True
>   _ -> False

> isAtInit
>   :: ( Valued m a )
>   => OnePointedList m a -> Bool
> isAtInit w = case w of
>   Vacant -> False
>   Point (as,_,_) -> isEmptyFT as
> 
> isAtLast
>   :: ( Valued m a )
>   => OnePointedList m a -> Bool
> isAtLast w = case w of
>   Vacant -> False
>   Point (_,_,bs) -> isEmptyFT bs

> readInit
>   :: ( Valued m a )
>   => OnePointedList m a -> Maybe a
> readInit w = case w of
>   Vacant -> Nothing
>   Point (as, x, bs) -> case uncons as of
>     Nothing -> Just x
>     Just (a, _) -> Just a
> 
> alterInit
>   :: ( Valued m a )
>   => (a -> a)
>   -> OnePointedList m a -> OnePointedList m a
> alterInit f w = case w of
>   Vacant -> Vacant
>   Point (as, x, bs) -> case uncons as of
>     Nothing ->
>       Point (mempty, f x, bs)
>     Just (a, as') ->
>       Point (cons (f a) as', x, bs)

> moveToInit  
>   :: ( Valued m a )
>   => OnePointedList m a -> OnePointedList m a
> moveToInit w = case w of
>   Vacant -> Vacant
>   Point (as, x, bs) -> case uncons as of
>     Nothing ->
>       Point (mempty, x, bs)
>     Just (a, as') ->
>       Point (mempty, a, as' <> (cons x bs))

> insertInit
>   :: ( Valued m a )
>   => a -> OnePointedList m a -> OnePointedList m a
> insertInit a w = case w of
>   Vacant ->
>     Point (mempty, a, mempty)
>   Point (as, x, bs) ->
>     Point (cons a as, x, bs)
> 
> deleteInit
>   :: ( Valued m a )
>   => OnePointedList m a -> OnePointedList m a
> deleteInit w = case w of
>   Vacant -> Vacant
>   Point (as, x, bs) -> case uncons as of
>     Just (_,as') -> Point (as', x, bs)
>     Nothing -> case uncons bs of
>       Nothing -> Vacant
>       Just (b,bs') -> Point (mempty, b, bs')

> readLast
>   :: ( Valued m a )
>   => OnePointedList m a -> Maybe a
> readLast w = case w of
>   Vacant -> Nothing
>   Point (as, x, bs) -> case unsnoc bs of
>     Nothing -> Just x
>     Just (b,_) -> Just b

> moveToLast
>   :: ( Valued m a )
>   => OnePointedList m a -> OnePointedList m a
> moveToLast w = case w of
>   Vacant -> Vacant
>   Point (as, x, bs) -> case unsnoc bs of
>     Nothing ->
>       Point (as, x, mempty)
>     Just (b, bs') -> 
>       Point (as <> (cons x bs'), b, mempty)

> alterLast
>   :: ( Valued m a )
>   => (a -> a)
>   -> OnePointedList m a -> OnePointedList m a
> alterLast f w = case w of
>   Vacant -> Vacant
>   Point (as, x, bs) -> case unsnoc bs of
>     Nothing ->
>       Point (as, f x, mempty)
>     Just (b,bs') ->
>       Point (as, x, snoc (f b) bs')

> insertLast
>   :: ( Valued m a )
>   => a -> OnePointedList m a -> OnePointedList m a
> insertLast a w = case w of
>   Vacant ->
>     Point (mempty, a, mempty)
>   Point (as, x, bs) ->
>     Point (as, x, snoc a bs)

> deleteLast
>   :: ( Valued m a )
>   => OnePointedList m a -> OnePointedList m a
> deleteLast w = case w of
>   Vacant -> Vacant
>   Point (as, x, bs) -> case unsnoc bs of
>     Nothing -> case unsnoc as of
>       Nothing -> Vacant
>       Just (a, as') -> Point (as', a, mempty)
>     Just (_, bs') -> Point (as, x, bs')

> readPoint
>   :: OnePointedList m a -> Maybe a
> readPoint w = case w of
>   Vacant -> Nothing
>   Point (_,x,_) -> Just x
> 
> alterPoint
>   :: ( Valued m a )
>   => (a -> a)
>   -> OnePointedList m a -> OnePointedList m a
> alterPoint f w = case w of
>   Vacant -> Vacant
>   Point (as, x, bs) -> Point (as, f x, bs)

> movePointLeft
>   :: ( Valued m a )
>   => OnePointedList m a -> OnePointedList m a
> movePointLeft w = case w of
>   Vacant -> Vacant
>   Point (as, x, bs) -> case unsnoc as of
>     Nothing ->
>       Point (mempty, x, bs)
>     Just (a, as') ->
>       Point (as', a, cons x bs)

> movePointRight
>   :: ( Valued m a )
>   => OnePointedList m a -> OnePointedList m a
> movePointRight w = case w of
>   Vacant -> Vacant
>   Point (as, x, bs) -> case uncons bs of
>     Nothing ->
>       Point (as, x, mempty)
>     Just (b, bs') ->
>       Point (snoc x as, b, bs')

> insertPointLeft
>   :: ( Valued m a )
>   => a -> OnePointedList m a -> OnePointedList m a
> insertPointLeft a w = case w of
>   Vacant -> Point (mempty, a, mempty)
>   Point (as, x, bs) -> Point (snoc a as, x, bs)

> insertPointRight
>   :: ( Valued m a )
>   => a -> OnePointedList m a -> OnePointedList m a
> insertPointRight a w = case w of
>   Vacant -> Point (mempty, a, mempty)
>   Point (as, x, bs) -> Point (as, x, cons a bs)

> deletePointRight
>   :: ( Valued m a )
>   => OnePointedList m a -> OnePointedList m a
> deletePointRight w = case w of
>   Vacant -> Vacant
>   Point (as, x, bs) -> case uncons bs of
>     Just (b,bs') -> Point (as, x, bs')
>     Nothing -> case unsnoc as of
>       Just (a,as') -> Point (as', a, mempty)
>       Nothing -> Vacant

> deletePointLeft
>   :: ( Valued m a )
>   => OnePointedList m a -> OnePointedList m a
> deletePointLeft w = case w of
>   Vacant -> Vacant
>   Point (as, x, bs) -> case unsnoc as of
>     Just (a,as') -> Point (as', x, bs)
>     Nothing -> case uncons bs of
>       Just (b,bs') -> Point (mempty, b, bs')
>       Nothing -> Vacant



Testing and debugging
=====================

> depthFTZ
>   :: ( Valued m a )
>   => OnePointedList m a -> Int
> depthFTZ = depthFT . integrateFTZ

> toListDebugFTZ
>   :: ( Valued m a )
>   => OnePointedList m a -> [(a,m)]
> toListDebugFTZ w = case w of
>   Vacant -> []
>   Point (as, x, bs) ->
>     toListDebugFT (as <> cons x bs)

> showInternalFTZ
>   :: ( Valued m a, Show m, Show a )
>   => OnePointedList m a -> String
> showInternalFTZ w = case w of
>   Vacant -> "OnePointedList Nothing"
>   Point (as,x,bs) ->
>     let
>       p cs = if elem ' ' cs
>         then concat ["(",cs,")"]
>         else cs
>     in
>       unwords
>         [ "Just", "("
>         , p $ (showInternalFT as) ++ ","
>         , p $ (show x) ++ ","
>         , p $ (showInternalFT bs) ++ ")"
>         ]

> validateFTZ
>   :: ( Eq m, Valued m a )
>   => OnePointedList m a -> Bool
> validateFTZ w = case w of
>   Vacant ->
>     True
>   Point (as, _, bs) ->
>       (validateFT as) && (validateFT bs)
