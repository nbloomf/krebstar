---
title: One-Pointed Lists
---

::: contents
* [The Derivative of a List](#the-derivative-of-a-list)
* [Queries](#queries)
:::



::: frontmatter

> {-# LANGUAGE
>     MultiParamTypeClasses
>   , FlexibleInstances
>   , ScopedTypeVariables
> #-}

> module Kreb.Struct.OnePointedList where
> 
> import qualified Data.Foldable as Fold
> import Data.List (unwords)
> 
> import Kreb.Check
> import Kreb.Struct.FingerTree

:::



The Derivative of a List
------------------------

Algebraic types are a powerful tool for modeling data, but they do have some tradeoffs. One of the most painful is a general lack of _random access_. As an example consider the standard implementation of cons-lists.

::: example
``` haskell
data List a = Nil | Cons a (List a)
```
:::

Suppose we have a specific nonempty list of `a`s constructed via nested applications of `Cons`. There's exactly one entry in this list that we're guaranteed to have constant-effort access to: the first one. To get an arbitrary element of the list requires destructuring -- that is, peeling of list elements starting at the head. On average this will take $n/2$ destructurings where $n$ is the length of the list. This is okay for some applications, but there are times when we really need efficient random access.

Full-on random access lists are achievable in Haskell using, for instance, the `Array` interface, but this comes at a cost: we lose the benefits of algebraic types. If we're willing to settle for something between the extremes of "constant access only at the head" and "constant random access" there's another solution, called a _zipper_, that looks something like this.

::: example
``` haskell
data Zipper a = Nada | Zipper (List a) a (List a)
```
:::

Note first of all that `Zipper` is defined in terms of `List`. We can interpret this definition as follows: a `Zipper` is either empty, or it has a special value `a` together with a list of values to the left and a list of values to the right. Collectively these are equivalent to a single list except that the special value acts like a _pointer_ to some position in the middle of the list. Moreover, we can move this pointer to the left or right with constant effort.

::: example
``` haskell
moveLeft :: Zipper a -> Zipper a
moveLeft z = case z of
  Nada -> Nada
  Zipper as x bs -> case as of
    [] -> Zipper [] x bs
    u:us -> Zipper us u (x:bs)
```
:::

So `Zipper` is not quite as nice as a random access array, but we've traded the expensive access pattern of `List` for a kind of constant-effort _read head_ that can move about the list. The major advantage of `Zipper` is that it is an algebraic type, making it easier to reason about.

Here's the big idea: the shift from `List` to `Zipper` is just one example of a more general pattern for constructing navigable types like this. The general pattern applies to any polynomial type -- that is, built out of constants, type variables, products, and sums -- and this pattern is based on calculus. The elevator pitch is that if we have an algebraic type given by a polynomial functor, we can treat the definition like a high school algebra style implicit function definition. Then the _derivative_ of this function is again a polynomial functor and represents _one-hole contexts_ over the original type.

In this module we will develop such a type from scratch, called `OnePointedList`. Specifically, we'll define something analogous to `Zipper`, but using `FingerTree` as the underlying list representation rather than naive cons-lists. The definition is pretty simple: a `OnePointedList` is either `Vacant`, or has a special value called the _point_ together with finger trees of values to the left and to the right of the point.

> data OnePointedList m a
>   = Vacant
>   | Point (FingerTree m a, a, FingerTree m a)
>   deriving (Eq, Show)

(Note that, since this is defined in terms of `FingerTree`, we have an extra value type parameter `m`.) `OnePointedList` combines the benefits of `Zipper` with the efficient splitting and deque access of `FingerTree`. Recall that we have amortized constant time access to both ends of a finger tree, so `OnePointedList` gives us fast access to the beginning and end of the list in addition to the read head.

As usual, we'll build up a theory of `OnePointedList`s by defining queries and operations on them. First though we need some constructors. Two natural candidates are `empty` and `singleton`.

> empty
>   :: OnePointedList m a
> empty = Vacant
> 
> singleton
>   :: ( Valued m a )
>   => a -> OnePointedList m a
> singleton x =
>   Point (mempty, x, mempty)

It's also natural to convert lists to `OnePointedList`s; we'll do this by interpreting the head of the list as the point.

> makeFromList
>   :: ( Valued m a )
>   => [a] -> OnePointedList m a
> makeFromList xs = case xs of
>   [] -> Vacant
>   x:xs -> Point (mempty, x, fromListFT xs)



Queries
-------

> isEmpty
>   :: OnePointedList m a -> Bool
> isEmpty w = case w of
>   Vacant -> True
>   _ -> False

::: doctest

> -- $
> -- >>> isEmpty empty
> -- True
> --
> -- >>> :{
> -- let
> --   x :: OnePointedList Count Char
> --   x = singleton 'a'
> -- in isEmpty x
> -- :}
> -- False
> --
> -- >>> let x = makeFromList [] :: OnePointedList Count Char
> -- >>> isEmpty x
> -- True
> --
> -- >>> let x = makeFromList ['a','b'] :: OnePointedList Count Char
> -- >>> isEmpty x
> -- False

:::

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

> readLast
>   :: ( Valued m a )
>   => OnePointedList m a -> Maybe a
> readLast w = case w of
>   Vacant -> Nothing
>   Point (as, x, bs) -> case unsnoc bs of
>     Nothing -> Just x
>     Just (b,_) -> Just b

> readPoint
>   :: OnePointedList m a -> Maybe a
> readPoint w = case w of
>   Vacant -> Nothing
>   Point (_,x,_) -> Just x







Constructors
------------



> instance
>   ( Valued m a
>   ) => Valued m (OnePointedList m a)
>   where
>     value w = case w of
>       Vacant -> mempty
>       Point (as, x, bs) -> mconcat
>         [ value as, value x, value bs ]
> 
> valueAtPoint
>   :: ( Valued m a )
>   => OnePointedList m a -> Maybe m
> valueAtPoint w = case w of
>   Vacant -> Nothing
>   Point (as, x, _) ->
>     Just (value as <> value x)

> integrate
>   :: ( Valued m a )
>   => OnePointedList m a -> FingerTree m a
> integrate w = case w of
>   Vacant -> mempty
>   Point (as, x, bs) -> as <> (cons x bs)







Navigation
----------

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



Mutation
--------

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


> 
> alterPoint
>   :: ( Valued m a )
>   => (a -> a)
>   -> OnePointedList m a -> OnePointedList m a
> alterPoint f w = case w of
>   Vacant -> Vacant
>   Point (as, x, bs) -> Point (as, f x, bs)



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



Testing and Debugging
---------------------

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

> makePoint
>   :: ( Valued m a )
>   => [a] -> a -> [a]
>   -> OnePointedList m a
> makePoint as x bs =
>   Point (fromListFT as, x, fromListFT bs)





Contents
--------

* [Introduction](#introduction)
* [Exposed API](#exposed-api)
* [Testing and debugging](#testing-and-debugging)



Introduction
============

In [Ned.Data.FingerTree](src/Ned/Data/FingerTree.html) we developed a list-like data structure with efficient amortized cons and splitting. Our goal is to use this structure as the basis for a text buffer. But before we get there we need something extra: a _zipper_. Zippers are a powerful technique for turning an algebraic data type into a new type with a distinguished 'pointer' position, or in our case, a _cursor_, where we have constant-time access to data. Think about how when editing text there's a special position in the character stream where any new edits happen; this is the cursor position, and the edits there need to be as efficient as possible.

This code uses the following extensions:





The Zipper
==========

The general strategy for constructing a zipper over a type involves expressing it as a functor, calculusifying it, and taking the derivative. Our `FingerTree` type is a little odd though, because of its non-uniform recursion. Actually differentiating the type directly is surely possible, but complicated enough that I don't feel bad cheating a little bit here. Our `FingerTree` is isomorphic to the type of lists, so the derivative is isomorphic to pairs of lists. We can split this into two cases, where the list is either empty or not, like so.


> 
> 

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



Now for a couple of handy value related functions. First we will often need the accumulated monoid value at the read head.



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



> unTapeFTZ
>   :: ( Valued m a )
>   => OnePointedList m a -> [a]
> unTapeFTZ w = case w of
>   Vacant -> []
>   Point (as,x,bs) -> Fold.concat
>     [ Fold.toList as, [x], Fold.toList bs ]








Testing and debugging
=====================

> depthFTZ
>   :: ( Valued m a )
>   => OnePointedList m a -> Int
> depthFTZ = depthFT . integrate

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
