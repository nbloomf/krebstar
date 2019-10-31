---
title: Sequences
---

::: contents
* [Introduction](#introduction): Specializing one-pointed lists
* [Class instances](#class-instances): Code for free
* [Queries](#queries): Getting information out of a sequence
* [Navigation](#navigation): Moving around
* [Mutation](#mutation): Making edits
* [Concatenation](#concatenation): Putting sequences together
* [Testing and debugging](#testing-and-debugging): For when things go wrong
:::



::: frontmatter

> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE InstanceSigs #-}
> 
> module Kreb.Struct.Sequence (
>     Sequence()
>   , empty
>   , singleton
>   , fromList
> 
>   , isEmpty
>   , isSingleton
>   , isAtInit
>   , isAtLast
>   , readPoint
>   , readInit
>   , readLast
>   , getLength
> 
>   , movePointLeft
>   , movePointRight
>   , moveToInit
>   , moveToLast
>   , moveToIndex
> 
>   , insertInit
>   , deleteInit
>   , insertLast
>   , deleteLast
>   , insertPointLeft
>   , deletePointLeft
>   , insertPointRight
>   , deletePointRight
>   , alterInit
>   , alterLast
>   , alterPoint
> 
>   , prepend
>   , append
> 
>   , debugShowSequence
> ) where
> 
> import Data.Foldable
> 
> import           Kreb.Check
> import           Kreb.Struct.Valued
> import qualified Kreb.Struct.OnePointedList as OPL

:::



Introduction
------------

So far we've developed the finger tree data type, and taken its derivative to get a linear structure with a distinguished and movable pointer somewhere in the middle. We'll use this as the basis for a more specialized structure: _sequences_. This type uses our existing `Count` monoid to describe a list with efficient indexing.

> newtype Sequence a = Sequence
>   { unSequence :: OPL.OnePointedList Count (Item a)
>   } deriving (Eq, Show)

The `Item` constructor allows us to hide the otherwise awkward `Valued` constraint from consumers of this module; because it is implemented as a newtype, it has no runtime overhead.

> newtype Item a = Item
>   { unItem :: a
>   } deriving (Eq, Show)
> 
> instance Valued Count (Item a) where
>   value _ = Count 1
> 
> instance Functor Item where
>   fmap f (Item x) = Item (f x)

Nearly all of the functionality we want out of sequences is already provided by one-pointed lists, so we can just lift the API of that module to work over the `Sequence` type. First are the basic constructors.

> empty :: Sequence a
> empty = Sequence OPL.empty
> 
> singleton
>   :: a -> Sequence a
> singleton = Sequence . OPL.singleton . Item
> 
> fromList
>   :: [a] -> Sequence a
> fromList = Sequence . OPL.makeFromList . fmap Item

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x :: Sequence Int
> --   x = singleton 5
> -- in x == empty
> -- :}
> -- False

:::

We also expose a constructor that gives us control over the location of the point; this is only used for testing and debugging.

> makePointSequence
>   :: [a] -> a -> [a]
>   -> Sequence a
> makePointSequence as x bs =
>   Sequence $ OPL.makePoint
>     (fmap Item as) (Item x) (fmap Item bs)



Class instances
---------------

We have a `Foldable` instance for `Sequence`:

> instance Foldable Sequence where
>   toList =
>     fmap unItem . toList . unSequence
> 
>   foldr f e =
>     let g (Item a) (Item b) = Item (f a b)
>     in unItem . foldr g (Item e) . unSequence
> 
>   foldl f e =
>     let g (Item a) (Item b) = Item (f a b)
>     in unItem . foldl g (Item e) . unSequence

Because we've hidden the `Valued` constraint, we can give a real `Functor` instance for sequences.

> instance Functor Sequence where
>   fmap f (Sequence x) =
>     Sequence $ OPL.fmapList (fmap f) x

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x, y :: Sequence Int
> --   x = fromList [1,2,3]
> --   y = fromList [2,4,6]
> -- in y == fmap (*2) x
> -- :}
> -- True

:::



Queries
-------

Next we can get information out of a sequence. First are the basic predicates:

> isEmpty
>   :: Sequence a -> Bool
> isEmpty =
>   OPL.isEmpty . unSequence
> 
> isSingleton
>   :: Sequence a -> Bool
> isSingleton =
>   OPL.isSingleton . unSequence
> 
> isAtInit
>   :: Sequence a -> Bool
> isAtInit =
>   OPL.isAtInit . unSequence
> 
> isAtLast
>   :: Sequence a -> Bool
> isAtLast =
>   OPL.isAtLast . unSequence

And some examples for good measure.

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x :: Sequence Int
> --   x = makePointSequence
> --     [] 1 [2,3]
> -- in (isAtInit x, isAtLast x)
> -- :}
> -- (True,False)
> --
> -- >>> :{
> -- let
> --   x :: Sequence Int
> --   x = makePointSequence
> --     [1,2] 3 []
> -- in (isAtInit x, isAtLast x)
> -- :}
> -- (False,True)

:::

Then we have functions for extracting the value at the point and at the ends of the sequence.

> readInit
>   :: Sequence a -> Maybe a
> readInit =
>   fmap unItem . OPL.readInit . unSequence
> 
> readLast
>   :: Sequence a -> Maybe a
> readLast =
>   fmap unItem . OPL.readLast . unSequence
> 
> readPoint
>   :: Sequence a -> Maybe a
> readPoint =
>   fmap unItem . OPL.readPoint . unSequence

And an example.

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x :: Sequence Int
> --   x = makePointSequence
> --     [1,2] 3 [4,5,6]
> -- in (readInit x, readPoint x, readLast x)
> -- :}
> -- (Just 1,Just 3,Just 6)

:::

We'll sometimes also want to get the length of a sequence.

> getLength
>   :: Sequence a -> Int
> getLength (Sequence as) =
>   let Count k = value as
>   in k

And some examples.

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x :: Sequence Int
> --   x = fromList [1,2,3,4,5]
> -- in getLength x
> -- :}
> -- 5
> --
> -- >>> getLength (empty :: Sequence Int)
> -- 0

:::



Navigation
----------

Next we have the basic navigation primitives.

> movePointLeft
>   :: Sequence a -> Sequence a
> movePointLeft =
>   Sequence . OPL.movePointLeft . unSequence
> 
> movePointRight
>   :: Sequence a -> Sequence a
> movePointRight =
>   Sequence . OPL.movePointRight . unSequence
> 
> moveToInit
>   :: Sequence a -> Sequence a
> moveToInit =
>   Sequence . OPL.moveToInit . unSequence
> 
> moveToLast
>   :: Sequence a -> Sequence a
> moveToLast =
>   Sequence . OPL.moveToLast . unSequence

And some examples:

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x, y, z :: Sequence Int
> --   x = makePointSequence
> --     [1,2] 3 [4,5,6]
> --   y = makePointSequence
> --     [] 1 [2,3,4,5,6]
> --   z = makePointSequence
> --     [1,2,3,4,5] 6 []
> -- in (y == moveToInit x, z == moveToLast x)
> -- :}
> -- (True,True)
> --
> -- >>> :{
> -- let
> --   x, y, z :: Sequence Int
> --   x = makePointSequence
> --     [1,2] 3 [4,5,6]
> --   y = makePointSequence
> --     [1] 2 [3,4,5,6]
> --   z = makePointSequence
> --     [1,2,3] 4 [5,6]
> -- in (y == movePointLeft x, z == movePointRight x)
> -- :}
> -- (True,True)

:::

We can also navigate to an arbitrary index in the sequence, which is sort of the purpose of this type. :) Note that the indexing is zero-based.

> moveToIndex
>   :: Int -> Sequence a -> Sequence a
> moveToIndex k (Sequence as) = Sequence $
>   OPL.split
>     (\(Count i) -> i > k)
>     (OPL.integrate as)

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x :: Sequence Char
> --   x = fromList ['a','b','c','d','e']
> -- in readPoint (moveToIndex 0 x)
> -- :}
> -- Just 'a'
> --
> -- >>> :{
> -- let
> --   x :: Sequence Char
> --   x = fromList ['a','b','c','d','e']
> -- in readPoint (moveToIndex 3 x)
> -- :}
> -- Just 'd'

:::



Mutation
--------

Finally we lift the mutation primitives. We have insert and delete at the ends:

> insertInit
>   :: a -> Sequence a -> Sequence a
> insertInit a =
>   Sequence . OPL.insertInit (Item a) . unSequence
> 
> deleteInit
>   :: Sequence a -> Sequence a
> deleteInit =
>   Sequence . OPL.deleteInit . unSequence
> 
> insertLast
>   :: a -> Sequence a -> Sequence a
> insertLast a =
>   Sequence . OPL.insertLast (Item a) . unSequence
> 
> deleteLast
>   :: Sequence a -> Sequence a
> deleteLast =
>   Sequence . OPL.deleteLast . unSequence

Insert and delete at the point:

> insertPointLeft
>   :: a -> Sequence a -> Sequence a
> insertPointLeft a =
>   Sequence . OPL.insertPointLeft (Item a) . unSequence
> 
> deletePointLeft
>   :: Sequence a -> Sequence a
> deletePointLeft =
>   Sequence . OPL.deletePointLeft . unSequence
> 
> insertPointRight
>   :: a -> Sequence a -> Sequence a
> insertPointRight a =
>   Sequence . OPL.insertPointRight (Item a) . unSequence
> 
> deletePointRight
>   :: Sequence a -> Sequence a
> deletePointRight =
>   Sequence . OPL.deletePointRight . unSequence

And alter at the ends and the point.

> alterInit
>   :: (a -> a)
>   -> Sequence a -> Sequence a
> alterInit f =
>   Sequence . OPL.alterInit (fmap f) . unSequence
> 
> alterLast
>   :: (a -> a)
>   -> Sequence a -> Sequence a
> alterLast f =
>   Sequence . OPL.alterLast (fmap f) . unSequence
> 
> alterPoint
>   :: (a -> a)
>   -> Sequence a -> Sequence a
> alterPoint f =
>   Sequence . OPL.alterPoint (fmap f) . unSequence



Concatenation
-------------

It will also be useful to concatenate sequences. Like one-pointed lists, we have two versions of this, depending on whether we want to preserve the left or right point.

> prepend
>   :: Sequence a -> Sequence a -> Sequence a
> prepend as bs =
>   Sequence $ OPL.prepend (unSequence as) (unSequence bs)
> 
> append
>   :: Sequence a -> Sequence a -> Sequence a
> append as bs =
>   Sequence $ OPL.append (unSequence as) (unSequence bs)

And some examples:

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x,y,z :: Sequence Int
> --   x = makePointSequence
> --     [1] 2 [3]
> --   y = makePointSequence
> --     [4] 5 [6]
> --   z = makePointSequence
> --     [4,5,6,1] 2 [3]
> -- in z == prepend y x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   x,y,z :: Sequence Int
> --   x = makePointSequence
> --     [1] 2 [3]
> --   y = makePointSequence
> --     [4] 5 [6]
> --   z = makePointSequence
> --     [1] 2 [3,4,5,6]
> -- in z == append y x
> -- :}
> -- True

:::



Testing and debugging
---------------------

We have the usual class instances for interacting with the property testing framework.

> instance
>   ( Arb a
>   ) => Arb (Item a)
>   where
>     arb = fmap Item arb
> 
> instance
>   ( Prune a
>   ) => Prune (Item a)
>   where
>     prune (Item x) = map Item $ prune x

> instance
>   ( Arb a
>   ) => Arb (Sequence a)
>   where
>     arb = Sequence <$> arb
> 
> instance
>   ( Prune a
>   ) => Prune (Sequence a)
>   where
>     prune (Sequence x) =
>       map Sequence $ prune x

> debugShowSequence
>   :: (a -> String) -> Sequence a -> String
> debugShowSequence p xs =
>   "Seq\n===\n" ++ unlines [] -- TODO: fix this -- (map p $ unTape xs)
