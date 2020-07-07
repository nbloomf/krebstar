---
title: DequeZippers
---

::: contents
* [Introduction](#introduction): Specializing one-pointed lists
* [Class instances](#class-instances): Code for free
* [Queries](#queries): Getting information out of a DequeZipper
* [Navigation](#navigation): Moving around
* [Mutation](#mutation): Making edits
* [Concatenation](#concatenation): Putting DequeZippers together
* [Testing and debugging](#testing-and-debugging): For when things go wrong
:::



::: frontmatter

> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE InstanceSigs #-}
> {-# LANGUAGE TypeFamilies #-}
> 
> module Kreb.Struct.Data.Deque.Zipper (
>     DequeZipper()
>   , NonEmptyDequeZipper()

> {-

>   , getLength
> 
>   , moveToIndex
> 
>   , prepend
>   , append
> 
>   , debugShowDequeZipper
> -}
> ) where
> 
> import Data.Foldable
> 
> import           Kreb.Control
> import           Kreb.Category
> import           Kreb.Prop

> import           Kreb.Struct.Class
> import qualified Kreb.Struct.Data.FingerTree.Zipper as FTZ
> import           Kreb.Struct.Data.Zipper

:::



Introduction
------------

So far we've developed the finger tree data type, and taken its derivative to get a linear structure with a distinguished and movable pointer somewhere in the middle. We'll use this as the basis for a more specialized structure: _DequeZippers_. This type uses our existing `Count` monoid to describe a list with efficient indexing.

> data DequeZipper a
>   = Empty
>   | NonEmpty (NonEmptyDequeZipper a)
>   deriving (Eq, Show)

> newtype NonEmptyDequeZipper a = NonEmptyDequeZipper
>   { unNonEmptyDequeZipper :: FTZ.FingerTreeZipper (Counted a)
>   } deriving (Eq, Show)





> instance Container DequeZipper where
>   type ElementOf DequeZipper = Hask

> instance Container NonEmptyDequeZipper where
>   type ElementOf NonEmptyDequeZipper = Hask

> instance Subset NonEmptyDequeZipper where
>   type SupersetOf NonEmptyDequeZipper = DequeZipper
> 
>   inject
>     :: ( Hask a )
>     => NonEmptyDequeZipper a -> DequeZipper a
>   inject = NonEmpty
> 
>   restrict
>     :: ( Hask a )
>     => DequeZipper a -> Maybe (NonEmptyDequeZipper a)
>   restrict x = case x of
>     Empty -> Nothing
>     NonEmpty z -> Just z

> instance NonEmpty NonEmptyDequeZipper where
>   empty
>     :: ( Hask a )
>     => DequeZipper a
>   empty = Empty
> 
>   isEmpty
>     :: ( Hask a )
>     => DequeZipper a -> Bool
>   isEmpty x = case x of
>     Empty -> True
>     NonEmpty _ -> False

> instance Singleton DequeZipper where
>   singleton
>     :: ( Hask a )
>     => a -> DequeZipper a
>   singleton = NonEmpty . singleton
> 
>   fromSingleton
>     :: ( Hask a )
>     => DequeZipper a -> Maybe a
>   fromSingleton x = case x of
>     Empty      -> Nothing
>     NonEmpty z -> fromSingleton z

> instance Singleton NonEmptyDequeZipper where
>   singleton
>     :: ( Hask a )
>     => a -> NonEmptyDequeZipper a
>   singleton = NonEmptyDequeZipper . singleton . Counted
> 
>   fromSingleton
>     :: ( Hask a )
>     => NonEmptyDequeZipper a -> Maybe a
>   fromSingleton = fmap unCounted . fromSingleton . unNonEmptyDequeZipper

> instance Foldable DequeZipper where
>   foldr :: (a -> b -> b) -> b -> DequeZipper a -> b
>   foldr f e w = case w of
>     Empty -> e
>     NonEmpty z -> foldr f e z
> 
> instance Foldable NonEmptyDequeZipper where
>   foldr
>     :: forall a b
>      . (a -> b -> b) -> b -> NonEmptyDequeZipper a -> b
>   foldr f e (NonEmptyDequeZipper x) =
>     let g (Counted a) b = f a b
>     in foldr g e x

> instance Functor DequeZipper where
>   fmap f w = case w of
>     Empty -> Empty
>     NonEmpty x -> NonEmpty $ fmap f x
> 
> instance Functor NonEmptyDequeZipper where
>   fmap f (NonEmptyDequeZipper x) =
>     NonEmptyDequeZipper $
>       fmapC @Valued @(->) @Valued @(->) @FTZ.FingerTreeZipper
>         (fmap f) x

> instance Traversable DequeZipper where
>   traverse f w = case w of
>     Empty -> pure Empty
>     NonEmpty x -> fmap NonEmpty $ traverse f x
> 
> instance Traversable NonEmptyDequeZipper where
>   traverse f (NonEmptyDequeZipper x) =
>     let g = fmap Counted . f . unCounted
>     in fmap NonEmptyDequeZipper $
>       traverseC @Valued @(->) @Valued @(->) @FTZ.FingerTreeZipper g x



> instance LinearZipper DequeZipper where
>   readPointer
>     :: ( Hask a )
>     => DequeZipper a -> Maybe a
>   readPointer w = case w of
>     Empty -> Nothing
>     NonEmpty x -> readPointer x
> 
>   alterPointer
>     :: ( Hask a )
>     => (a -> a) -> DequeZipper a -> DequeZipper a
>   alterPointer f w = case w of
>     Empty -> Empty
>     NonEmpty x -> NonEmpty $ alterPointer f x
> 
>   alterPointerM
>     :: ( Hask a, Monad m )
>     => (a -> m a) -> DequeZipper a -> m (DequeZipper a)
>   alterPointerM f w = case w of
>     Empty -> return Empty
>     NonEmpty x -> fmap NonEmpty $ alterPointerM f x
> 
>   isAtStart
>     :: ( Hask a )
>     => DequeZipper a -> Bool
>   isAtStart w = case w of
>     Empty -> False
>     NonEmpty x -> isAtStart x
> 
>   isAtEnd
>     :: ( Hask a )
>     => DequeZipper a -> Bool
>   isAtEnd w = case w of
>     Empty -> False
>     NonEmpty x -> isAtEnd x
> 
>   moveTowardStart
>     :: ( Hask a )
>     => DequeZipper a -> DequeZipper a
>   moveTowardStart w = case w of
>     Empty -> Empty
>     NonEmpty x -> NonEmpty $ moveTowardStart x
> 
>   moveTowardEnd
>     :: ( Hask a )
>     => DequeZipper a -> DequeZipper a
>   moveTowardEnd w = case w of
>     Empty -> Empty
>     NonEmpty x -> NonEmpty $ moveTowardEnd x
> 
>   moveToStart
>     :: ( Hask a )
>     => DequeZipper a -> DequeZipper a
>   moveToStart w = case w of
>     Empty -> Empty
>     NonEmpty x -> NonEmpty $ moveToStart x
> 
>   moveToEnd
>     :: ( Hask a )
>     => DequeZipper a -> DequeZipper a
>   moveToEnd w = case w of
>     Empty -> Empty
>     NonEmpty x -> NonEmpty $ moveToEnd x

> instance LinearZipper NonEmptyDequeZipper where
>   readPointer
>     :: ( Hask a )
>     => NonEmptyDequeZipper a -> Maybe a
>   readPointer (NonEmptyDequeZipper x) =
>     fmap unCounted $ readPointer x
> 
>   alterPointer
>     :: ( Hask a )
>     => (a -> a) -> NonEmptyDequeZipper a -> NonEmptyDequeZipper a
>   alterPointer f (NonEmptyDequeZipper x) =
>     NonEmptyDequeZipper $ alterPointer (fmap f) x
> 
>   alterPointerM
>     :: ( Hask a, Monad m )
>     => (a -> m a) -> NonEmptyDequeZipper a -> m (NonEmptyDequeZipper a)
>   alterPointerM f (NonEmptyDequeZipper x) =
>     let g = fmap Counted . f . unCounted
>     in fmap NonEmptyDequeZipper $ alterPointerM g x
> 
>   isAtStart
>     :: ( Hask a )
>     => NonEmptyDequeZipper a -> Bool
>   isAtStart (NonEmptyDequeZipper x) = isAtStart x
> 
>   isAtEnd
>     :: ( Hask a )
>     => NonEmptyDequeZipper a -> Bool
>   isAtEnd (NonEmptyDequeZipper x) = isAtEnd x
> 
>   moveTowardStart
>     :: ( Hask a )
>     => NonEmptyDequeZipper a -> NonEmptyDequeZipper a
>   moveTowardStart (NonEmptyDequeZipper x) =
>     NonEmptyDequeZipper $ moveTowardStart x
> 
>   moveTowardEnd
>     :: ( Hask a )
>     => NonEmptyDequeZipper a -> NonEmptyDequeZipper a
>   moveTowardEnd (NonEmptyDequeZipper x) =
>     NonEmptyDequeZipper $ moveTowardEnd x
> 
>   moveToStart
>     :: ( Hask a )
>     => NonEmptyDequeZipper a -> NonEmptyDequeZipper a
>   moveToStart (NonEmptyDequeZipper x) =
>     NonEmptyDequeZipper $ moveToStart x
> 
>   moveToEnd
>     :: ( Hask a )
>     => NonEmptyDequeZipper a -> NonEmptyDequeZipper a
>   moveToEnd (NonEmptyDequeZipper x) =
>     NonEmptyDequeZipper $ moveToEnd x





> {-

The `Item` constructor allows us to hide the otherwise awkward `Valued` constraint from consumers of this module; because it is implemented as a newtype, it has no runtime overhead.

Nearly all of the functionality we want out of DequeZippers is already provided by one-pointed lists, so we can just lift the API of that module to work over the `DequeZipper` type. First are the basic constructors.

> 
> fromList
>   :: [a] -> DequeZipper a
> fromList = DequeZipper . OPL.fromList . fmap Counted

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x :: DequeZipper Int
> --   x = singleton 5
> -- in x == empty
> -- :}
> -- False

:::

We also expose a constructor that gives us control over the location of the point; this is only used for testing and debugging.

> makePointDequeZipper
>   :: [a] -> a -> [a]
>   -> DequeZipper a
> makePointDequeZipper as x bs =
>   DequeZipper $ OPL.makePoint
>     (fmap Counted as) (Counted x) (fmap Counted bs)



Class instances
---------------


::: doctest

> -- $
> -- >>> :{
> -- let
> --   x, y :: DequeZipper Int
> --   x = fromList [1,2,3]
> --   y = fromList [2,4,6]
> -- in y == fmap (*2) x
> -- :}
> -- True

:::




Queries
-------


And some examples for good measure.

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x :: DequeZipper Int
> --   x = makePointDequeZipper
> --     [] 1 [2,3]
> -- in (isAtInit x, isAtLast x)
> -- :}
> -- (True,False)
> --
> -- >>> :{
> -- let
> --   x :: DequeZipper Int
> --   x = makePointDequeZipper
> --     [1,2] 3 []
> -- in (isAtInit x, isAtLast x)
> -- :}
> -- (False,True)

:::

Then we have functions for extracting the value at the point and at the ends of the DequeZipper.

> readInit
>   :: DequeZipper a -> Maybe a
> readInit =
>   fmap unCounted . peekFront . unDequeZipper
> 
> readLast
>   :: DequeZipper a -> Maybe a
> readLast =
>   fmap unCounted . peekBack . unDequeZipper


And an example.

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x :: DequeZipper Int
> --   x = makePointDequeZipper
> --     [1,2] 3 [4,5,6]
> -- in (readInit x, readPoint x, readLast x)
> -- :}
> -- (Just 1,Just 3,Just 6)

:::

We'll sometimes also want to get the length of a DequeZipper.

> getLength
>   :: DequeZipper a -> Int
> getLength (DequeZipper as) =
>   let Count k = value as
>   in k

And some examples.

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x :: DequeZipper Int
> --   x = fromList [1,2,3,4,5]
> -- in getLength x
> -- :}
> -- 5
> --
> -- >>> getLength (empty :: DequeZipper Int)
> -- 0

:::



Navigation
----------

Next we have the basic navigation primitives

And some examples:

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x, y, z :: DequeZipper Int
> --   x = makePointDequeZipper
> --     [1,2] 3 [4,5,6]
> --   y = makePointDequeZipper
> --     [] 1 [2,3,4,5,6]
> --   z = makePointDequeZipper
> --     [1,2,3,4,5] 6 []
> -- in (y == moveToInit x, z == moveToLast x)
> -- :}
> -- (True,True)
> --
> -- >>> :{
> -- let
> --   x, y, z :: DequeZipper Int
> --   x = makePointDequeZipper
> --     [1,2] 3 [4,5,6]
> --   y = makePointDequeZipper
> --     [1] 2 [3,4,5,6]
> --   z = makePointDequeZipper
> --     [1,2,3] 4 [5,6]
> -- in (y == movePointLeft x, z == movePointRight x)
> -- :}
> -- (True,True)

:::

We can also navigate to an arbitrary index in the DequeZipper, which is sort of the purpose of this type. :) Note that the indexing is zero-based.

> moveToIndex
>   :: Int -> DequeZipper a -> DequeZipper a
> moveToIndex k (DequeZipper as) = DequeZipper $
>   OPL.split
>     (\(Count i) -> i > k)
>     (OPL.integrate as)

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x :: DequeZipper Char
> --   x = fromList ['a','b','c','d','e']
> -- in readPoint (moveToIndex 0 x)
> -- :}
> -- Just 'a'
> --
> -- >>> :{
> -- let
> --   x :: DequeZipper Char
> --   x = fromList ['a','b','c','d','e']
> -- in readPoint (moveToIndex 3 x)
> -- :}
> -- Just 'd'

:::



Mutation
--------

Finally we lift the mutation primitives. We have insert and delete at the ends:

> insertInit
>   :: a -> DequeZipper a -> DequeZipper a
> insertInit a =
>   DequeZipper . OPL.insertInit (Counted a) . unDequeZipper
> 
> deleteInit
>   :: DequeZipper a -> DequeZipper a
> deleteInit =
>   DequeZipper . OPL.deleteInit . unDequeZipper
> 
> insertLast
>   :: a -> DequeZipper a -> DequeZipper a
> insertLast a =
>   DequeZipper . OPL.insertLast (Counted a) . unDequeZipper
> 
> deleteLast
>   :: DequeZipper a -> DequeZipper a
> deleteLast =
>   DequeZipper . OPL.deleteLast . unDequeZipper

Insert and delete at the point:

> insertPointLeft
>   :: a -> DequeZipper a -> DequeZipper a
> insertPointLeft a =
>   DequeZipper . OPL.insertPointLeft (Counted a) . unDequeZipper
> 
> deletePointLeft
>   :: DequeZipper a -> DequeZipper a
> deletePointLeft =
>   DequeZipper . OPL.deletePointLeft . unDequeZipper
> 
> insertPointRight
>   :: a -> DequeZipper a -> DequeZipper a
> insertPointRight a =
>   DequeZipper . OPL.insertPointRight (Counted a) . unDequeZipper
> 
> deletePointRight
>   :: DequeZipper a -> DequeZipper a
> deletePointRight =
>   DequeZipper . OPL.deletePointRight . unDequeZipper

And alter at the ends and the point.

> alterInit
>   :: (a -> a)
>   -> DequeZipper a -> DequeZipper a
> alterInit f =
>   DequeZipper . OPL.alterInit (fmap f) . unDequeZipper
> 
> alterLast
>   :: (a -> a)
>   -> DequeZipper a -> DequeZipper a
> alterLast f =
>   DequeZipper . OPL.alterLast (fmap f) . unDequeZipper
> 
> alterPoint
>   :: (a -> a)
>   -> DequeZipper a -> DequeZipper a
> alterPoint f =
>   DequeZipper . OPL.alterPoint (fmap f) . unDequeZipper

> alterPointM
>   :: ( Monad m )
>   => (a -> m a)
>   -> DequeZipper a -> m (DequeZipper a)
> alterPointM f (DequeZipper seq) = do
>   seq' <- OPL.alterPointA (\(Counted x) -> fmap Counted $ f x) seq
>   return $ DequeZipper seq'



Concatenation
-------------

It will also be useful to concatenate DequeZippers. Like one-pointed lists, we have two versions of this, depending on whether we want to preserve the left or right point.

> prepend
>   :: DequeZipper a -> DequeZipper a -> DequeZipper a
> prepend as bs =
>   DequeZipper $ OPL.prepend (unDequeZipper as) (unDequeZipper bs)
> 
> append
>   :: DequeZipper a -> DequeZipper a -> DequeZipper a
> append as bs =
>   DequeZipper $ OPL.append (unDequeZipper as) (unDequeZipper bs)

And some examples:

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x,y,z :: DequeZipper Int
> --   x = makePointDequeZipper
> --     [1] 2 [3]
> --   y = makePointDequeZipper
> --     [4] 5 [6]
> --   z = makePointDequeZipper
> --     [4,5,6,1] 2 [3]
> -- in z == prepend y x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   x,y,z :: DequeZipper Int
> --   x = makePointDequeZipper
> --     [1] 2 [3]
> --   y = makePointDequeZipper
> --     [4] 5 [6]
> --   z = makePointDequeZipper
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
>   ) => Arb (DequeZipper a)
>   where
>     arb = DequeZipper <$> arb
> 
> instance
>   ( Prune a
>   ) => Prune (DequeZipper a)
>   where
>     prune (DequeZipper x) =
>       map DequeZipper $ prune x

> debugShowDequeZipper
>   :: (a -> String) -> DequeZipper a -> String
> debugShowDequeZipper p xs =
>   "Seq\n===\n" ++ unlines [] -- TODO: fix this -- (map p $ unTape xs)

> -}
