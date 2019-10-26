---
title: Two-Pointed Lists
---

::: contents
* [Higher Derivatives](#higher-derivatives): A type of two-hole contexts
* [Constructors](#constructors): Building examples
* [Class Instances](#class-instances): Code for free
* [Queries](#queries): Extracting value from a two-pointed list
* [Navigation](#navigation): Moving the point and mark
* [Mutation](#mutation): Altering state
* [Region Operations](#region-operations): Read, delete, and alter the region
* [Measurement](#measurement): Working with value annotations
* [Splitting](#splitting): Breaking and searching
* [Concatenation](#concatenation): Putting together
* [Testing and Debugging](#testing-and-debugging): For when things go wrong
:::



::: frontmatter

> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE InstanceSigs #-}
> 
> module Kreb.Struct.TwoPointedList where
> 
> import Data.Foldable
> import Data.List (unwords)
> 
> import           Kreb.Check
> import           Kreb.Struct.Valued
> import qualified Kreb.Struct.FingerTree as FT

:::



Higher Derivatives
------------------

In `OnePointedList`, we saw how a certain concept of _derivative_ lets us systematically construct a type of one-hole contexts over some other type. Specifically, we defined a data type that behaved sort of like a list, but with a movable read head we could use to access items in the list. It's not terribly surprising that this process can be repeated, and higher derivatives give us contexts with more than one hole -- more than one read head where access is cheap. This is useful for us because text buffers naturally have _two_ important positions: the location of the cursor, called the _point_, and a second location, called the _mark_, which is used for (say) highlighting text or delimiting the effects of an operation.

In this module we'll develop a type of two-pointed lists. This code is analogous to that for one-pointed lists, and many of the functions have the same names. It's expected that we'll import this module qualified when we use it later.

Calling this type the second derivative of the list functor is a little too simplistic. In a text buffer, we always want to have a point, but we may not care to have a mark. However the point and the mark (if present) should be independently mobile, so it may happen that the point and the mark are at the same position.

With `OnePointedList`, we used a triple to model a distinguished list item with its left and right contexts. For `TwoPointedList` things are a little more complicated; if both the point and the mark are present, we need to keep track of which is "on the left" in the list (or if they are distinct at all). If the mark is not present then our type degenerates to `OnePointedList`. We can model this with the following definition.

> data TwoPointedList m a
>   = Vacant
>   | PointOnly
>       (FT.FingerTree m a, a, FT.FingerTree m a)
>   | Coincide
>       (FT.FingerTree m a, a, FT.FingerTree m a)
>   | PointMark
>       (FT.FingerTree m a, a, FT.FingerTree m a, a, FT.FingerTree m a)
>   | MarkPoint
>       (FT.FingerTree m a, a, FT.FingerTree m a, a, FT.FingerTree m a)
>   deriving (Eq, Show)

This is a little hairy! But consumers of this code should not have to worry about the details of how this type is implemented. The goal is to allow client code to treat a `TwoPointedList` like, well, a list of items with one and maybe a second read head.



Constructors
------------

Next we define some constructors for this type. We have the usual `empty` and `singleton` constructors for list-like types:

> empty
>   ::TwoPointedList m a
> empty = Vacant
> 
> singleton
>   :: ( Valued m a )
>   => a -> TwoPointedList m a
> singleton a =
>   PointOnly (mempty, a, mempty)

We can also convert a list into a two-pointed list. This is analogous to `makeFromList` on one-pointed lists; the head of the input list becomes the point, and the tail the right context. We'll also implement a version of this constructor for finger trees.

> makeFromList
>   :: ( Valued m a )
>   => [a] -> TwoPointedList m a
> makeFromList xs =
>   case xs of
>     [] -> Vacant
>     u:us -> PointOnly (mempty, u, FT.fromList us)
> 
> fromFingerTree
>   :: ( Valued m a )
>   => FT.FingerTree m a -> TwoPointedList m a
> fromFingerTree as =
>   case FT.uncons as of
>     Nothing -> Vacant
>     Just (u, us) -> PointOnly (mempty, u, us)

For testing purposes it will be handy to have constructors which allow precise placement of the point and the mark. In principle these should not be used in real code, although there's no harm in it.

> makePointOnly
>   :: ( Valued m a )
>   => [a] -> a -> [a]
>   -> TwoPointedList m a
> makePointOnly as x bs =
>   PointOnly ( FT.fromList as, x, FT.fromList bs )
> 
> makeCoincide
>   :: ( Valued m a )
>   => [a] -> a -> [a]
>   -> TwoPointedList m a
> makeCoincide as x bs =
>   Coincide ( FT.fromList as, x, FT.fromList bs )
> 
> makePointMark
>   :: ( Valued m a )
>   => [a] -> a -> [a] -> a -> [a]
>   -> TwoPointedList m a
> makePointMark as x bs y cs = PointMark
>   ( FT.fromList as, x, FT.fromList bs, y, FT.fromList cs )
> 
> makeMarkPoint
>   :: ( Valued m a )
>   => [a] -> a -> [a] -> a -> [a]
>   -> TwoPointedList m a
> makeMarkPoint as x bs y cs = MarkPoint
>   ( FT.fromList as, x, FT.fromList bs, y, FT.fromList cs )



Class Instances
---------------

Since two-pointed lists are essentially lists, it's not surprising they have a `Foldable` instance.

> instance Foldable (TwoPointedList m) where
>   toList
>     :: TwoPointedList m a -> [a]
>   toList w = case w of
>     Vacant -> []
>     PointOnly (as, x, bs) -> concat
>       [ toList as, [x], toList bs ]
>     Coincide (as, x, bs) -> concat
>       [ toList as, [x], toList bs ]
>     PointMark (as, x, bs, y, cs) -> concat
>       [ toList as, [x], toList bs, [y], toList cs ]
>     MarkPoint (as, x, bs, y, cs) -> concat
>       [ toList as, [x], toList bs, [y], toList cs ]
> 
>   foldr
>     :: (a -> b -> b) -> b
>     -> TwoPointedList m a -> b
>   foldr f e w = case w of
>     Vacant -> e
>     PointOnly (as, x, bs) ->
>       foldr f (f x (foldr f e bs)) as
>     Coincide (as, x, bs) ->
>       foldr f (f x (foldr f e bs)) as
>     PointMark (as, x, bs, y, cs) ->
>       foldr f (f x (foldr f (f y (foldr f e cs)) bs)) as
>     MarkPoint (as, x, bs, y, cs) ->
>       foldr f (f x (foldr f (f y (foldr f e cs)) bs)) as
> 
>   foldl
>     :: (b -> a -> b) -> b
>     -> TwoPointedList m a -> b
>   foldl f e w = case w of
>     Vacant -> e
>     PointOnly (as, x, bs) ->
>       foldl f (f (foldl f e as) x) bs
>     Coincide (as, x, bs) ->
>       foldl f (f (foldl f e as) x) bs
>     PointMark (as, x, bs, y, cs) ->
>       foldl f (f (foldl f (f (foldl f e as) x) bs) y) cs
>     MarkPoint (as, x, bs, y, cs) ->
>       foldl f (f (foldl f (f (foldl f e as) x) bs) y) cs

We'd also like `TwoPointedList` to be a functor. But as was the case with `OnePointedList`, the extra `Valued` constraint means we can't express this as a proper type class. Instead we define a plain function which can be used to define proper `Functor` instances on types which specialize the value parameter.

> fmapList
>   :: forall m1 m2 a1 a2
>    . ( Valued m1 a1, Valued m2 a2 )
>   => (a1 -> a2) -> TwoPointedList m1 a1 -> TwoPointedList m2 a2
> fmapList f w = case w of
>   Vacant -> Vacant
>   PointOnly (as, x, bs) ->
>     PointOnly (FT.fmapFT f as, f x, FT.fmapFT f bs)
>   Coincide (as, x, bs) ->
>     Coincide (FT.fmapFT f as, f x, FT.fmapFT f bs)
>   PointMark (as, x, bs, y, cs) ->
>     PointMark (FT.fmapFT f as, f x, FT.fmapFT f bs, f y, FT.fmapFT f cs)
>   MarkPoint (as, x, bs, y, cs) ->
>     MarkPoint (FT.fmapFT f as, f x, FT.fmapFT f bs, f y, FT.fmapFT f cs)



Queries
-------

Next we define some helper functions for extracting information from a two-pointed list. First we can define some simple predicates based on the constructor.

> isEmpty
>   :: TwoPointedList m a -> Bool
> isEmpty w = case w of
>   Vacant -> True
>   _ -> False
> 
> hasMark
>   :: TwoPointedList m a -> Bool
> hasMark w = case w of
>   PointOnly _ -> False
>   _ -> True

It will also be useful to detect when a two-pointed list has exactly one item in it.

> isSingleton
>   :: ( Valued m a )
>   => TwoPointedList m a -> Bool
> isSingleton w = case w of
>   PointOnly (as, _, bs) ->
>     (FT.isEmpty as) && (FT.isEmpty bs)
>   Coincide (as, _, bs) ->
>     (FT.isEmpty as) && (FT.isEmpty bs)
>   _ -> False

We can also detect when the point is at the first or last position in the list.

> isPointAtStart
>   :: ( Valued m a )
>   => TwoPointedList m a -> Bool
> isPointAtStart w = case w of
>   Vacant -> False
>   PointOnly (as, _, _) ->
>     case FT.uncons as of
>       Nothing -> True
>       _ -> False
>   Coincide (as, _, _) ->
>     case FT.uncons as of
>       Nothing -> True
>       _ -> False
>   PointMark (as, _, _, _, _) ->
>     case FT.uncons as of
>       Nothing -> True
>       _ -> False
>   MarkPoint _ -> False
> 
> isPointAtEnd
>   :: ( Valued m a )
>   => TwoPointedList m a -> Bool
> isPointAtEnd w = case w of
>   Vacant -> False
>   PointOnly (_, _, bs) -> case FT.unsnoc bs of
>     Nothing -> True
>     _ -> False
>   Coincide (_, _, bs) -> case FT.unsnoc bs of
>     Nothing -> True
>     _ -> False
>   PointMark _ -> False
>   MarkPoint (_, _, _, _, cs) -> case FT.unsnoc cs of
>     Nothing -> True
>     _ -> False

We can do the same for the mark, although it's less clear when this would be useful outside of tests.

> isMarkAtStart
>   :: ( Valued m a )
>   => TwoPointedList m a -> Bool
> isMarkAtStart w = case w of
>   Vacant -> False
>   PointOnly _ -> False
>   Coincide (as, _, _) ->
>     case FT.uncons as of
>       Nothing -> True
>       _ -> False
>   PointMark _ -> False
>   MarkPoint (as, _, _, _, _) ->
>     case FT.uncons as of
>       Nothing -> True
>       _ -> False
> 
> isMarkAtEnd
>   :: ( Valued m a )
>   => TwoPointedList m a -> Bool
> isMarkAtEnd w = case w of
>   Vacant -> False
>   PointOnly _ -> False
>   Coincide (_, _, bs) -> case FT.unsnoc bs of
>     Nothing -> True
>     _ -> False
>   PointMark (_, _, _, _, cs) -> case FT.unsnoc cs of
>     Nothing -> True
>     _ -> False
>   MarkPoint _ -> False

And we'll need to read the item at the point and the mark.

> readPoint
>   :: TwoPointedList m a -> Maybe a
> readPoint w = case w of
>   Vacant -> Nothing
>   PointOnly (_, x, _) -> Just x
>   Coincide (_, x, _) -> Just x
>   PointMark (_, x, _, _, _) -> Just x
>   MarkPoint (_, _, _, x, _) -> Just x
> 
> readMark
>   :: TwoPointedList m a -> Maybe a
> readMark w = case w of
>   Vacant -> Nothing
>   PointOnly _ -> Nothing
>   Coincide (_, x, _) -> Just x
>   PointMark (_, _, _, x, _) -> Just x
>   MarkPoint (_, x, _, _, _) -> Just x



Navigation
----------

Some of the most important operations on two-pointed lists are those which move the point and the mark around. There are up to two read heads, and four special movement operations -- move to start or end and move left or right -- yielding eight navigation primitives.

When navigating a two-pointed list we need to be deliberate about what happens to the mark, if it is set. In particular it will be most useful to leave the mark alone, and if a particular situation requires the mark to be cleared, do that explicitly.

We can move the point directly to the start or end position:

> movePointToStart
>   :: ( Valued m a )
>   => TwoPointedList m a -> TwoPointedList m a
> movePointToStart w = case w of
>   Vacant -> Vacant
>   PointOnly (as, x, bs) -> case FT.uncons as of
>     Nothing -> PointOnly (mempty, x, bs)
>     Just (a, as') -> PointOnly (mempty, a, FT.snoc x as' <> bs)
>   Coincide (as, x, bs) -> case FT.uncons as of
>     Nothing -> Coincide (mempty, x, bs)
>     Just (a, as') -> PointMark (mempty, a, as', x, bs)
>   PointMark (as, x, bs, y, cs) -> case FT.uncons as of
>     Nothing -> PointMark (mempty, x, bs, y, cs)
>     Just (a, as') -> PointMark (mempty, a, FT.snoc x as' <> bs, y, cs)
>   MarkPoint (as, x, bs, y, cs) -> case FT.uncons as of
>     Nothing -> Coincide (mempty, x, bs <> FT.cons y cs)
>     Just (a, as') -> PointMark (mempty, a, as', x, FT.snoc y bs <> cs)
> 
> movePointToEnd
>   :: ( Valued m a )
>   => TwoPointedList m a -> TwoPointedList m a
> movePointToEnd w = case w of
>   Vacant -> Vacant
>   PointOnly (as, x, bs) -> case FT.unsnoc bs of
>     Nothing -> PointOnly (as, x, mempty)
>     Just (b, bs') -> PointOnly (FT.snoc x as <> bs', b, mempty)
>   Coincide (as, x, bs) -> case FT.unsnoc bs of
>     Nothing -> Coincide (as, x, mempty)
>     Just (b, bs') -> MarkPoint (as, x, bs', b, mempty)
>   PointMark (as, x, bs, y, cs) -> case FT.unsnoc cs of
>     Nothing -> Coincide (FT.snoc x as <> bs, y, mempty)
>     Just (c, cs') -> MarkPoint (FT.snoc x as <> bs, y, cs', c, mempty)
>   MarkPoint (as, x, bs, y, cs) -> case FT.unsnoc cs of
>     Nothing -> MarkPoint (as, x, bs, y, mempty)
>     Just (c, cs') -> MarkPoint (as, x, bs <> FT.cons y cs', c, mempty)

And we can move the point left and right by one item.

> movePointLeft
>   :: ( Valued m a )
>   => TwoPointedList m a -> TwoPointedList m a
> movePointLeft w = case w of
>   Vacant -> Vacant
>   PointOnly (as, x, bs) -> case FT.unsnoc as of
>     Nothing -> PointOnly (mempty, x, bs)
>     Just (a, as') -> PointOnly (as', a, FT.cons x bs)
>   Coincide (as, x, bs) -> case FT.unsnoc as of
>     Nothing -> Coincide (mempty, x, bs)
>     Just (a, as') -> PointMark (as', a, mempty, x, bs)
>   PointMark (as, x, bs, y, cs) -> case FT.unsnoc as of
>     Nothing -> PointMark (mempty, x, bs, y, cs)
>     Just (a, as') -> PointMark (as', a, FT.cons x bs, y, cs)
>   MarkPoint (as, x, bs, y, cs) -> case FT.unsnoc bs of
>     Nothing -> Coincide (as, x, FT.cons y cs)
>     Just (b, bs') -> MarkPoint (as, x, bs', b, FT.cons y cs)
> 
> movePointRight
>   :: ( Valued m a )
>   => TwoPointedList m a -> TwoPointedList m a
> movePointRight w = case w of
>   Vacant -> Vacant
>   PointOnly (as, x, bs) -> case FT.uncons bs of
>     Nothing -> PointOnly (as, x, mempty)
>     Just (b, bs') -> PointOnly (FT.snoc x as, b, bs')
>   Coincide (as, x, bs) -> case FT.uncons bs of
>     Nothing -> Coincide (as, x, mempty)
>     Just (b, bs') -> MarkPoint (as, x, mempty, b, bs')
>   PointMark (as, x, bs, y, cs) -> case FT.uncons bs of
>     Nothing -> Coincide (FT.snoc x as, y, cs)
>     Just (b, bs') -> PointMark (FT.snoc x as, b, bs', y, cs)
>   MarkPoint (as, x, bs, y, cs) -> case FT.uncons cs of
>     Nothing -> MarkPoint (as, x, bs, y, mempty)
>     Just (c, cs') -> MarkPoint (as, x, FT.snoc y bs, c, cs')

And a few examples to test our understanding.

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x, xL, xR :: TwoPointedList Count Char
> --   x = makePointMark ['a','b'] 'c' ['d'] 'e' ['f']
> --   xL = makePointMark ['a'] 'b' ['c','d'] 'e' ['f']
> --   xR = makePointMark ['a','b','c'] 'd' [] 'e' ['f']
> -- in (xL == movePointLeft x, xR == movePointRight x)
> -- :}
> -- (True,True)

:::

We can perform the same actions on the mark. In this case, if the mark is not set the navigation primitives act like the identity.

> moveMarkToStart
>   :: ( Valued m a )
>   => TwoPointedList m a -> TwoPointedList m a
> moveMarkToStart w = case w of
>   Vacant -> Vacant
>   PointOnly (as, x, bs) -> PointOnly (as, x, bs)
>   Coincide (as, x, bs) -> case FT.uncons as of
>     Nothing -> Coincide (mempty, x, bs)
>     Just (a, as') -> MarkPoint (mempty, a, as', x, bs)
>   PointMark (as, x, bs, y, cs) -> case FT.uncons as of
>     Nothing -> Coincide (mempty, x, bs <> FT.cons y cs)
>     Just (a, as') -> MarkPoint (mempty, a, as', x, FT.snoc y bs <> cs)
>   MarkPoint (as, x, bs, y, cs) -> case FT.uncons as of
>     Nothing -> MarkPoint (mempty, x, bs, y, cs)
>     Just (a, as') -> MarkPoint (mempty, a, FT.snoc x as' <> bs, y, cs)
> 
> moveMarkToEnd
>   :: ( Valued m a )
>   => TwoPointedList m a -> TwoPointedList m a
> moveMarkToEnd w = case w of
>   Vacant -> Vacant
>   PointOnly (as, x, bs) -> PointOnly (as, x, bs)
>   Coincide (as, x, bs) -> case FT.unsnoc bs of
>     Nothing -> Coincide (as, x, mempty)
>     Just (b, bs') -> PointMark (as, x, bs', b, mempty)
>   PointMark (as, x, bs, y, cs) -> case FT.unsnoc cs of
>     Nothing -> PointMark (as, x, bs, y, mempty)
>     Just (c, cs') -> PointMark (as, x, bs <> FT.cons y cs', c, mempty)
>   MarkPoint (as, x, bs, y, cs) -> case FT.unsnoc cs of
>     Nothing -> Coincide (FT.snoc x as <> bs, y, mempty)
>     Just (c, cs') -> PointMark (FT.snoc x as <> bs, y, cs', c, mempty)
> 
> moveMarkLeft
>   :: ( Valued m a )
>   => TwoPointedList m a -> TwoPointedList m a
> moveMarkLeft w = case w of
>   Vacant -> Vacant
>   PointOnly z -> PointOnly z
>   Coincide (as, x, bs) -> case FT.unsnoc as of
>     Nothing -> Coincide (mempty, x, bs)
>     Just (a, as') -> MarkPoint (as', a, mempty, x, bs)
>   PointMark (as, x, bs, y, cs) -> case FT.unsnoc bs of
>     Nothing -> Coincide (as, x, FT.cons y cs)
>     Just (b, bs') -> PointMark (as, x, bs', b, FT.cons y cs)
>   MarkPoint (as, x, bs, y, cs) -> case FT.unsnoc as of
>     Nothing -> MarkPoint (mempty, x, bs, y, cs)
>     Just (a, as') -> MarkPoint (as', a, FT.cons x bs, y, cs)
> 
> moveMarkRight
>   :: ( Valued m a )
>   => TwoPointedList m a -> TwoPointedList m a
> moveMarkRight w = case w of
>   Vacant -> Vacant
>   PointOnly z -> PointOnly z
>   Coincide (as, x, bs) -> case FT.uncons bs of
>     Nothing -> Coincide (as, x, mempty)
>     Just (b, bs') -> PointMark (as, x, mempty, b, bs')
>   PointMark (as, x, bs, y, cs) -> case FT.uncons cs of
>     Nothing -> PointMark (as, x, bs, y, mempty)
>     Just (c, cs') -> PointMark (as, x, FT.snoc y bs, c, cs')
>   MarkPoint (as, x, bs, y, cs) -> case FT.uncons bs of
>     Nothing -> Coincide (FT.snoc x as, y, cs)
>     Just (b, bs') -> MarkPoint (FT.snoc x as, b, bs', y, cs)



Mutation
--------

Next we define our two main primitives for bringing the mark in and out of existence. The first introduces a mark if one doesn't already exist, and if it does, leaves it alone.

> leaveMark
>   :: ( Valued m a )
>   => TwoPointedList m a -> TwoPointedList m a
> leaveMark w = case w of
>   PointOnly (as, x, bs) -> Coincide (as, x, bs)
>   _ -> w

The second primitive clears the mark by resorbing it into the rest of the list.

> clearMark
>   :: ( Valued m a )
>   => TwoPointedList m a -> TwoPointedList m a
> clearMark w = case w of
>   Vacant ->
>     Vacant
>   PointOnly (as, x, bs) ->
>     PointOnly (as, x, bs)
>   Coincide (as, x, bs) ->
>     PointOnly (as, x, bs)
>   PointMark (as, x, bs, y, cs) ->
>     PointOnly (as, x, bs <> (FT.cons y cs))
>   MarkPoint (as, x, bs, y, cs) ->
>     PointOnly (as <> (FT.cons x bs), y, cs)

And of course we have efficient operations for insertion and deletion. First at the start and end -- these are analogous to `cons/snoc` and `uncons/unsnoc` on ordinary lists, but we give them different names.

> insertAtStart
>   :: ( Valued m a )
>   => a -> TwoPointedList m a -> TwoPointedList m a
> insertAtStart u w = case w of
>   Vacant ->
>     PointOnly (mempty, u, mempty)
>   PointOnly (as, x, bs) ->
>     PointOnly (FT.cons u as, x, bs)
>   Coincide (as, x, bs) ->
>     Coincide (FT.cons u as, x, bs)
>   PointMark (as, x, bs, y, cs) ->
>     PointMark (FT.cons u as, x, bs, y, cs)
>   MarkPoint (as, x, bs, y, cs) ->
>     MarkPoint (FT.cons u as, x, bs, y, cs)
> 
> viewAtStart
>   :: ( Valued m a )
>   => TwoPointedList m a -> Maybe (a, TwoPointedList m a)
> viewAtStart w = case w of
>   Vacant -> Nothing
>   PointOnly (as, x, bs) -> Just $ case FT.uncons as of
>     Nothing -> case FT.uncons bs of
>       Nothing -> (x, empty)
>       Just (b, bs') -> (x, PointOnly (mempty, b, bs'))
>     Just (a, as') -> (a, PointOnly (as', x, bs))
>   Coincide (as, x, bs) -> Just $ case FT.uncons as of
>     Nothing -> case FT.uncons bs of
>       Nothing -> (x, empty)
>       Just (b, bs') -> (x, Coincide (mempty, b, bs))
>     Just (a, as') -> (a, Coincide (as', x, bs))
>   PointMark (as, x, bs, y, cs) -> Just $ case FT.uncons as of
>     Nothing -> case FT.uncons bs of
>       Nothing -> (x, Coincide (mempty, y, cs))
>       Just (b, bs') -> (x, PointMark (mempty, b, bs', y, cs))
>     Just (a, as') -> (a, PointMark (as', x, bs, y, cs))
>   MarkPoint (as, x, bs, y, cs) -> Just $ case FT.uncons as of
>     Nothing -> case FT.uncons bs of
>       Nothing -> (x, Coincide (mempty, y, cs))
>       Just (b, bs') -> (x, MarkPoint (mempty, b, bs', y, cs))
>     Just (a, as') -> (a, MarkPoint (as', x, bs, y, cs))
> 
> deleteAtStart
>   :: ( Valued m a )
>   => TwoPointedList m a -> TwoPointedList m a
> deleteAtStart w = case w of
>   Vacant -> Vacant
>   PointOnly (as, x, bs) -> case FT.uncons as of
>     Nothing -> case FT.uncons bs of
>       Nothing -> Vacant
>       Just (b, bs') -> PointOnly (mempty, b, bs')
>     Just (_, as') -> PointOnly (as', x, bs)
>   Coincide (as, x, bs) -> case FT.uncons as of
>     Nothing -> case FT.uncons bs of
>       Nothing -> Vacant
>       Just (b, bs') -> Coincide (mempty, b, bs')
>     Just (_, as') -> Coincide (as', x, bs)
>   PointMark (as, x, bs, y, cs) -> case FT.uncons as of
>     Nothing -> case FT.uncons bs of
>       Nothing -> Coincide (mempty, y, cs)
>       Just (b, bs') -> PointMark (mempty, b, bs', y, cs)
>     Just (_, as') -> PointMark (as', x, bs, y, cs)
>   MarkPoint (as, x, bs, y, cs) -> case FT.uncons as of
>     Nothing -> case FT.uncons bs of
>       Nothing -> Coincide (mempty, y, cs)
>       Just (b, bs') -> MarkPoint (mempty, b, bs', y, cs)
>     Just (_, as') -> MarkPoint (as', x, bs, y, cs)
> 
> insertAtEnd
>   :: ( Valued m a )
>   => a -> TwoPointedList m a -> TwoPointedList m a
> insertAtEnd u w = case w of
>   Vacant ->
>     PointOnly (mempty, u, mempty)
>   PointOnly (as, x, bs) ->
>     PointOnly (as, x, FT.snoc u bs)
>   Coincide (as, x, bs) ->
>     Coincide (as, x, FT.snoc u bs)
>   PointMark (as, x, bs, y, cs) ->
>     PointMark (as, x, bs, y, FT.snoc u cs)
>   MarkPoint (as, x, bs, y, cs) ->
>     MarkPoint (as, x, bs, y, FT.snoc u cs)
> 
> viewAtEnd
>   :: ( Valued m a )
>   => TwoPointedList m a -> Maybe (a, TwoPointedList m a)
> viewAtEnd w = case w of
>   Vacant -> Nothing
>   PointOnly (as, x, bs) -> Just $ case FT.unsnoc bs of
>     Nothing -> case FT.unsnoc as of
>       Nothing -> (x, empty)
>       Just (a, as') -> (x, PointOnly (as', a, mempty))
>     Just (b, bs') -> (b, PointOnly (as, x, bs'))
>   Coincide (as, x, bs) -> Just $ case FT.unsnoc bs of
>     Nothing -> case FT.unsnoc as of
>       Nothing -> (x, empty)
>       Just (a, as') -> (x, Coincide (as', a, mempty))
>     Just (b, bs') -> (b, Coincide (as, x, bs'))
>   PointMark (as, x, bs, y, cs) -> Just $ case FT.unsnoc cs of
>     Nothing -> case FT.unsnoc bs of
>       Nothing -> (y, Coincide (as, x, mempty))
>       Just (b, bs') -> (y, PointMark (as, x, bs', b, mempty))
>     Just (c, cs') -> (c, PointMark (as, x, bs, y, cs'))
>   MarkPoint (as, x, bs, y, cs) -> Just $ case FT.unsnoc cs of
>     Nothing -> case FT.unsnoc bs of
>       Nothing -> (y, Coincide (as, x, mempty))
>       Just (b, bs') -> (y, MarkPoint (as, x, bs', b, mempty))
>     Just (c, cs') -> (c, MarkPoint (as, x, bs, y, cs'))
> 
> deleteAtEnd
>   :: ( Valued m a )
>   => TwoPointedList m a -> TwoPointedList m a
> deleteAtEnd w = case w of
>   Vacant -> Vacant
>   PointOnly (as, x, bs) -> case FT.unsnoc bs of
>     Nothing -> case FT.unsnoc as of
>       Nothing -> Vacant
>       Just (a, as') -> PointOnly (as', a, mempty)
>     Just (_, bs') -> PointOnly (as, x, bs')
>   Coincide (as, x, bs) -> case FT.unsnoc bs of
>     Nothing -> case FT.unsnoc as of
>       Nothing -> Vacant
>       Just (a, as') -> Coincide (as', a, mempty)
>     Just (_, bs') -> Coincide (as, x, bs')
>   PointMark (as, x, bs, y, cs) -> case FT.unsnoc cs of
>     Nothing -> case FT.unsnoc bs of
>       Nothing -> Coincide (as, x, mempty)
>       Just (b, bs') -> PointMark (as, x, bs', b, mempty)
>     Just (_, cs') -> PointMark (as, x, bs, y, cs')
>   MarkPoint (as, x, bs, y, cs) -> case FT.unsnoc cs of
>     Nothing -> case FT.unsnoc bs of
>       Nothing -> Coincide (as, x, mempty)
>       Just (b, bs') -> MarkPoint (as, x, bs', b, mempty)
>     Just (_, cs') -> MarkPoint (as, x, bs, y, cs')

And next to the left and the right of the point.

> insertPointLeft
>   :: ( Valued m a )
>   => a -> TwoPointedList m a -> TwoPointedList m a
> insertPointLeft u w = case w of
>   Vacant ->
>     PointOnly (mempty, u, mempty)
>   PointOnly (as, x, bs) ->
>     PointOnly (FT.snoc u as, x, bs)
>   Coincide (as, x, bs) ->
>     Coincide (FT.snoc u as, x, bs)
>   PointMark (as, x, bs, y, cs) ->
>     PointMark (FT.snoc u as, x, bs, y, cs)
>   MarkPoint (as, x, bs, y, cs) ->
>     MarkPoint (as, x, FT.snoc u bs, y, cs)
> 
> deletePointLeft
>   :: ( Valued m a )
>   => TwoPointedList m a -> TwoPointedList m a
> deletePointLeft w = case w of
>   Vacant ->
>     Vacant
>   PointOnly (as, x, bs) -> case FT.unsnoc as of
>     Nothing -> PointOnly (mempty, x, bs)
>     Just (_, as') -> PointOnly (as', x, bs)
>   Coincide (as, x, bs) -> case FT.unsnoc as of
>     Nothing -> Coincide (mempty, x, bs)
>     Just (_, as') -> Coincide (as', x, bs)
>   PointMark (as, x, bs, y, cs) -> case FT.unsnoc as of
>     Nothing -> PointMark (mempty, x, bs, y, cs)
>     Just (_, as') -> PointMark (as', x, bs, y, cs)
>   MarkPoint (as, x, bs, y, cs) -> case FT.unsnoc bs of
>     Nothing -> Coincide (as, y, cs)
>     Just (_, bs') -> MarkPoint (as, x, bs', y, cs)
> 
> insertPointRight
>   :: ( Valued m a )
>   => a -> TwoPointedList m a -> TwoPointedList m a
> insertPointRight u w = case w of
>   Vacant ->
>     PointOnly (mempty, u, mempty)
>   PointOnly (as, x, bs) ->
>     PointOnly (as, x, FT.cons u bs)
>   Coincide (as, x, bs) ->
>     Coincide (as, x, FT.cons u bs)
>   PointMark (as, x, bs, y, cs) ->
>     PointMark (as, x, FT.cons u bs, y, cs)
>   MarkPoint (as, x, bs, y, cs) ->
>     MarkPoint (as, x, bs, y, FT.cons u cs)
> 
> deletePointRight
>   :: ( Valued m a )
>   => TwoPointedList m a -> TwoPointedList m a
> deletePointRight w = case w of
>   Vacant ->
>     Vacant
>   PointOnly (as, x, bs) -> case FT.uncons bs of
>     Nothing -> PointOnly (as, x, mempty)
>     Just (_, bs') -> PointOnly (as, x, bs')
>   Coincide (as, x, bs) -> case FT.uncons bs of
>     Nothing -> Coincide (as, x, mempty)
>     Just (_, bs') -> Coincide (as, x, bs')
>   PointMark (as, x, bs, y, cs) -> case FT.uncons bs of
>     Nothing -> Coincide (as, x, cs)
>     Just (_, bs') -> PointMark (as, x, bs', y, cs)
>   MarkPoint (as, x, bs, y, cs) -> case FT.uncons cs of
>     Nothing -> MarkPoint (as, x, bs, y, mempty)
>     Just (_, cs') -> MarkPoint (as, x, bs, y, cs')



Region Operations
-----------------

Our goal is to use two-pointed lists to represent buffers of text. With that in mind, it will also be useful to have operations for working with the _region_ -- the portion of the list between the point and the mark. The main operations are, in rough terms, _copy_, _cut_, and _alter_.

It's important to be careful and consistent here about exactly what "between the point and the mark" means. In particular, are either the point or the mark themselves included? I can think of a few ways to resolve this:

* Include the point but not the mark;
* Include the mark but not the point;
* Include both the mark and the point;
* Include neither the mark nor the point;
* Include the right endpoint but not the left (whether it's the point or the mark); or
* Include the left endpoint but not the left (whether it's the point or the mark).

I can certainly imagine scenarios where any of these interpretations would make sense. However the last one in particular jumps out as more "natural" for the purpose of text editing. To see why, consider the way terminals work when the cursor symbol is set to a block character. In this setting, usually the insertion point acts as if it is at the _left edge_ of the highlighted cursor block.

So our region editing operations will all be _left biased_, affecting the left endpoint of the region but not the right, regardless of which is the point and which the mark.

To start, here's a left-biased region version of `fmap`.

> alterRegionL
>   :: forall m a
>    . ( Valued m a )
>   => (a -> a) -> TwoPointedList m a -> TwoPointedList m a
> alterRegionL f w = case w of
>   Vacant -> Vacant
>   PointOnly (as, x, bs) ->
>     PointOnly (as, f x, bs)
>   Coincide (as, x, bs) ->
>     Coincide (as, f x, bs)
>   PointMark (as, x, bs, y, cs) ->
>     PointMark (as, f x, FT.fmapFT f bs, y, cs)
>   MarkPoint (as, x, bs, y, cs) ->
>     MarkPoint (as, f x, FT.fmapFT f bs, y, cs)

An example of `alterRegionL` is useful here.

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x, y :: TwoPointedList Count Char
> --   x = makeMarkPoint ['a'] 'b' ['c','d'] 'e' ['f']
> --   y = makeMarkPoint ['a'] 'z' ['z','z'] 'e' ['f']
> -- in y == alterRegionL (const 'z') x
> -- :}
> -- True

:::

The next simplest operation simply extracts the region.

> copyRegionL
>   :: forall m a
>    . ( Valued m a )
>   => TwoPointedList m a
>   -> Maybe (FT.FingerTree m a)
> copyRegionL w = case w of
>   Vacant -> Nothing
>   PointOnly _ -> Nothing
>   Coincide (_, x, _) -> Just (FT.singleton x)
>   PointMark (_, x, bs, _, _) -> Just (FT.cons x bs)
>   MarkPoint (_, x, bs, _, _) -> Just (FT.cons x bs)

Our next region operation is _cut_. This removes the region, returning it along with the remainder of the list spliced together. Notably, after a cut operation the remaining list no longer has a mark set. The only tricky case is when the point and mark coincide with each other.

> cutRegionL
>   :: forall m a
>    . ( Valued m a )
>   => TwoPointedList m a
>   -> Maybe (FT.FingerTree m a, TwoPointedList m a)
> cutRegionL w = case w of
>   Vacant -> Nothing
>   PointOnly _ -> Nothing
>   Coincide (as, x, bs) -> Just $ case FT.uncons bs of
>     Just (b, bs') ->
>       (FT.singleton x, PointOnly (as, b, bs'))
>     Nothing -> case FT.unsnoc as of
>       Just (a, as') ->
>         (FT.singleton x, PointOnly (as', a, mempty))
>       Nothing -> (FT.singleton x, Vacant)
>   PointMark (as, x, bs, y, cs) ->
>     Just (FT.cons x bs, PointOnly (as, y, cs))
>   MarkPoint (as, x, bs, y, cs) ->
>     Just (FT.cons x bs, PointOnly (as, y, cs))

Finally, we have _insert_. We'd like insert and cut to be as close to inverses of each other as possible, although this is tricky; I think the best we can hope for is that cut is a left inverse of insert "up to a clearing of the mark".

> insertRegionL
>   :: forall m a
>    . ( Valued m a )
>   => FT.FingerTree m a -> TwoPointedList m a -> TwoPointedList m a
> insertRegionL us w = case FT.uncons us of
>   Nothing -> w
>   Just (u, us') -> case w of
>     Vacant -> case FT.unsnoc us' of
>       Nothing -> Coincide (mempty, u, mempty)
>       Just (v, us'') -> MarkPoint (mempty, u, us', v, mempty)
>     PointOnly (as, x, bs) -> MarkPoint (as, u, us', x, bs)
>     Coincide (as, x, bs) -> case FT.unsnoc us' of
>       Nothing -> Coincide (as, u, bs)
>       Just (v, us'') -> MarkPoint (as, u, us'', v, bs)
>     PointMark (as, _, _, y, cs) ->
>       MarkPoint (as, u, us', y, cs)
>     MarkPoint (as, _, _, y, cs) ->
>       MarkPoint (as, u, us', y, cs)

Cut an insert are complicated enough that some examples would be helpful.

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x :: TwoPointedList Count Char
> --   x = makePointMark ['a','b'] 'c' ['d'] 'e' ['f']
> --   --
> --   y :: FT.FingerTree Count Char
> --   y = FT.fromList ['x','y','z']
> --   --
> --   z :: TwoPointedList Count Char
> --   z = makeMarkPoint ['a','b'] 'x' ['y','z'] 'e' ['f']
> -- in z == insertRegionL y x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   x :: TwoPointedList Count Char
> --   x = makePointMark ['a','b'] 'c' ['d'] 'e' ['f']
> --   --
> --   y :: TwoPointedList Count Char
> --   y = makePointOnly ['a','b'] 'e' ['f']
> --   --
> --   z :: FT.FingerTree Count Char
> --   z = FT.fromList ['c','d']
> -- in Just (z, y) == cutRegionL x
> -- :}
> -- True

:::



Measurement
-----------

As with one-pointed lists, it will be handy to have access to the accumulated value annotations at the point, mark, and end of a two-pointed list. First we define a `Valued` instance, which gives us the accumulated value at the end.

> instance
>   ( Valued m a
>   ) => Valued m (TwoPointedList m a)
>   where
>     value w = case w of
>       Vacant -> mempty
>       PointOnly (as, x, bs) -> mconcat
>         [value as, value x, value bs]
>       Coincide (as, x, bs) -> mconcat
>         [value as, value x, value bs]
>       PointMark (as, x, bs, y, cs) -> mconcat
>         [value as, value x, value bs, value y, value cs]
>       MarkPoint (as, x, bs, y, cs) -> mconcat
>         [value as, value x, value bs, value y, value cs]

We can also extract the accumulated value at the point and the mark. These return a `Maybe` to account for the case where the mark (or point!) is not set.

> valueBeforePoint
>   :: ( Valued m a )
>   => TwoPointedList m a -> Maybe m
> valueBeforePoint w = case w of
>   Vacant ->
>     Nothing
>   PointOnly (as, _, _) ->
>     Just (value as)
>   Coincide (as, _, _) ->
>     Just (value as)
>   PointMark (as, _, _, _, _) ->
>     Just (value as)
>   MarkPoint (as, x, bs, _, _) ->
>     Just (value as <> value x <> value bs)
> 
> valueAtPoint
>   :: ( Valued m a )
>   => TwoPointedList m a -> Maybe m
> valueAtPoint w = case w of
>   Vacant -> Nothing
>   PointOnly (_, x, _) ->
>     Just (value x)
>   Coincide (_, x, _) ->
>     Just (value x)
>   PointMark (_, x, _, _, _) ->
>     Just (value x)
>   MarkPoint (_, _, _, y, _) ->
>     Just (value y)
> 
> valueUpToPoint
>   :: ( Valued m a )
>   => TwoPointedList m a -> Maybe m
> valueUpToPoint w = case w of
>   Vacant ->
>     Nothing
>   PointOnly (as, x, _) ->
>     Just (value as <> value x)
>   Coincide (as, x, _) ->
>     Just (value as <> value x)
>   PointMark (as, x, _, _, _) ->
>     Just (value as <> value x)
>   MarkPoint (as, x, bs, y, _) ->
>     Just (value as <> value x <> value bs <> value y)
> 
> valueUpToMark
>   :: ( Valued m a )
>   => TwoPointedList m a -> Maybe m
> valueUpToMark w = case w of
>   Vacant ->
>     Nothing
>   PointOnly _ ->
>     Nothing
>   Coincide (as, _, _) ->
>     Just (value as)
>   PointMark (as, x, bs, _, _) ->
>     Just (value as <> value x <> value bs)
>   MarkPoint (as, _, _, _, _) ->
>     Just (value as)
> 
> valueAtMark
>   :: ( Valued m a )
>   => TwoPointedList m a -> Maybe m
> valueAtMark w = case w of
>   Vacant -> Nothing
>   PointOnly _ -> Nothing
>   Coincide (_, x, _) ->
>     Just (value x)
>   PointMark (_, _, _, y, _) ->
>     Just (value y)
>   MarkPoint (_, x, _, _, _) ->
>     Just (value x)

Finally, we can remeasure a two-pointed list. This will be especially useful for working with text buffers, where the value will encode type level parameters.

> remeasure
>   :: ( Valued m1 a, Valued m2 a )
>   => TwoPointedList m1 a -> TwoPointedList m2 a
> remeasure w = case w of
>   Vacant -> Vacant
>   PointOnly (as, x, bs) ->
>     PointOnly (FT.remeasure as, x, FT.remeasure bs)
>   Coincide (as, x, bs) ->
>     Coincide (FT.remeasure as, x, FT.remeasure bs)
>   PointMark (as, x, bs, y, cs) ->
>     PointMark (FT.remeasure as, x, FT.remeasure bs, y, FT.remeasure cs)
>   MarkPoint (as, x, bs, y, cs) ->
>     MarkPoint (FT.remeasure as, x, FT.remeasure bs, y, FT.remeasure cs)



Splitting
---------

We can convert a two-pointed list back into a finger tree; this looks sort of like integration at the type level.

> integrate
>   :: ( Valued m a )
>   => TwoPointedList m a -> FT.FingerTree m a
> integrate w = case w of
>   Vacant ->
>     mempty
>   PointOnly (as, x, bs) ->
>     as <> (FT.cons x bs)
>   Coincide (as, x, bs) ->
>     as <> (FT.cons x bs)
>   PointMark (as, x, bs, y, cs) ->
>     as <> (FT.cons x bs) <> (FT.cons y cs)
>   MarkPoint (as, x, bs, y, cs) ->
>     as <> (FT.cons x bs) <> (FT.cons y cs)

Conversely, we can use split on finger trees to manipulate the point and mark of a two-pointed list. This is the operation that makes two-pointed lists practical; with an appropriate value annotation we can use it to get log complexity random access.

> split
>   :: ( Valued m a )
>   => (m -> Bool)       -- point predicate
>   -> Maybe (m -> Bool) -- mark predicate
>   -> TwoPointedList m a -> Maybe (TwoPointedList m a)
> split pointP q w =
>   let xs = integrate w
>   in case FT.split pointP xs of
>     Nothing -> Nothing
>     Just (as, x, bs) -> Just $ case q of
>       Nothing -> PointOnly (as, x, bs)
>       Just markP -> case FT.split markP as of
>         Just (us, y, vs) ->
>           MarkPoint (us, y, vs, x, bs)
>         Nothing -> case FT.split markP bs of
>           Just (us, y, vs) ->
>             PointMark (as, x, us, y, vs)
>           Nothing -> PointOnly (as, x, bs)

We also provide a splitting function that preserves the mark.

> splitPoint
>   :: ( Valued m a )
>   => (m -> Bool) -- point predicate
>   -> TwoPointedList m a -> Maybe (TwoPointedList m a)
> splitPoint pointP w = case w of
>   Vacant -> Nothing
>   PointOnly (as, x, bs) ->
>     let xs = as <> FT.cons x bs
>     in case FT.split pointP xs of
>       Nothing -> Nothing
>       Just (us, y, vs) ->
>         Just $ PointOnly (us, y, vs)
>   Coincide (as, x, bs) ->
>     if (not (pointP (value as))) && (pointP (value as <> value x))
>       then Just $ Coincide (as, x, bs)
>       else case FT.split pointP as of
>         Just (us, y, vs) -> Just $ PointMark (us, y, vs, x, bs)
>         Nothing -> case FT.splitWithContext (value as <> value x) pointP bs of
>           Just (us, y, vs) -> Just $ MarkPoint (as, x, us, y, vs)
>           Nothing -> Nothing
>   PointMark (as, x, bs, y, cs) ->
>     let ds = as <> FT.cons x bs
>     in case FT.split pointP ds of
>       Just (us, z, vs) -> Just $ PointMark (us, z, vs, y, cs)
>       Nothing -> case FT.splitWithContext (value ds <> value y) pointP cs of
>         Just (us, z, vs) -> Just $ MarkPoint (ds, y, us, z, vs )
>         Nothing -> Nothing
>   MarkPoint (as, x, bs, y, cs) ->
>     let ds = bs <> FT.cons y cs
>     in case FT.split pointP as of
>       Just (us, z, vs) -> Just $ PointMark (us, z, vs, x, ds)
>       Nothing -> case FT.splitWithContext (value as <> value x) pointP ds of
>         Just (us, z, vs) -> Just $ MarkPoint (as, x, us, z, vs)
>         Nothing -> Nothing

Some examples:

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x, y :: TwoPointedList Count Char
> --   x = makePointMark ['a'] 'b' ['c','d'] 'e' ['f']
> --   y = makePointMark ['a','b'] 'c' ['d'] 'e' ['f']
> -- in (Just y) == (splitPoint (\k -> k == Count 3) x)
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   x, y :: TwoPointedList Count Char
> --   x = makePointMark ['a'] 'b' ['c','d'] 'e' ['f']
> --   y = makeMarkPoint ['a','b','c','d'] 'e' [] 'f' []
> -- in (Just y) == (splitPoint (\k -> k == Count 6) x)
> -- :}
> -- True

:::

While we're here, it is also useful to convert two-pointed lists to ordinary lists with the accumulated value annotation.

> toAnnotatedList
>   :: ( Valued m a )
>   => TwoPointedList m a -> [(a,m)]
> toAnnotatedList =
>   FT.toAnnotatedList . integrate



Concatenation
-------------

It also makes sense to combine two-pointed lists with other structures in a few ways. In addition to being useful as utility functions, these concatenation operators fit nicely into the theory.

> prependFT
>   :: ( Valued m a )
>   => FT.FingerTree m a
>   -> TwoPointedList m a -> TwoPointedList m a
> prependFT xs w = case w of
>   Vacant -> case FT.unsnoc xs of
>     Nothing -> Vacant
>     Just (u,us) -> PointOnly (us, u, mempty)
>   PointOnly (as, x, bs) ->
>     PointOnly (xs <> as, x, bs)
>   Coincide (as, x, bs) ->
>     Coincide (xs <> as, x, bs)
>   PointMark (as, x, bs, y, cs) ->
>     PointMark (xs <> as, x, bs, y, cs)
>   MarkPoint (as, x, bs, y, cs) ->
>     MarkPoint (xs <> as, x, bs, y, cs)
> 
> appendFT
>   :: ( Valued m a )
>   => FT.FingerTree m a
>   -> TwoPointedList m a -> TwoPointedList m a
> appendFT xs w = case w of
>   Vacant -> case FT.uncons xs of
>     Nothing -> Vacant
>     Just (u,us) -> PointOnly (mempty, u, us)
>   PointOnly (as, x, bs) ->
>     PointOnly (as, x, bs <> xs)
>   Coincide (as, x, bs) ->
>     Coincide (as, x, bs <> xs)
>   PointMark (as, x, bs, y, cs) ->
>     PointMark (as, x, bs, y, cs <> xs)
>   MarkPoint (as, x, bs, y, cs) ->
>     MarkPoint (as, x, bs, y, cs <> xs)



Testing and Debugging
---------------------

We end with some utility code. First, we need some class instances for interoperating with our property testing library.

> instance
>   ( Arb a, Valued m a
>   ) => Arb (TwoPointedList m a)
>   where
>     arb = pickFrom5
>       ( pure Vacant
>       , PointOnly <$> arb
>       , Coincide <$> arb
>       , PointMark <$> arb
>       , MarkPoint <$> arb
>       )
> 
> instance
>   ( Prune a, Valued m a
>   ) => Prune (TwoPointedList m a)
>   where
>     prune w = case w of
>       Vacant -> []
>       PointOnly z -> concat
>         [ [ Vacant ]
>         , PointOnly <$> prune z
>         ]
>       Coincide z -> concat
>         [ [ Vacant, PointOnly z ]
>         , Coincide <$> prune z
>         ]
>       PointMark (as, x, bs, y, cs) -> concat
>         [ [ Vacant, Coincide (as, x, bs), Coincide (bs, y, cs) ]
>         , PointMark <$> prune (as, x, bs, y, cs)
>         ]
>       MarkPoint (as, x, bs, y, cs) -> concat
>         [ [ Vacant, Coincide (as, x, bs), Coincide (bs, y, cs) ]
>         , MarkPoint <$> prune (as, x, bs, y, cs)
>         ]

And finally, since two-pointed lists are built out of finger trees, it will be handy to have a function checking that the underlying structure is valid.

> validate
>   :: ( Eq m, Valued m a )
>   => TwoPointedList m a -> Bool
> validate w = case w of
>   Vacant -> True
>   PointOnly (as, _, bs) ->
>     (FT.validate as) && (FT.validate bs)
>   Coincide (as, _, bs) ->
>     (FT.validate as) && (FT.validate bs)
>   PointMark (as, _, bs, _, cs) ->
>     (FT.validate as) && (FT.validate bs) && (FT.validate cs)
>   MarkPoint (as, _, bs, _, cs) ->
>     (FT.validate as) && (FT.validate bs) && (FT.validate cs)
