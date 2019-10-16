---
title: Two-Pointed Lists
---

::: contents
* [Higher Derivatives](#higher-derivatives): A type of two-hole contexts
* [Constructors](#constructors): Building examples
* [Class Instances](#class-instances): Code for free
* [Queries](#queries): Extracting value from a two-pointed list
* [Navigation](#navigation): Moving the point and mark
:::



::: frontmatter

> {-# LANGUAGE
>     MultiParamTypeClasses
>   , ScopedTypeVariables
>   , FlexibleInstances
>   , InstanceSigs
> #-}

> module Kreb.Struct.TwoPointedList where
> 
> import Data.Foldable
> import Data.List (unwords)
> 
> import Kreb.Check
> import Kreb.Struct.FingerTree

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
>       (FingerTree m a, a, FingerTree m a)
>   | Coincide
>       (FingerTree m a, a, FingerTree m a)
>   | PointMark
>       (FingerTree m a, a, FingerTree m a, a, FingerTree m a)
>   | MarkPoint
>       (FingerTree m a, a, FingerTree m a, a, FingerTree m a)
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

We can also convert a list into a two-pointed list. This is analogous to `makeFromList` on one-pointed lists; the head of the input list becomes the point, and the tail the right context.

> makeFromList
>   :: ( Valued m a )
>   => [a] -> TwoPointedList m a
> makeFromList xs =
>   case uncons $ fromListFT xs of
>     Nothing -> Vacant
>     Just (a, as) -> PointOnly (mempty, a, as)

For testing purposes it will be handy to have constructors which allow precise placement of the point and the mark. In principle these should not be used in real code, although there's no harm in it.

> makePointOnly
>   :: ( Valued m a )
>   => [a] -> a -> [a]
>   -> TwoPointedList m a
> makePointOnly as x bs =
>   PointOnly (fromListFT as, x, fromListFT bs)
> 
> makeCoincide
>   :: ( Valued m a )
>   => [a] -> a -> [a]
>   -> TwoPointedList m a
> makeCoincide as x bs =
>   Coincide (fromListFT as, x, fromListFT bs)
> 
> makePointMark
>   :: ( Valued m a )
>   => [a] -> a -> [a] -> a -> [a]
>   -> TwoPointedList m a
> makePointMark as x bs y cs = PointMark
>   ( fromListFT as, x, fromListFT bs, y, fromListFT cs )
> 
> makeMarkPoint
>   :: ( Valued m a )
>   => [a] -> a -> [a] -> a -> [a]
>   -> TwoPointedList m a
> makeMarkPoint as x bs y cs = PointMark
>   ( fromListFT as, x, fromListFT bs, y, fromListFT cs )



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
>     PointOnly (fmapFT f as, f x, fmapFT f bs)
>   Coincide (as, x, bs) ->
>     Coincide (fmapFT f as, f x, fmapFT f bs)
>   PointMark (as, x, bs, y, cs) ->
>     PointMark (fmapFT f as, f x, fmapFT f bs, f y, fmapFT f cs)
>   MarkPoint (as, x, bs, y, cs) ->
>     MarkPoint (fmapFT f as, f x, fmapFT f bs, f y, fmapFT f cs)

We'll also provide a restricted form of `fmapList` that only takes effect between the mark and the point. Note that this function is _left-biased_ -- 'between' includes the left endpoint but not the right. This is because eventually we'll use two-pointed lists to model text buffers, and we'll think of the cursor (point or mark) as existing _between_ characters in the buffer, specifically at the left edge of each cell.

> fmapRegionL
>   :: forall m a
>    . ( Valued m a )
>   => (a -> a) -> TwoPointedList m a -> TwoPointedList m a
> fmapRegionL f w = case w of
>   Vacant -> Vacant
>   PointOnly (as, x, bs) ->
>     PointOnly (as, f x, bs)
>   Coincide (as, x, bs) ->
>     Coincide (as, f x, bs)
>   PointMark (as, x, bs, y, cs) ->
>     PointMark (as, f x, fmapFT f bs, y, cs)
>   MarkPoint (as, x, bs, y, cs) ->
>     MarkPoint (as, f x, fmapFT f bs, y, cs)

An example of `fmapRegionL` is useful here.

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x, y :: TwoPointedList Count Char
> --   x = makeMarkPoint ['a'] 'b' ['c','d'] 'e' ['f']
> --   y = makeMarkPoint ['a'] 'z' ['z','z'] 'e' ['f']
> -- in y == fmapRegionL (const 'z') x
> -- :}
> -- True

:::



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

We can also detect when the point is at the first or last position in the list.

> isPointAtStart
>   :: ( Valued m a )
>   => TwoPointedList m a -> Bool
> isPointAtStart w = case w of
>   Vacant -> False
>   PointOnly (as, _, _) -> case uncons as of
>     Nothing -> True
>     _ -> False
>   Coincide (as, _, _) -> case uncons as of
>     Nothing -> True
>     _ -> False
>   PointMark (as, _, _, _, _) -> case uncons as of
>     Nothing -> True
>     _ -> False
>   MarkPoint _ -> False
> 
> isPointAtEnd
>   :: ( Valued m a )
>   => TwoPointedList m a -> Bool
> isPointAtEnd w = case w of
>   Vacant -> False
>   PointOnly (_, _, bs) -> case unsnoc bs of
>     Nothing -> True
>     _ -> False
>   Coincide (_, _, bs) -> case unsnoc bs of
>     Nothing -> True
>     _ -> False
>   PointMark _ -> False
>   MarkPoint (_, _, _, _, cs) -> case unsnoc cs of
>     Nothing -> True
>     _ -> False

We can do the same for the mark, although it's less clear when this would be useful outside of tests.

> isMarkAtStart
>   :: ( Valued m a )
>   => TwoPointedList m a -> Bool
> isMarkAtStart w = case w of
>   Vacant -> False
>   PointOnly _ -> False
>   Coincide (as, _, _) -> case uncons as of
>     Nothing -> True
>     _ -> False
>   PointMark _ -> False
>   MarkPoint (as, _, _, _, _) -> case uncons as of
>     Nothing -> True
>     _ -> False
> 
> isMarkAtEnd
>   :: ( Valued m a )
>   => TwoPointedList m a -> Bool
> isMarkAtEnd w = case w of
>   Vacant -> False
>   PointOnly _ -> False
>   Coincide (_, _, bs) -> case unsnoc bs of
>     Nothing -> True
>     _ -> False
>   PointMark (_, _, _, _, cs) -> case unsnoc cs of
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
>   PointOnly (as, x, bs) -> case uncons as of
>     Nothing -> PointOnly (mempty, x, bs)
>     Just (a, as') -> PointOnly (mempty, a, snoc x as' <> bs)
>   Coincide (as, x, bs) -> case uncons as of
>     Nothing -> Coincide (mempty, x, bs)
>     Just (a, as') -> PointMark (mempty, a, as', x, bs)
>   PointMark (as, x, bs, y, cs) -> case uncons as of
>     Nothing -> PointMark (mempty, x, bs, y, cs)
>     Just (a, as') -> PointMark (mempty, a, snoc x as' <> bs, y, cs)
>   MarkPoint (as, x, bs, y, cs) -> case uncons as of
>     Nothing -> Coincide (mempty, x, bs <> cons y cs)
>     Just (a, as') -> PointMark (mempty, a, as', x, snoc y bs <> cs)
> 
> movePointToEnd
>   :: ( Valued m a )
>   => TwoPointedList m a -> TwoPointedList m a
> movePointToEnd w = case w of
>   Vacant -> Vacant
>   PointOnly (as, x, bs) -> case unsnoc bs of
>     Nothing -> PointOnly (as, x, mempty)
>     Just (b, bs') -> PointOnly (snoc x as <> bs', b, mempty)
>   Coincide (as, x, bs) -> case unsnoc bs of
>     Nothing -> Coincide (as, x, mempty)
>     Just (b, bs') -> MarkPoint (as, x, bs', b, mempty)
>   PointMark (as, x, bs, y, cs) -> case unsnoc cs of
>     Nothing -> Coincide (snoc x as <> bs, y, mempty)
>     Just (c, cs') -> MarkPoint (snoc x as <> bs, y, cs', c, mempty)
>   MarkPoint (as, x, bs, y, cs) -> case unsnoc cs of
>     Nothing -> MarkPoint (as, x, bs, y, mempty)
>     Just (c, cs') -> MarkPoint (as, x, bs <> cons y cs', c, mempty)

And we can move the point left and right by one item.

> movePointLeft
>   :: ( Valued m a )
>   => TwoPointedList m a -> TwoPointedList m a
> movePointLeft w = case w of
>   Vacant -> Vacant
>   PointOnly (as, x, bs) -> case unsnoc as of
>     Nothing -> PointOnly (mempty, x, bs)
>     Just (a, as') -> PointOnly (as', a, cons x bs)
>   Coincide (as, x, bs) -> case unsnoc as of
>     Nothing -> Coincide (mempty, x, bs)
>     Just (a, as') -> PointMark (as', a, mempty, x, bs)
>   PointMark (as, x, bs, y, cs) -> case unsnoc as of
>     Nothing -> PointMark (mempty, x, bs, y, cs)
>     Just (a, as') -> PointMark (as', a, cons x bs, y, cs)
>   MarkPoint (as, x, bs, y, cs) -> case unsnoc bs of
>     Nothing -> Coincide (as, x, cons y cs)
>     Just (b, bs') -> MarkPoint (as, x, bs', b, cons y cs)
> 
> movePointRight
>   :: ( Valued m a )
>   => TwoPointedList m a -> TwoPointedList m a
> movePointRight w = case w of
>   Vacant -> Vacant
>   PointOnly (as, x, bs) -> case uncons bs of
>     Nothing -> PointOnly (as, x, mempty)
>     Just (b, bs') -> PointOnly (snoc x as, b, bs')
>   Coincide (as, x, bs) -> case uncons bs of
>     Nothing -> Coincide (as, x, mempty)
>     Just (b, bs') -> MarkPoint (as, x, mempty, b, bs')
>   PointMark (as, x, bs, y, cs) -> case uncons bs of
>     Nothing -> Coincide (snoc x as, y, cs)
>     Just (b, bs') -> PointMark (snoc x as, b, bs', y, cs)
>   MarkPoint (as, x, bs, y, cs) -> case uncons cs of
>     Nothing -> MarkPoint (as, x, bs, y, mempty)
>     Just (c, cs') -> MarkPoint (as, x, snoc y bs, c, cs')

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
>   Coincide (as, x, bs) -> case uncons as of
>     Nothing -> Coincide (mempty, x, bs)
>     Just (a, as') -> MarkPoint (mempty, a, as', x, bs)
>   PointMark (as, x, bs, y, cs) -> case uncons as of
>     Nothing -> Coincide (mempty, x, bs <> cons y cs)
>     Just (a, as') -> MarkPoint (mempty, a, as', x, snoc y bs <> cs)
>   MarkPoint (as, x, bs, y, cs) -> case uncons as of
>     Nothing -> MarkPoint (mempty, x, bs, y, cs)
>     Just (a, as') -> MarkPoint (mempty, a, snoc x as' <> bs, y, cs)
> 
> moveMarkToEnd
>   :: ( Valued m a )
>   => TwoPointedList m a -> TwoPointedList m a
> moveMarkToEnd w = case w of
>   Vacant -> Vacant
>   PointOnly (as, x, bs) -> PointOnly (as, x, bs)
>   Coincide (as, x, bs) -> case unsnoc bs of
>     Nothing -> Coincide (as, x, mempty)
>     Just (b, bs') -> PointMark (as, x, bs', b, mempty)
>   PointMark (as, x, bs, y, cs) -> case unsnoc cs of
>     Nothing -> PointMark (as, x, bs, y, mempty)
>     Just (c, cs') -> PointMark (as, x, bs <> cons y cs', c, mempty)
>   MarkPoint (as, x, bs, y, cs) -> case unsnoc cs of
>     Nothing -> Coincide (snoc x as <> bs, y, mempty)
>     Just (c, cs') -> PointMark (snoc x as <> bs, y, cs', c, mempty)
> 
> moveMarkLeft
>   :: ( Valued m a )
>   => TwoPointedList m a -> TwoPointedList m a
> moveMarkLeft w = case w of
>   Vacant -> Vacant
>   PointOnly z -> PointOnly z
>   Coincide (as, x, bs) -> case unsnoc as of
>     Nothing -> Coincide (mempty, x, bs)
>     Just (a, as') -> MarkPoint (as', a, mempty, x, bs)
>   PointMark (as, x, bs, y, cs) -> case unsnoc bs of
>     Nothing -> Coincide (as, x, cons y cs)
>     Just (b, bs') -> PointMark (as, x, bs', b, cons y cs)
>   MarkPoint (as, x, bs, y, cs) -> case unsnoc as of
>     Nothing -> MarkPoint (mempty, x, bs, y, cs)
>     Just (a, as') -> MarkPoint (as', a, cons x bs, y, cs)
> 
> moveMarkRight
>   :: ( Valued m a )
>   => TwoPointedList m a -> TwoPointedList m a
> moveMarkRight w = case w of
>   Vacant -> Vacant
>   PointOnly z -> PointOnly z
>   Coincide (as, x, bs) -> case uncons bs of
>     Nothing -> Coincide (as, x, mempty)
>     Just (b, bs') -> PointMark (as, x, mempty, b, bs')
>   PointMark (as, x, bs, y, cs) -> case uncons cs of
>     Nothing -> PointMark (as, x, bs, y, mempty)
>     Just (c, cs') -> PointMark (as, x, snoc y bs, c, cs')
>   MarkPoint (as, x, bs, y, cs) -> case uncons bs of
>     Nothing -> Coincide (snoc x as, y, cs)
>     Just (b, bs') -> MarkPoint (snoc x as, b, bs', y, cs)



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
>     PointOnly (as, x, bs <> (cons y cs))
>   MarkPoint (as, x, bs, y, cs) ->
>     PointOnly (as <> (cons x bs), y, cs)

And of course we have efficient operations for insertion and deletion. First at the start and end:

> insertAtStart
>   :: ( Valued m a )
>   => a -> TwoPointedList m a -> TwoPointedList m a
> insertAtStart u w = case w of
>   Vacant ->
>     PointOnly (mempty, u, mempty)
>   PointOnly (as, x, bs) ->
>     PointOnly (cons u as, x, bs)
>   Coincide (as, x, bs) ->
>     Coincide (cons u as, x, bs)
>   PointMark (as, x, bs, y, cs) ->
>     PointMark (cons u as, x, bs, y, cs)
>   MarkPoint (as, x, bs, y, cs) ->
>     MarkPoint (cons u as, x, bs, y, cs)
> 
> deleteAtStart
>   :: ( Valued m a )
>   => TwoPointedList m a -> TwoPointedList m a
> deleteAtStart w = case w of
>   Vacant -> Vacant
>   PointOnly (as, x, bs) -> case uncons as of
>     Nothing -> case uncons bs of
>       Nothing -> Vacant
>       Just (b, bs') -> PointOnly (mempty, b, bs')
>     Just (_, as') -> PointOnly (as', x, bs)
>   Coincide (as, x, bs) -> case uncons as of
>     Nothing -> case uncons bs of
>       Nothing -> Vacant
>       Just (b, bs') -> Coincide (mempty, b, bs')
>     Just (_, as') -> Coincide (as', x, bs)
>   PointMark (as, x, bs, y, cs) -> case uncons as of
>     Nothing -> case uncons bs of
>       Nothing -> Coincide (mempty, y, cs)
>       Just (b, bs') -> PointMark (mempty, b, bs', y, cs)
>     Just (_, as') -> PointMark (as', x, bs, y, cs)
>   MarkPoint (as, x, bs, y, cs) -> case uncons as of
>     Nothing -> case uncons bs of
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
>     PointOnly (as, x, snoc u bs)
>   Coincide (as, x, bs) ->
>     Coincide (as, x, snoc u bs)
>   PointMark (as, x, bs, y, cs) ->
>     PointMark (as, x, bs, y, snoc u cs)
>   MarkPoint (as, x, bs, y, cs) ->
>     MarkPoint (as, x, bs, y, snoc u cs)
> 
> deleteAtEnd
>   :: ( Valued m a )
>   => TwoPointedList m a -> TwoPointedList m a
> deleteAtEnd w = case w of
>   Vacant -> Vacant
>   PointOnly (as, x, bs) -> case unsnoc bs of
>     Nothing -> case unsnoc as of
>       Nothing -> Vacant
>       Just (a, as') -> PointOnly (as', a, mempty)
>     Just (_, bs') -> PointOnly (as, x, bs')
>   Coincide (as, x, bs) -> case unsnoc bs of
>     Nothing -> case unsnoc as of
>       Nothing -> Vacant
>       Just (a, as') -> Coincide (as', a, mempty)
>     Just (_, bs') -> Coincide (as, x, bs')
>   PointMark (as, x, bs, y, cs) -> case unsnoc cs of
>     Nothing -> case unsnoc bs of
>       Nothing -> Coincide (as, x, mempty)
>       Just (b, bs') -> PointMark (as, x, bs', b, mempty)
>     Just (_, cs') -> PointMark (as, x, bs, y, cs')
>   MarkPoint (as, x, bs, y, cs) -> case unsnoc cs of
>     Nothing -> case unsnoc bs of
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
>     PointOnly (snoc u as, x, bs)
>   Coincide (as, x, bs) ->
>     Coincide (snoc u as, x, bs)
>   PointMark (as, x, bs, y, cs) ->
>     PointMark (snoc u as, x, bs, y, cs)
>   MarkPoint (as, x, bs, y, cs) ->
>     MarkPoint (as, x, snoc u bs, y, cs)
> 
> deletePointLeft
>   :: ( Valued m a )
>   => TwoPointedList m a -> TwoPointedList m a
> deletePointLeft w = case w of
>   Vacant ->
>     Vacant
>   PointOnly (as, x, bs) -> case unsnoc as of
>     Nothing -> PointOnly (mempty, x, bs)
>     Just (_, as') -> PointOnly (as', x, bs)
>   Coincide (as, x, bs) -> case unsnoc as of
>     Nothing -> Coincide (mempty, x, bs)
>     Just (_, as') -> Coincide (as', x, bs)
>   PointMark (as, x, bs, y, cs) -> case unsnoc as of
>     Nothing -> PointMark (mempty, x, bs, y, cs)
>     Just (_, as') -> PointMark (as', x, bs, y, cs)
>   MarkPoint (as, x, bs, y, cs) -> case unsnoc bs of
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
>     PointOnly (as, x, cons u bs)
>   Coincide (as, x, bs) ->
>     Coincide (as, x, cons u bs)
>   PointMark (as, x, bs, y, cs) ->
>     PointMark (as, x, cons u bs, y, cs)
>   MarkPoint (as, x, bs, y, cs) ->
>     MarkPoint (as, x, bs, y, cons u cs)
> 
> deletePointRight
>   :: ( Valued m a )
>   => TwoPointedList m a -> TwoPointedList m a
> deletePointRight w = case w of
>   Vacant ->
>     Vacant
>   PointOnly (as, x, bs) -> case uncons bs of
>     Nothing -> PointOnly (as, x, mempty)
>     Just (_, bs') -> PointOnly (as, x, bs')
>   Coincide (as, x, bs) -> case uncons bs of
>     Nothing -> Coincide (as, x, mempty)
>     Just (_, bs') -> Coincide (as, x, bs')
>   PointMark (as, x, bs, y, cs) -> case uncons bs of
>     Nothing -> Coincide (as, x, cs)
>     Just (_, bs') -> MarkPoint (as, x, bs', y, cs)
>   MarkPoint (as, x, bs, y, cs) -> case uncons cs of
>     Nothing -> PointMark (as, x, bs, y, mempty)
>     Just (_, cs') -> PointMark (as, x, bs, y, cs')



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

> valueAtPoint
>   :: ( Valued m a )
>   => TwoPointedList m a -> Maybe m
> valueAtPoint w = case w of
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
> valueAtMark
>   :: ( Valued m a )
>   => TwoPointedList m a -> Maybe m
> valueAtMark w = case w of
>   Vacant ->
>     Nothing
>   PointOnly (as, x, _) ->
>     Just (value as <> value x)
>   Coincide (as, x, _) ->
>     Just (value as <> value x)
>   PointMark (as, x, bs, y, _) ->
>     Just (value as <> value x <> value bs <> value y)
>   MarkPoint (as, x, _, _, _) ->
>     Just (value as <> value x)

Finally, we can remeasure a two-pointed list. This will be especially useful for working with text buffers, where the value will encode type level parameters.

> remeasure
>   :: ( Valued m1 a, Valued m2 a )
>   => TwoPointedList m1 a -> TwoPointedList m2 a
> remeasure w = case w of
>   Vacant -> Vacant
>   PointOnly (as, x, bs) ->
>     PointOnly (remeasureFT as, x, remeasureFT bs)
>   Coincide (as, x, bs) ->
>     Coincide (remeasureFT as, x, remeasureFT bs)
>   PointMark (as, x, bs, y, cs) ->
>     PointMark (remeasureFT as, x, remeasureFT bs, y, remeasureFT cs)
>   MarkPoint (as, x, bs, y, cs) ->
>     MarkPoint (remeasureFT as, x, remeasureFT bs, y, remeasureFT cs)



Splitting
---------

We can convert a two-pointed list back into a finger tree; this looks sort of like integration at the type level.

> integrate
>   :: ( Valued m a )
>   => TwoPointedList m a -> FingerTree m a
> integrate w = case w of
>   Vacant ->
>     mempty
>   PointOnly (as, x, bs) ->
>     as <> (cons x bs)
>   Coincide (as, x, bs) ->
>     as <> (cons x bs)
>   PointMark (as, x, bs, y, cs) ->
>     as <> (cons x bs) <> (cons y cs)
>   MarkPoint (as, x, bs, y, cs) ->
>     as <> (cons x bs) <> (cons y cs)

Conversely, we can use split on finger trees to manipulate the point and mark of a two-pointed list. This is the operation that makes two-pointed lists practical; with an appropriate value annotation we can use it to get log complexity random access.

> split
>   :: ( Valued m a )
>   => (m -> Bool)       -- point predicate
>   -> Maybe (m -> Bool) -- mark predicate
>   -> TwoPointedList m a -> Maybe (TwoPointedList m a)
> split pointP q w =
>   let xs = integrate w
>   in case splitFT pointP xs of
>     Nothing -> Nothing
>     Just (as, x, bs) -> Just $ case q of
>       Nothing -> PointOnly (as, x, bs)
>       Just markP -> case splitFT markP as of
>         Just (us, y, vs) ->
>           MarkPoint (us, y, vs, x, bs)
>         Nothing -> case splitFT markP bs of
>           Just (us, y, vs) ->
>             PointMark (as, x, us, y, vs)
>           Nothing -> PointOnly (as, x, bs)



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

> showInternal
>   :: ( Show m, Show a, Valued m a )
>   => TwoPointedList m a -> String
> showInternal w = case w of
>   Vacant -> "Vacant"
>   PointOnly (as, x, bs) -> concat
>     [ "PointOnly "
>     , "( ", showInternalFT as
>     , ", ", show x
>     , ", ", showInternalFT bs
>     , " )"
>     ]
>   Coincide (as, x, bs) -> concat
>     [ "Coincide "
>     , "( ", showInternalFT as
>     , ", ", show x
>     , ", ", showInternalFT bs
>     , " )"
>     ]
>   PointMark (as, x, bs, y, cs) -> concat
>     [ "PointMark "
>     , "( ", showInternalFT as
>     , ", ", show x
>     , ", ", showInternalFT bs
>     , ", ", show y
>     , ", ", showInternalFT cs
>     , " )"
>     ]
>   MarkPoint (as, x, bs, y, cs) -> concat
>     [ "MarkPoint "
>     , "( ", showInternalFT as
>     , ", ", show x
>     , ", ", showInternalFT bs
>     , ", ", show y
>     , ", ", showInternalFT cs
>     , " )"
>     ]

> validate
>   :: ( Eq m, Valued m a )
>   => TwoPointedList m a -> Bool
> validate w = case w of
>   Vacant -> True
>   PointOnly (as, _, bs) ->
>     (validateFT as) && (validateFT bs)
>   Coincide (as, _, bs) ->
>     (validateFT as) && (validateFT bs)
>   PointMark (as, _, bs, _, cs) ->
>     (validateFT as) && (validateFT bs) && (validateFT cs)
>   MarkPoint (as, _, bs, _, cs) ->
>     (validateFT as) && (validateFT bs) && (validateFT cs)

> toListDebug
>   :: ( Valued m a )
>   => TwoPointedList m a -> [(a, m)]
> toListDebug w = case w of
>   Vacant -> []
>   PointOnly (as, x, bs) ->
>     toListDebugFT (as <> (cons x bs))
>   Coincide (as, x, bs) ->
>     toListDebugFT (as <> (cons x bs))
>   PointMark (as, x, bs, y, cs) ->
>     toListDebugFT (as <> (cons x bs) <> (cons y cs))
>   MarkPoint (as, x, bs, y, cs) ->
>     toListDebugFT (as <> (cons x bs) <> (cons y cs))




