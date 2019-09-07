---
title: Kreb.Struct.FingerTree
---

<blockquote>
I can whistle with my fingers, especially if I have a whistle.

<cite>Mitch Hedberg</cite>
</blockquote>

In this module we develop the core data structure of our text editor: _finger trees_.



Contents
--------

* [Introduction](#introduction)
* [Exposed API](#exposed-api)
* [The Valued Class](#the-valued-class)
* [Internal Types](#internal-types)
* [Finger Trees](#finger-trees)
    * [Cons and Uncons](#cons-and-uncons)
    * [Concatenation](#concatenation)
    * [Splitting](#splitting)
    * [Taking subsequences](#taking-subsequences)
    * [Debugging](#debugging)



Introduction
============

The most basic requirement of a text editor is surely that it provide some mechanism for modeling and manipulating strings of characters. This mechanism had better be pretty robust and efficient, too, at least for common operations, since it will be doing most of the work during everyday use. Several different data structures have been developed for this purpose, including gap buffers and piece tables, but not all of them translate cleanly to a language like Haskell where mutation is not allowed (or at least very strictly controlled) and evaluation is lazy by default.

Choosing a data structure is all about making tradeoffs. We think about our particular application and what data and operations it needs, then choose a structure that makes those operations efficient. With that in mind, what exactly do we need for our simple text editor? I can think of a few things.

1. Our data structure should essentially model a list of characters; that is, whatever weird branching or in-place mutating business is happening behind the scenes, the API is more or less that of lists.
2. Typically when editing text we've got a distinguished position called the _cursor_ where our editing actions (insert, delete) have immediate effect. It is vital that interacting with and moving the cursor around be blazing fast, since that's where most of the work happens.
3. Another very common action when editing text is _search_. This takes a few forms -- jumping to a particular line and column position, or searching for a literal substring, or looking for matches to a given regular expression.

There are others -- fast syntax highlighting is nice, for instance -- but these are the absolute essentials.

Lucky for us, there's a powerful data structure that gives us all three, based on _finger trees_. These were introduced in the present form in the paper [_Finger trees: a simple general-purpose data structure_](http://www.staff.city.ac.uk/~ross/papers/FingerTree.html) by Hinze and Paterson. The funny name refers to the fact that trees of this type have _fingers_ -- distinguished locations where read and write access is cheap.

In this module we'll develop an API for working with finger trees by following the Hinze and Paterson paper pretty closely. There are existing implementations of this that we could use instead, but finger trees are quite elegant and seeing how they work by rolling our own is a worthwhile exercise.

This code uses the following compiler extensions:

> {-# LANGUAGE
>     MultiParamTypeClasses
>   , ScopedTypeVariables
>   , FlexibleContexts
>   , FlexibleInstances
>   , DeriveGeneric
>   , InstanceSigs
>   , Rank2Types
> #-}

Some of these are essential, while others are just convenient. We will address them as we come to them.



Exposed API
===========

> module Kreb.Struct.FingerTree (
>   -- ** Valued
>     Valued(..)
>   , Count(..)
> 
>   -- ** FingerTree
>   , FingerTree()
>   , fmapFT
> 
>   -- * Constructors
>   , fromListFT
>   , leaf
>   , cons
>   , snoc
> 
>   -- * Destructors
>   , uncons
>   , unsnoc
> 
>   -- * Operators
>   , reverseFT
>   , remeasureFT
>   , takeWhileValueFT
>   , breakPrefixWhileValueFT
> 
>   -- * Queries
>   , isEmptyFT
>   , notEmptyFT
>   , isLeafFT
> 
>   -- * Splitting
>   , splitFT
>   , splitTree
>   , takeFromSplitFT
> 
>   -- ** Testing and Debugging
>   , toListDebugFT
>   , depthFT
>   , showInternalFT
>   , validateFT
> ) where
> 
> import GHC.Generics
> 
> import Data.Monoid
> import Data.Foldable
> import Data.List (unwords)

> import Debug.Trace

> import Kreb.Check
>   ( Arb(..), Prune(..), CoArb(..)
>   , MakeTo(..), makeToIntegralWith, makeToExtendWith )



The Valued Class
================

The secret power of finger trees is that they aren't just _a_ data structure. They are a whole _family_ of data structures parameterized by a monoid. Different choices of this monoid give structures with different characteristics, but all are constructed and accessed in the same way.

The _content_ type `a` of a finger tree must have a distinguished mapping into some monoid type `m`. We can enforce this in Haskell using a typeclass with two parameters. It looks a little bit like we're saying that `a` has an `m`-valuation, so we'll call this class `Valued`.

> class
>   ( Monoid m
>   ) => Valued m a
>   where
>     value :: a -> m

This is where the `MultiParamTypeClasses` extension is needed. And the extension is genuinely necessary here; in general we may want multiple valuations on the same `a`, and likewise multiple types valued by the same `m`. So as far as I can see we can't drop MPTC in favor of some other mechanism like associated type families.

Note that the `Valued` class does not impose any laws. It can't, really, because at this level of abstraction there's no structure on `a` to relate to that of `m`. Normally this is a design smell -- generally typeclasses should come with laws -- but here we'll let it slide.

Now lets define a concrete monoid and `Valued` instance for testing. Note that while `Valued` is a many-to-many type class constraint, for most applications we'll want to define a specific monoid type `m` just for that purpose -- even if it's just a `newtype` wrapper around some other type. For example, here's a wrapper around `Int` that is a monoid under addition.

> data Count
>   = Count Int
>   deriving (Eq, Show, Generic)
> 
> instance Semigroup Count where
>   (Count a) <> (Count b) = Count (a + b)
> 
> instance Monoid Count where
>   mempty = Count 0
> 
> instance Arb Count where
>   arb = Count <$> arb
> 
> instance Prune Count where
>   prune (Count k) =
>     map Count $ prune k
> 
> instance CoArb Count where
>   coarb (Count k) = coarb k
> 
> instance MakeTo Count where
>   makeTo = makeToIntegralWith g h
>     where
>       g :: Count -> Integer
>       g (Count k) = fromIntegral k
> 
>       h :: Integer -> Count
>       h k = Count $ fromInteger $ abs k



> instance Valued Count Char where
>   value _ = Count 1
> 
> instance
>   ( Valued Count a
>   ) => Valued Count (a,b)
>   where
>     value (a,_) = value a



Internal Types
==============

Finger trees exhibit a phenomenon called _polymorphic recursion_, where at each level the 'inner' and 'outer' structures have different types. First we have the 'fingers' to the left and the right, which can number from one to four on each side. We represent this with the `Some` class.

> data Some m a
>   = Only1 m a
>   | Only2 m a a
>   | Only3 m a a a
>   | Only4 m a a a a
>   deriving (Eq, Show)

Note the `m` parameter; this is the cached `value` of the internal data.

> instance
>   ( Monoid m
>   ) => Valued m (Some m a)
>   where
>     value w = case w of
>       Only1 m _ -> m
>       Only2 m _ _ -> m
>       Only3 m _ _ _ -> m
>       Only4 m _ _ _ _ -> m

To maintain the invariant that our cached `value` is accurate, it's important that we only create `Some` values using the following _smart constructors_:

> only1 :: (Valued m a) => a -> Some m a
> only1 a1 = Only1 m a1
>   where m = value a1
> 
> only2
>   :: ( Valued m a )
>   => a -> a -> Some m a
> only2 a1 a2 =
>   let
>     m = mconcat
>       [ value a1, value a2 ]
>   in Only2 m a1 a2 
> 
> only3 :: (Valued m a) => a -> a -> a -> Some m a
> only3 a1 a2 a3 =
>   let
>     m = mconcat
>       [ value a1, value a2, value a3 ]
>   in Only3 m a1 a2 a3 
> 
> only4 :: (Valued m a) => a -> a -> a -> a -> Some m a
> only4 a1 a2 a3 a4 =
>   let
>     m = mconcat
>       [ value a1, value a2, value a3, value a4 ]
>   in Only4 m a1 a2 a3 a4 

These constructors are not exposed outside this module since they are only used in the internal representation. For instance, we use them to define something like `fmap` for `Some` -- although we can't use the `Functor` typeclass for this due to the `Valued` constraint.

> fmapSome
>   :: ( Valued m1 a1, Valued m2 a2 )
>   => (a1 -> a2) -> Some m1 a1 -> Some m2 a2
> fmapSome f x = case x of
>   Only1 _ u1 ->
>     only1 (f u1)
>   Only2 _ u1 u2 ->
>     only2 (f u1) (f u2)
>   Only3 _ u1 u2 u3 ->
>     only3 (f u1) (f u2) (f u3)
>   Only4 _ u1 u2 u3 u4 ->
>     only4 (f u1) (f u2) (f u3) (f u4)

Next we have a `Foldable` instance (this is where we use `InstanceSigs` for clarity):

> instance Foldable (Some m) where
>  toList
>    :: Some m a -> [a]
>  toList w = case w of
>    Only1 _ a1 -> [a1]
>    Only2 _ a1 a2 -> [a1, a2]
>    Only3 _ a1 a2 a3 -> [a1, a2, a3]
>    Only4 _ a1 a2 a3 a4 -> [a1, a2, a3, a4]
> 
>  foldr
>    :: (a -> b -> b) -> b -> Some m a -> b
>  foldr f b w = case w of
>    Only1 _ a1 ->
>      f a1 b
>    Only2 _ a1 a2 ->
>      f a1 (f a2 b)
>    Only3 _ a1 a2 a3 ->
>      f a1 (f a2 (f a3 b))
>    Only4 _ a1 a2 a3 a4 ->
>      f a1 (f a2 (f a3 (f a4 b)))
> 
>  foldl
>    :: (b -> a -> b) -> b -> Some m a -> b
>  foldl f b w = case w of
>    Only1 _ a1 ->
>      f b a1
>    Only2 _ a1 a2 ->
>      f (f b a1) a2
>    Only3 _ a1 a2 a3 ->
>      f (f (f b a1) a2) a3
>    Only4 _ a1 a2 a3 a4 ->
>      f (f (f (f b a1) a2) a3) a4

This instance is mainly used as a helper for defining the instance for `FingerTree`.

Next we have the nested tree type. Finger trees are basically rearranged 2-3 trees, and the nested `Node` type represents internal branching nodes of this kind.

> data Node m a
>  = Node2 m a a
>  | Node3 m a a a
>  deriving (Eq, Show)

Again we cache the value of the internal data, using smart constructors to maintain the integrity of the cache.

> instance
>   ( Monoid m
>   ) => Valued m (Node m a)
>   where
>     value w = case w of
>       Node2 m _ _ -> m
>       Node3 m _ _ _ -> m
> 
> node2
>   :: ( Valued m a )
>   => a -> a -> Node m a
> node2 a1 a2 =
>   let
>     m = mconcat
>       [ value a1, value a2 ]
>   in Node2 m a1 a2
> 
> node3
>   :: ( Valued m a )
>   => a -> a -> a -> Node m a
> node3 a1 a2 a3 =
>   let
>     m = mconcat
>       [ value a1, value a2, value a3 ]
>   in Node3 m a1 a2 a3

From here we can give something like `fmap` for `Node`, again outside of the usual typeclass because of the `Valued` constraint.

> fmapNode
>   :: ( Valued m1 a1, Valued m2 a2 )
>   => (a1 -> a2) -> Node m1 a1 -> Node m2 a2
> fmapNode f x = case x of
>   Node2 _ u1 u2 ->
>     node2 (f u1) (f u2)
>   Node3 _ u1 u2 u3 ->
>     node3 (f u1) (f u2) (f u3)

And we need a `Foldable` instance:

> instance Foldable (Node m) where
>  foldr
>    :: (a -> b -> b) -> b -> Node m a -> b
>  foldr f b w = case w of
>    Node2 _ a1 a2 ->
>      f a1 (f a2 b)
>    Node3 _ a1 a2 a3 ->
>      f a1 (f a2 (f a3 b))
> 
>  foldl
>    :: (b -> a -> b) -> b -> Node m a -> b
>  foldl f b w = case w of
>    Node2 _ a1 a2 ->
>      f (f b a1) a2
>    Node3 _ a1 a2 a3 ->
>      f (f (f b a1) a2) a3

Finally, note that `Node` can be thought of as a strict subset of `Some`. We use a helper function, `toSome`, to make this formal.

> toSome
>   :: ( Valued m a )
>   => Node m a -> Some m a
> toSome w = case w of
>   Node2 _ a1 a2 -> only2 a1 a2
>   Node3 _ a1 a2 a3 -> only3 a1 a2 a3



Finger Trees
============

Now for the big show. A finger tree is either empty, or consists of a single node (with its cached value), or has some left and right fingers with a nested finger tree in the middle (with the cached product of their values).

> data FingerTree m a
>  = Stump
>  | Leaf m a
>  | Branch m (Some m a) (FingerTree m (Node m a)) (Some m a)

As with `Some` and `Node`, `FingerTree` inherits an instance of `Valued` and to maintain integrity we must only use smart constructors to define them.

> instance
>   ( Valued m a
>   ) => Valued m (FingerTree m a)
>   where
>     value w = case w of
>       Stump          -> mempty
>       Leaf   m _     -> m
>       Branch m _ _ _ -> m
> 
> stump
>   :: FingerTree m a
> stump = Stump
> 
> leaf
>   :: ( Valued m a )
>   => a
>   -> FingerTree m a
> leaf a = Leaf m a
>   where m = value a
> 
> branch
>   :: ( Valued m a )
>   => Some m a -> FingerTree m (Node m a) -> Some m a
>   -> FingerTree m a
> branch heads mids lasts =
>   let
>      m = mconcat
>        [ value heads, value mids, value lasts ]
>   in Branch m heads mids lasts

> isLeafFT
>   :: ( Valued m a )
>   => FingerTree m a -> Bool
> isLeafFT x = case x of
>   Leaf _ _ -> True
>   _ -> False

And we can define something like `fmap` for finger trees:

> fmapFT
>   :: forall m1 m2 a1 a2
>    . ( Valued m1 a1, Valued m2 a2 )
>   => (a1 -> a2) -> FingerTree m1 a1 -> FingerTree m2 a2
> fmapFT f x = case x of
>   Stump -> stump
>   Leaf _ a -> leaf (f a)
>   Branch _ as bs cs -> branch
>     (fmapSome f as)
>     (fmapFT (fmapNode f) bs)
>     (fmapSome f cs)

With this version of `fmap` we can do something interesting. Mapping with `id` can swap out the cached value monoid.

> remeasureFT
>  :: forall m1 m2 a
>   . ( Valued m1 a, Valued m2 a )
>  => FingerTree m1 a -> FingerTree m2 a
> remeasureFT = fmapFT id

Next we define some simple structural queries on finger trees. First to detect whether the tree is empty:

> isEmptyFT
>   :: FingerTree m a -> Bool
> isEmptyFT x = case x of
>   Stump -> True
>   _     -> False
> 
> notEmptyFT
>   :: FingerTree m a -> Bool
> notEmptyFT x = case x of
>   Stump -> False
>   _     -> True

We can also compute the 'depth' of the tree. This function is only exposed for testing purposes. During normal use the internal structure of a finger tree is of no interest to us, but this will help us to ensure good test coverage later.

> depthFT
>  :: FingerTree m a -> Int
> depthFT x = case x of
>  Stump -> 0
>  Leaf _ _ -> 1
>  Branch _ _ z _ -> 1 + depthFT z

Now we can define a `Foldable` instance:

> instance
>   Foldable (FingerTree m)
>   where
>     foldr
>       :: (a -> b -> b) -> b -> FingerTree m a -> b
>     foldr f b w = case w of
>       Stump -> b
>       Leaf _ a -> f a b
>       Branch _ as1 as2 as3 ->
>         foldr f
>           (foldr (flip (foldr f)) (foldr f b as3) as2) as1
> 
>     foldl
>       :: (b -> a -> b) -> b -> FingerTree m a -> b
>     foldl f b w = case w of
>       Stump -> b
>       Leaf _ a -> f b a
>       Branch _ as1 as2 as3 ->
>         foldl f
>           (foldl (foldl f) (foldl f b as1) as2) as3

Note that we didn't derive the `Eq` instance for `FingerTree`. This is because in some sense the tree structure itself is only incidental, used for maintaining the fingers and caching values. The meat of the structure is the left-to-right traversal of the leaf nodes and fingers, and the derived (structural) equality instance would be too granular. To get around this we'll instead check for equality on `FingerTree`s by converting to lists first, using the `Foldable` instance.

> instance
>   ( Eq a
>   ) => Eq (FingerTree m a)
>   where
>     a == b =
>       (toList a) == (toList b)

As every `Node` can be converted into a `Some`, every `Some` can be converted to a `FingerTree`. This conversion will come in handy later so we define it here, though this code is not exposed outside of this module.

> someToFingerTree
>   :: ( Valued m a )
>   => Some m a -> FingerTree m a
> someToFingerTree w = case w of
>   Only1 _ a1 ->
>     leaf a1
>   Only2 _ a1 a2 ->
>     branch (only1 a1) stump (only1 a2)
>   Only3 _ a1 a2 a3 ->
>     branch (only2 a1 a2) stump (only1 a3)
>   Only4 _ a1 a2 a3 a4 ->
>     branch (only2 a1 a2) stump (only2 a3 a4)
> 
> maybeToFingerTree
>   :: ( Valued m a )
>   => Maybe (Some m a) -> FingerTree m a
> maybeToFingerTree w = case w of
>   Nothing -> stump
>   Just z -> someToFingerTree z



Cons and Uncons
---------------

`cons` is a traditional name for the function that appends an item to the head of the list; it originates in Lisp. For finger trees, if the left side does not have a full complement of fingers then `cons` is a constant time operation. If it does, then we take some of the fingers and recursively `cons` them as a node to the inner tree.

> cons
>  :: ( Valued m a )
>  => a -> FingerTree m a -> FingerTree m a
> cons u w = case w of
>  Stump -> leaf u
>  Leaf _ a -> branch (only1 u) stump (only1 a)
>  Branch _ as1 as2 as3 -> case as1 of
>    Only1 _ a1 ->
>      branch (only2 u a1) as2 as3
>    Only2 _ a1 a2 ->
>      branch (only3 u a1 a2) as2 as3
>    Only3 _ a1 a2 a3 ->
>      branch (only4 u a1 a2 a3) as2 as3
>    Only4 _ a1 a2 a3 a4 ->
>      branch (only2 u a1) (cons (node3 a2 a3 a4) as2) as3

With `cons` in hand we can now write a helper for converting lists into finger trees; this is handy for testing.

> fromListFT
>  :: ( Valued m a )
>  => [a] -> FingerTree m a
> fromListFT = foldr cons stump

And with `fromListFT` we can also give a convenient `Show` instance. Note that the derived instance would include a lot of superfluous information about the internal structure of the tree.

> instance
>   ( Show a
>   ) => Show (FingerTree m a)
>   where
>     show a = "fromListFT " ++ show (toList a)

The mirror operation -- appending on the right -- is defined similarly. This is called `snoc`.

> snoc
>  :: ( Valued m a )
>  => a -> FingerTree m a -> FingerTree m a
> snoc u w = case w of
>  Stump -> leaf u
>  Leaf _ a -> branch (only1 a) stump (only1 u)
>  Branch _ as1 as2 as3 -> case as3 of
>    Only1 _ a1 ->
>      branch as1 as2 (only2 a1 u)
>    Only2 _ a1 a2 ->
>      branch as1 as2 (only3 a1 a2 u)
>    Only3 _ a1 a2 a3 ->
>      branch as1 as2 (only4 a1 a2 a3 u)
>    Only4 _ a1 a2 a3 a4 ->
>      branch as1 (snoc (node3 a1 a2 a3) as2) (only2 a4 u)

With `snoc` we can reverse the tree.

> reverseFT
>   :: ( Valued m a )
>   => FingerTree m a -> FingerTree m a
> reverseFT = foldr snoc mempty

`cons` also has an inverse, called `uncons`. (The inverse here is not literal, but can be made so if instead of cons we consider the coproduct of cons with the constant `stump` function, but for our purposes here that's splitting hairs.)

Like `cons`, `uncons` is very fast if the left side of the tree has fingers to spare. But if not, we have to do a kind of recursive borrowing.

> uncons
>  :: ( Valued m a )
>  => FingerTree m a
>  -> Maybe (a, FingerTree m a)
> uncons w = case w of
>  Stump -> Nothing
>  Leaf _ a -> Just (a, stump)
>  Branch _ as1 as2 as3 ->
>    let (a, as) = uncons' as1
>    in Just (a, borrowL as as2 as3)
> 
> uncons'
>  :: ( Valued m a )
>  => Some m a
>  -> (a, Maybe (Some m a))
> uncons' w = case w of
>  Only1 _ a1 -> (a1, Nothing)
>  Only2 _ a1 a2 -> (a1, Just (only1 a2))
>  Only3 _ a1 a2 a3 -> (a1, Just (only2 a2 a3))
>  Only4 _ a1 a2 a3 a4 -> (a1, Just (only3 a2 a3 a4))
> 
> borrowL
>  :: ( Valued m a )
>  => Maybe (Some m a)
>  -> FingerTree m (Node m a)
>  -> Some m a
>  -> FingerTree m a
> borrowL w as2 as3 = case w of
>  Just as1 -> branch as1 as2 as3
>  Nothing -> case uncons as2 of
>    Nothing -> someToFingerTree as3
>    Just (a, as) -> branch (toSome a) as as3

And there's a similar analogue for `snoc`.

> unsnoc
>  :: ( Valued m a )
>  => FingerTree m a
>  -> Maybe (a, FingerTree m a)
> unsnoc w = case w of
>  Stump -> Nothing
>  Leaf _ a -> Just (a, stump)
>  Branch _ as1 as2 as3 ->
>    let (a, as) = unsnoc' as3
>    in Just (a, borrowR as1 as2 as)
> 
> unsnoc'
>  :: ( Valued m a )
>  => Some m a
>  -> (a, Maybe (Some m a))
> unsnoc' w = case w of
>  Only1 _ a1 -> (a1, Nothing)
>  Only2 _ a1 a2 -> (a2, Just (only1 a1))
>  Only3 _ a1 a2 a3 -> (a3, Just (only2 a1 a2))
>  Only4 _ a1 a2 a3 a4 -> (a4, Just (only3 a1 a2 a3))
> 
> borrowR
>  :: ( Valued m a )
>  => Some m a
>  -> FingerTree m (Node m a)
>  -> Maybe (Some m a)
>  -> FingerTree m a
> borrowR as1 as2 w = case w of
>  Just as3 -> branch as1 as2 as3
>  Nothing -> case unsnoc as2 of
>    Nothing -> someToFingerTree as1
>    Just (a, as) -> branch as1 as (toSome a)



Concatenation
-------------

Before defining concatenation proper, we start with a generalized version that takes an additional list of elements to insert between the concatenands.

> cat'
>  :: ( Valued m a )
>  => FingerTree m a
>  -> [a]
>  -> FingerTree m a
>  -> FingerTree m a
> cat' u as v = case u of
>  Stump -> foldr cons v as
>  Leaf _ a -> cons a (foldr cons v as)
>  Branch _ us1 us2 us3 -> case v of
>    Stump -> foldl (flip snoc) u as
>    Leaf _ a -> snoc a (foldl (flip snoc) u as)
>    Branch _ vs1 vs2 vs3 ->
>      let ns = (toList us3) ++ as ++ (toList vs1)
>      in branch us1 (cat' us2 (toNodes ns) vs2) vs3
> 
> toNodes
>  :: ( Valued m a )
>  => [a] -> [Node m a]
> toNodes w = case w of
>  [] -> []
>  [a1, a2] -> [node2 a1 a2]
>  [a1, a2, a3] -> [node3 a1 a2 a3]
>  [a1, a2, a3, a4] -> [node2 a1 a2, node2 a3 a4]
>  a1:a2:a3:a4:as -> (node3 a1 a2 a3) : toNodes (a4:as)
>  _ -> error "toNodes: panic"

Now the real `cat` is a specialization of `cat'`:

> cat
>  :: ( Valued m a )
>  => FingerTree m a
>  -> FingerTree m a
>  -> FingerTree m a
> cat u v = cat' u [] v

And concat makes the type of finger trees into a monoid.

> instance
>  ( Valued m a
>  ) => Semigroup (FingerTree m a)
>  where
>    (<>) = cat
> 
> instance
>  ( Valued m a
>  ) => Monoid (FingerTree m a)
>  where
>    mempty = stump



Splitting
---------

The killer operation on finger trees, and the reason for caching the `m` value at each node, is efficient _splitting_. This operation takes a predicate `p` on `m` and a finger tree `w` and attempts to break it into three pieces, `as`, `x`, and `bs`, with the property that `w == as <> leaf x <> bs`, `p (value as)` is false, and `p (value as <> value x)` is true. The internal structure of finger trees allow this to be done efficiently. In general there may be many possible places to break the tree, but with a judicious choice of `m` and the predicate we can perform some complex operations quickly.

First we need a version of this operation on the fingers.

> splitSome
>  :: ( Valued m a )
>  => (m -> Bool) -> m -> Some m a
>  -> Maybe (Maybe (Some m a), a, Maybe (Some m a))
> splitSome p i w = case w of
>  Only1 _ a1 ->
>    let v1 = i <> (value a1) in
>    if p v1
>      then Just (Nothing, a1, Nothing)
>      else Nothing
>  Only2 _ a1 a2 ->
>    let v1 = i <> (value a1) in
>    if p v1
>      then Just (Nothing, a1, Just (only1 a2))
>      else
>        let v2 = v1 <> (value a2) in
>        if p v2
>          then Just (Just (only1 a1), a2, Nothing)
>          else Nothing
>  Only3 _ a1 a2 a3 ->
>    let v1 = i <> (value a1) in
>    if p v1
>      then Just (Nothing, a1, Just (only2 a2 a3))
>      else
>        let v2 = v1 <> (value a2) in
>        if p v2
>          then Just (Just (only1 a1), a2, Just (only1 a3))
>          else
>            let v3 = v2 <> (value a3) in
>            if p v3
>              then Just (Just (only2 a1 a2), a3, Nothing)
>              else Nothing
>  Only4 _ a1 a2 a3 a4 ->
>    let v1 = i <> (value a1) in
>    if p v1
>      then Just (Nothing, a1, Just (only3 a2 a3 a4))
>      else
>        let v2 = v1 <> (value a2) in
>          if p v2
>            then Just (Just (only1 a1), a2, Just (only2 a3 a4))
>            else
>              let v3 = v2 <> (value a3) in
>                if p v3
>                  then Just (Just (only2 a1 a2), a3, Just (only1 a4))
>                  else
>                    let v4 = v3 <> (value a4) in
>                      if p v4
>                        then Just (Just (only3 a1 a2 a3), a4, Nothing)
>                        else Nothing

Next we define a generalized split that prepends a given monoid value to the left of `value as`.

> splitTree
>  :: ( Valued m a )
>  => (m -> Bool) -> m -> FingerTree m a
>  -> Maybe (FingerTree m a, a, FingerTree m a)
> splitTree p i w =
>   if p i
>     then case uncons w of
>       Nothing -> Nothing
>       Just (x,bs) -> Just (mempty, x, bs)
>     else case w of
>       Stump -> Nothing
>       Leaf _ a ->
>         let va = i <> (value a) in
>         if p va
>           then Just (stump, a, stump)
>           else Nothing
>       Branch _ as1 as2 as3 ->
>         let vas1 = i <> (value as1) in
>         if p vas1
>           -- there's a match in the left fingers
>           then case (splitSome p i as1) of
>             Nothing -> error "split: panic (1)"
>             Just (ds1, x, ds3) ->
>               let
>                 bs1 = maybeToFingerTree ds1
>                 bs3 = borrowL ds3 as2 as3
>               in
>                 Just (bs1, x, bs3)
>           else
>             let vas2 = vas1 <> (value as2) in
>             if p vas2
>               -- there's a match in the spine
>               then case splitTree p vas1 as2 of
>                 Nothing -> error "split: panic (2)"
>                 Just (cs1, xs, cs3) ->
>                   let vs = vas1 <> (value cs1) in
>                   case splitSome p vs (toSome xs) of
>                     Nothing -> error "split: panic (3)"
>                     Just (ds1, x, ds3) ->
>                       let
>                         bs1 = borrowR as1 cs1 ds1
>                         bs3 = borrowL ds3 cs3 as3
>                       in
>                         Just (bs1, x, bs3)
>               else
>                 let vas3 = vas2 <> (value as3) in
>                 if p vas3
>                   -- there's a match in the right fingers
>                   then case (splitSome p vas2 as3) of
>                     Nothing -> error "split: panic (4)"
>                     Just (ds1, x, ds3) ->
>                       let
>                         bs1 = borrowR as1 as2 ds1
>                         bs3 = maybeToFingerTree ds3
>                       in
>                         Just (bs1, x, bs3)
>                   else Nothing -- precondition violated

Now the general `split` function specializes to the identity.

> splitFT
>  :: ( Valued m a )
>  => (m -> Bool)
>  -> FingerTree m a
>  -> Maybe (FingerTree m a, a, FingerTree m a)
> splitFT p w = splitTree p mempty w

> takeFromSplitFT
>   :: forall m a
>    . ( Valued m a )
>   => (m -> Bool)
>   -> (Int -> m -> Bool)
>   -> Int
>   -> FingerTree m a
>   -> [(FingerTree m a, m)]
> takeFromSplitFT p q k w =
>   case splitFT p w of
>     Nothing -> []
>     Just (as,x,bs) -> f 0 (cons x bs) (value as :: m) []
>   where
>     f
>       :: Int -> FingerTree m a -> m
>       -> [(FingerTree m a, m)] -> [(FingerTree m a, m)]
>     f t z m us =
>       if (isEmptyFT z) || (t >= k)
>         then reverse us
>         else case splitTree (q t) m z of
>           Nothing -> reverse us
>           Just (as,x,bs) ->
>             f (t+1) bs (m <> value (snoc x as)) ((snoc x as,m):us)



Taking subsequences
-------------------

Now we have some miscellaneous functions for taking subsequences of a finger tree.

> breakPrefixWhileValueFT
>   :: forall m a
>    . ( Valued m a )
>   => (m -> Bool) -> FingerTree m a
>   -> (FingerTree m a, FingerTree m a)
> breakPrefixWhileValueFT p xs =
>   break mempty xs
>   where
>     break
>       :: FingerTree m a -> FingerTree m a
>       -> (FingerTree m a, FingerTree m a)
>     break as bs = case uncons bs of
>       Nothing -> (as, mempty)
>       Just (x,cs) ->
>         if p (value as <> value x)
>           then break (snoc x as) cs
>           else (as, bs)

> takeWhileValueFT
>  :: ( Valued m a )
>  => m -> (m -> Bool) -> FingerTree m a -> [(a, m)]
> takeWhileValueFT e p xs =
>  case uncons xs of
>    Nothing -> []
>    Just (a, ys) ->
>      let z = e <> value a in
>      if p z
>        then (a, z) : takeWhileValueFT z p ys
>        else []



Debugging
---------

Even the most thoroughly tested code can go wrong sometimes. For when that happens, we'll also provide a basic debugging function that exposes the accumulated value of the finger tree at each position.

> validateFT
>   :: ( Eq m, Valued m a )
>   => FingerTree m a -> Bool
> validateFT = validateFT' (const True)
> 
> validateFT'
>   :: ( Eq m, Valued m a )
>   => (a -> Bool) -> FingerTree m a -> Bool
> validateFT' valid xs = case xs of
>   Stump ->
>     True
>   Leaf m a ->
>     m == value a
>   Branch m as x bs -> and
>     [ validateSome valid as
>     , validateFT' (validateNode valid) x
>     , validateSome valid bs
>     , m == mconcat [ value as, value x, value bs ]
>     ]
> 
> validateSome
>   :: ( Eq m, Valued m a )
>   => (a -> Bool) -> Some m a -> Bool
> validateSome valid x = case x of
>   Only1 m a1 -> and
>     [ m == value a1
>     , valid a1
>     ]
>   Only2 m a1 a2 -> and
>     [ m == mconcat [ value a1, value a2 ]
>     , valid a1, valid a2
>     ]
>   Only3 m a1 a2 a3 -> and
>     [ m == mconcat [ value a1, value a2, value a3 ]
>     , valid a1, valid a2, valid a3
>     ]
>   Only4 m a1 a2 a3 a4 -> and
>     [ m == mconcat [ value a1, value a2, value a3, value a4 ]
>     , valid a1, valid a2, valid a3, valid a4
>     ]
> 
> validateNode
>   :: ( Eq m, Valued m a )
>   => (a -> Bool) -> Node m a -> Bool
> validateNode valid x = case x of
>   Node2 m a1 a2 -> and
>     [ m == mconcat [ value a1, value a2 ]
>     , valid a1, valid a2
>     ]
>   Node3 m a1 a2 a3 -> and
>     [ m == mconcat [ value a1, value a2, value a3 ]
>     , valid a1, valid a2, valid a3
>     ]

> toListDebugFT
>  :: ( Valued m a )
>  => FingerTree m a -> [(a,m)]
> toListDebugFT xs = foo mempty xs
>  where
>    foo e z = case uncons z of
>      Nothing -> []
>      Just (a, ys) ->
>        let u = e <> value a
>        in (a, u) : foo u ys

> showInternalFT
>   :: ( Valued m a, Show m, Show a )
>   => FingerTree m a -> String
> showInternalFT = showInternalFT' show

> showInternalFT'
>   :: forall m a
>    . ( Valued m a, Show m )
>   => (a -> String)
>   -> FingerTree m a -> String
> showInternalFT' s x =
>   let
>     p cs = if elem ' ' cs
>       then concat ["(",cs,")"]
>       else cs
> 
>     showNode
>       :: (a -> String) -> Node m a -> String
>     showNode s x = case x of
>       Node2 m a1 a2 -> unwords
>         [ "Node2", p $ show m, p $ s a1, p $ s a2 ]
>       Node3 m a1 a2 a3 -> unwords
>         [ "Node3", p $ show m, p $ s a1, p $ s a2, p $ s a3 ]
> 
>     showSome
>       :: (a -> String) -> Some m a -> String
>     showSome s x = case x of
>       Only1 m a1 -> unwords
>         [ "Only1", p $ show m, p $ s a1 ]
>       Only2 m a1 a2 -> unwords
>         [ "Only2", p $ show m, p $ s a1, p $ s a2 ]
>       Only3 m a1 a2 a3 -> unwords
>         [ "Only3", p $ show m, p $ s a1, p $ s a2, p $ s a3 ]
>       Only4 m a1 a2 a3 a4 -> unwords
>         [ "Only4", p $ show m, p $ s a1, p $ s a2, p $ s a3, p $ s a4 ]
>   in
>     case x of
>       Stump -> "Stump"
>       Leaf m a -> unwords
>         [ "Leaf", p $ show m, s a ]
>       Branch m as x bs -> unwords
>         [ "Branch"
>         , p $ show m
>         , p $ showSome s as
>         , p $ showInternalFT' (showNode s) x
>         , p $ showSome s bs
>         ]



> instance
>   ( Arb a, Valued m a
>   ) => Arb (FingerTree m a)
>   where
>     arb = fromListFT <$> arb
> 
> instance
>   ( Prune a, Valued m a
>   ) => Prune (FingerTree m a)
>   where
>     prune =
>       map fromListFT . prune . toList
> 
> instance
>   ( CoArb a, Valued m a
>   ) => CoArb (FingerTree m a)
>   where
>     coarb x = coarb (toList x)
> 
> instance
>   ( MakeTo a, Valued m a
>   ) => MakeTo (FingerTree m a)
>   where
>     makeTo = makeToExtendWith makeTo toList fromListFT
