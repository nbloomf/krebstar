---
title: Finger Trees
---

::: contents
* [Introduction](#introduction): The problem we're solving
* [Internal Types](#internal-types): Building toward polymorphic recursion
* [Finger Trees](#finger-trees): Not as scary as it sounds
* [Queries](#queries): Extracting basic information from a finger tree
* [Class Instances](#class-instances): Code for free
* [Cons and Uncons](#cons-and-uncons): Building and destructuring finger trees
* [Concatenation](#concatenation): Joining finger trees
* [Splitting](#splitting): Efficient search
* [Testing and Debugging](#testing-and-debugging): Validation and test case generation
:::



::: frontmatter

> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE DeriveGeneric #-}
> {-# LANGUAGE InstanceSigs #-}
> {-# LANGUAGE Rank2Types #-}
> 
> module Kreb.Struct.FingerTree (
>     FingerTree()
>   , empty
>   , singleton
> 
>   , isEmpty
>   , isSingleton
>   , readInit
>   , readLast
> 
>   , fmapFT
>   , remeasure
>   , traverseFT
> 
>   , cons
>   , fromList
>   , snoc
>   , reverse
>   , uncons
>   , unsnoc
>   , toAnnotatedList
> 
>   , concatWithList
>   , inflateWith
> 
>   , splitWithContext
>   , split
> 
>   , validate
> ) where
> 
> import Prelude hiding (reverse)
> import qualified Prelude as Prelude (reverse)
> 
> import GHC.Generics
> 
> import Data.Monoid
> import Data.Foldable
> import Data.List (unwords)

> import Kreb.Check
> import Kreb.Struct.Valued

:::



Introduction
------------

The most basic requirement of a text editor is surely that it provide some mechanism for modeling and manipulating strings of characters. This mechanism had better be pretty robust and efficient, too, at least for common operations, since it will be doing most of the work during everyday use. Several different data structures have been developed for this purpose, including gap buffers and piece tables, but not all of them translate cleanly to a language like Haskell where mutation is very strictly controlled and evaluation is lazy by default.

Choosing a data structure is all about making tradeoffs. We think about our particular application and what data and operations it needs, then choose a structure that makes those operations efficient. With that in mind, what exactly do we need for our simple text editor? I can think of a few things.

1. Our data structure should essentially model a list of characters; that is, whatever weird branching or in-place mutating business is happening behind the scenes, the API is more or less that of lists.
2. Typically when editing text we've got a distinguished position called the _cursor_ where our editing actions (insert, delete) have immediate effect. It is vital that interacting with and moving the cursor around be blazing fast, since that's where most of the work happens.
3. Another very common action when editing text is _search_. This takes a few forms -- jumping to a particular line and column position, or searching for a literal substring, or looking for matches to a given regular expression.

There are others -- fast syntax highlighting is nice, for instance -- but these are the absolute essentials.

Lucky for us, there's a powerful data structure that gives us all three, based on _finger trees_. These were introduced in the present form in the paper [_Finger trees: a simple general-purpose data structure_](http://www.staff.city.ac.uk/~ross/papers/FingerTree.html) by Hinze and Paterson. The funny name refers to the fact that trees of this type have _fingers_ -- distinguished locations where read and write access is cheap.^[Finger trees are themselves an instance of a more general and very powerful pattern for designing data structures based on _numerical representations_. Okasaki's <a href="https://www.cs.cmu.edu/~rwh/theses/okasaki.pdf">thesis</a> on _Purely Functional Data Structures_ (also available in book form) is the classic reference on this topic.]

In this module we'll develop an API for working with finger trees by following the Hinze and Paterson paper pretty closely. There are existing implementations of this that we could use instead, but finger trees are quite elegant and seeing how they work by rolling our own is a worthwhile exercise.

The secret sauce behind finger trees is that they are not a single data structure but a whole family of structures, parameterized by a monoid, which permit an efficient _search_ algorithm. Different choices for the monoid yield specific structures with different behavior, but we can implement them all at once. This is a good case study on the principle that _throwing away detail_ can lead to clean and general code.



Internal Types
--------------

Recursive data types are the bread and butter of functional programming -- after a little practice we whip up new recursive types without even thinking about it. Finger trees are also recursively defined, but exhibit a phenomenon called _polymorphic recursion_ where at each level the "inner" and "outer" structures have different types. To get there we first need to define some auxiliary types.

Finger trees have nested data of two forms: the _fingers_, which provide amortized constant complexity access to a small number of values on either "end" of the tree, and the _branches_, which carry more deeply nested values. These are implemented as the types `Some` and `Node`, which are used only inside this module.

First for the fingers. The number of values on each end ranges from one to four. Why four? The short answer is that this is the number required to make the complexity proofs work out; it provides just the right balance between _immediate access_ at the fingers and _deep nesting_ in the branches. The Hinze and Paterson paper has the details. We represent the fingers with the following `Some` type.^[Some is also a sum.]

> data Some m a
>   = Only1 m a
>   | Only2 m a a
>   | Only3 m a a a
>   | Only4 m a a a a
>   deriving (Eq, Show)

Note the `m` parameter; this is the cached monoidal value of the internal data. Specifically the value of a `Some` is the product of the values of its internal data.

> instance
>   ( Monoid m
>   ) => Valued m (Some m a)
>   where
>     value w = case w of
>       Only1 m _ -> m
>       Only2 m _ _ -> m
>       Only3 m _ _ _ -> m
>       Only4 m _ _ _ _ -> m

To maintain the invariant that our cached value is accurate, it's important that we only create `Some` values using the following _smart constructors_. The `Valued` class here is essential; this is where the `Monoid` instance on `m` is enforced.

> only1
>   :: ( Valued m a )
>   => a -> Some m a
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
> only3
>   :: ( Valued m a )
>   => a -> a -> a -> Some m a
> only3 a1 a2 a3 =
>   let
>     m = mconcat
>       [ value a1, value a2, value a3 ]
>   in Only3 m a1 a2 a3 
> 
> only4
>   :: ( Valued m a )
>   => a -> a -> a -> a -> Some m a
> only4 a1 a2 a3 a4 =
>   let
>     m = mconcat
>       [ value a1, value a2, value a3, value a4 ]
>   in Only4 m a1 a2 a3 a4 

These constructors are not exposed outside this module since they are only used in the internal representation. For instance, we use them to define something like `fmap` for `Some` -- although we can't use the `Functor` typeclass for this due to the `Valued` constraint. (A common theme for types based on finger trees.)

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

We can test our intuition for how `fmapSome` should behave with an example.

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x, y :: Some Count Int
> --   x = only3 1 2 3
> --   y = only3 2 4 6
> -- in y == fmapSome (*2) x
> -- :}
> -- True

:::

> traverseSome
>   :: ( Valued v1 a1, Valued v2 a2, Applicative f )
>   => (a1 -> f a2) -> Some v1 a1 -> f (Some v2 a2)
> traverseSome f w = case w of
>   Only1 _ a1 ->
>     only1 <$> f a1
>   Only2 _ a1 a2 ->
>     only2 <$> f a1 <*> f a2
>   Only3 _ a1 a2 a3 ->
>     only3 <$> f a1 <*> f a2 <*> f a3
>   Only4 _ a1 a2 a3 a4 ->
>     only4 <$> f a1 <*> f a2 <*> f a3 <*> f a4

Next we have a `Foldable` instance (this is where we use `InstanceSigs` for clarity). Here we do have a bona fide class instance because we don't need to use the smart constructors.

> instance Foldable (Some m) where
>   toList
>     :: Some m a -> [a]
>   toList w = case w of
>     Only1 _ a1 -> [a1]
>     Only2 _ a1 a2 -> [a1, a2]
>     Only3 _ a1 a2 a3 -> [a1, a2, a3]
>     Only4 _ a1 a2 a3 a4 -> [a1, a2, a3, a4]
> 
>   foldr
>     :: (a -> b -> b) -> b -> Some m a -> b
>   foldr f b w = case w of
>     Only1 _ a1 ->
>       f a1 b
>     Only2 _ a1 a2 ->
>       f a1 (f a2 b)
>     Only3 _ a1 a2 a3 ->
>       f a1 (f a2 (f a3 b))
>     Only4 _ a1 a2 a3 a4 ->
>       f a1 (f a2 (f a3 (f a4 b)))
> 
>   foldl
>     :: (b -> a -> b) -> b -> Some m a -> b
>   foldl f b w = case w of
>     Only1 _ a1 ->
>       f b a1
>     Only2 _ a1 a2 ->
>       f (f b a1) a2
>     Only3 _ a1 a2 a3 ->
>       f (f (f b a1) a2) a3
>     Only4 _ a1 a2 a3 a4 ->
>       f (f (f (f b a1) a2) a3) a4

This instance is mainly used as a helper for defining the instance for `FingerTree`. Because `Some` is not exposed outside of this module we won't rigorously check that this instance is lawful (though we will do this indirectly when we test the corresponding instance for finger trees). We can however manually check some small examples.

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x :: Some Count Char
> --   x = only3 'a' 'b' 'c'
> -- in (foldr (:) [] x, foldl (flip (:)) [] x)
> -- :}
> -- ("abc","cba")

:::

Next we have a nested tree type. Finger trees are basically rearranged 2-3 trees, and the nested `Node` type represents internal branching nodes of this sort.

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

> traverseNode
>   :: ( Valued v1 a1, Valued v2 a2, Applicative f )
>   => (a1 -> f a2) -> Node v1 a1 -> f (Node v2 a2)
> traverseNode f w = case w of
>   Node2 _ u1 u2 ->
>     node2 <$> f u1 <*> f u2
>   Node3 _ u1 u2 u3 ->
>     node3 <$> f u1 <*> f u2 <*> f u3

And we need a `Foldable` instance:

> instance Foldable (Node m) where
>   toList
>     :: Node m a -> [a]
>   toList w = case w of
>     Node2 _ a1 a2 -> [a1, a2]
>     Node3 _ a1 a2 a3 -> [a1, a2, a3]
> 
>   foldr
>     :: (a -> b -> b) -> b -> Node m a -> b
>   foldr f b w = case w of
>     Node2 _ a1 a2 ->
>       f a1 (f a2 b)
>     Node3 _ a1 a2 a3 ->
>       f a1 (f a2 (f a3 b))
> 
>   foldl
>     :: (b -> a -> b) -> b -> Node m a -> b
>   foldl f b w = case w of
>     Node2 _ a1 a2 ->
>       f (f b a1) a2
>     Node3 _ a1 a2 a3 ->
>       f (f (f b a1) a2) a3

Finally, note that `Node` can be thought of as a strict subset of `Some`. We use a helper function, `toSome`, to make this formal.

> toSome
>   :: ( Valued m a )
>   => Node m a -> Some m a
> toSome w = case w of
>   Node2 _ a1 a2 -> only2 a1 a2
>   Node3 _ a1 a2 a3 -> only3 a1 a2 a3



Finger Trees
------------

Now for the big show. A finger tree is either empty, or consists of a single node (with its cached value), or has some left and right fingers with a nested finger tree in the middle (with the cached product of their values).

> data FingerTree m a
>   = Stump
>   | Leaf m a
>   | Branch m
>       (Some m a)                -- left fingers
>       (FingerTree m (Node m a)) -- nested tree
>       (Some m a)                -- right fingers

Note that the second type parameter of the outer appearance of `FingerTree` is simply `a`, while on the inner appearance it is `Node m a`. This is polymorphic recursion, and it took me a while to wrap my head around what it means.

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
>   => a -> FingerTree m a
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

These constructors are useful inside this module, where we know (and need to know) the internal structure of a finger tree. But outside this module it will be more useful to expose constructors with less concrete names.

> empty
>   :: FingerTree m a
> empty = stump
> 
> singleton
>   :: ( Valued m a )
>   => a -> FingerTree m a
> singleton = leaf

Also, just as every `Node` can be converted into a `Some`, every `Some` can be converted to a `FingerTree`. This conversion will come in handy later so we define it here, but this code is not exposed outside of this module.

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
> maybeSomeToFingerTree
>   :: ( Valued m a )
>   => Maybe (Some m a) -> FingerTree m a
> maybeSomeToFingerTree w = case w of
>   Nothing -> stump
>   Just z -> someToFingerTree z



Queries
-------

Next we define some simple structural queries on finger trees. We can detect whether the tree is empty or a singleton.

> isEmpty
>   :: FingerTree m a -> Bool
> isEmpty x = case x of
>   Stump -> True
>   _     -> False
> 
> isSingleton
>   :: ( Valued m a )
>   => FingerTree m a -> Bool
> isSingleton x = case x of
>   Leaf _ _ -> True
>   _        -> False

Next we define very basic queries for reading the first or last item in a nonempty finger tree. Recall that efficient access to these items is the point of the finger. (These are very basic; we'll define much more powerful _destructors_ which generalize this behavior in a bit.)

> readInit
>   :: FingerTree m a -> Maybe a
> readInit w = case w of
>   Stump -> Nothing
>   Leaf _ a -> Just a
>   Branch _ as _ _ -> case as of
>     Only1 _ a -> Just a
>     Only2 _ a _ -> Just a
>     Only3 _ a _ _ -> Just a
>     Only4 _ a _ _ _ -> Just a
> 
> readLast
>   :: FingerTree m a -> Maybe a
> readLast w = case w of
>   Stump -> Nothing
>   Leaf _ a -> Just a
>   Branch _ _ _ as -> case as of
>     Only1 _ a -> Just a
>     Only2 _ _ a -> Just a
>     Only3 _ _ _ a -> Just a
>     Only4 _ _ _ _ a -> Just a



Class Instances
---------------

It isn't too surprising that we have a `Foldable` instance for finger trees.

> instance Foldable (FingerTree m) where
>   toList
>     :: FingerTree m a -> [a]
>   toList w = case w of
>     Stump -> []
>     Leaf _ a -> [a]
>     Branch _ as1 as2 as3 -> concat
>       [ toList as1, concatMap toList as2, toList as3 ]
> 
>   foldr
>     :: (a -> b -> b) -> b -> FingerTree m a -> b
>   foldr f b w = case w of
>     Stump -> b
>     Leaf _ a -> f a b
>     Branch _ as1 as2 as3 ->
>       foldr f
>         (foldr (flip (foldr f)) (foldr f b as3) as2) as1
> 
>   foldl
>     :: (b -> a -> b) -> b -> FingerTree m a -> b
>   foldl f b w = case w of
>     Stump -> b
>     Leaf _ a -> f b a
>     Branch _ as1 as2 as3 ->
>       foldl f
>         (foldl (foldl f) (foldl f b as1) as2) as3

Note that we didn't derive the `Eq` instance for `FingerTree`. This is because in some sense the tree structure itself is only incidental, used for maintaining the fingers and caching values. The meat of the structure is the left-to-right traversal of the leaf nodes and fingers, and the derived (structural) equality instance would be too granular. To get around this we'll instead check for equality on `FingerTree`s by converting to lists first, using the `Foldable` instance.

> instance (Eq a) => Eq (FingerTree m a) where
>   a == b = (toList a) == (toList b)

We'd also like for finger trees to be a functor. Unfortunately this can't be expressed in the Haskell type system (as far as I know) due to the `Valued` class constraint. We can however define a bespoke `fmap`-like function on finger trees.

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

> remeasure
>  :: forall m1 m2 a
>   . ( Valued m1 a, Valued m2 a )
>  => FingerTree m1 a -> FingerTree m2 a
> remeasure = fmapFT id

> traverseFT
>   :: ( Valued v1 a1, Valued v2 a2, Applicative f )
>   => (a1 -> f a2) -> FingerTree v1 a1 -> f (FingerTree v2 a2)
> traverseFT f w = case w of
>   Stump -> pure stump
>   Leaf _ a -> leaf <$> f a
>   Branch _ as bs cs ->
>     branch
>       <$> traverseSome f as
>       <*> traverseFT (traverseNode f) bs
>       <*> traverseSome f cs



Cons and Uncons
---------------

In this section we'll implement four of the most important functions on finger trees, which efficiently build and decompose finger trees by adding or removing items from one of the ends.

The traditional name for appending an item to a list is _cons_, after the Lisp function for this operation. Given a finger tree, if the left end does not have a full complement of fingers then `cons` is a constant time operation. If it does, then we take some of the fingers and recursively `cons` them as a node to the inner tree. This is the first place where the magic numbers 4 and 3, for the numbers of fingers and children per node, respectively, become important.

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

> fromList
>  :: ( Valued m a )
>  => [a] -> FingerTree m a
> fromList = foldr cons stump

At this point we can also test some interesting examples.

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x :: FingerTree Count Char
> --   x = fromList ['a','b','c']
> -- in (readInit x, readLast x)
> -- :}
> -- (Just 'a',Just 'c')
> --
> -- >>> :{
> -- let
> --   x, y :: FingerTree Count Char
> --   x = fromList ['b','c','d']
> --   y = fromList ['a','b','c','d']
> -- in y == cons 'a' x
> -- :}
> -- True

:::

Before moving on, with `fromList` in hand we can also give a convenient `Show` instance. Note that the derived instance would include a lot of superfluous information about the internal structure of the tree.

> instance (Show a) => Show (FingerTree m a) where
>   show a = "fromList " ++ show (toList a)

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

> reverse
>   :: ( Valued m a )
>   => FingerTree m a -> FingerTree m a
> reverse = foldr snoc mempty

For example:

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x, y :: FingerTree Count Char
> --   x = fromList ['a','b','c']
> --   y = fromList ['c','b','a']
> -- in y == reverse x
> -- :}
> -- True

:::

`cons` also has an inverse, called `uncons`. (The inverse here is not literal, but can be made so if instead of cons we consider the coproduct of cons with the constant `stump` function, but for our purposes here that's splitting hairs.)

Like `cons`, `uncons` is very fast if the left side of the tree has fingers to spare. But if not, we have to do a kind of recursive borrowing.^[Think "borrow" as in the usual algorithm for subtracting natural numbers.]

> uncons
>   :: ( Valued m a )
>   => FingerTree m a
>   -> Maybe (a, FingerTree m a)
> uncons w = case w of
>   Stump -> Nothing
>   Leaf _ a -> Just (a, stump)
>   Branch _ as1 as2 as3 ->
>     let (a, as) = uncons' as1
>     in Just (a, borrowL as as2 as3)
>   where
>     uncons'
>       :: ( Valued m a )
>       => Some m a
>       -> (a, Maybe (Some m a))
>     uncons' w = case w of
>       Only1 _ a1 -> (a1, Nothing)
>       Only2 _ a1 a2 -> (a1, Just (only1 a2))
>       Only3 _ a1 a2 a3 -> (a1, Just (only2 a2 a3))
>       Only4 _ a1 a2 a3 a4 -> (a1, Just (only3 a2 a3 a4))
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

With `uncons`, we can define an alternate version of `toList` (from the `Foldable` class) that also returns the accumulated value annotations. This will be handy in a few places.

> toAnnotatedList
>  :: ( Valued m a )
>  => FingerTree m a -> [(a,m)]
> toAnnotatedList xs = foo mempty xs
>  where
>    foo e z = case uncons z of
>      Nothing -> []
>      Just (a, ys) ->
>        let u = e <> value a
>        in (a, u) : foo u ys

And there's an analogous partner for `snoc`.

> unsnoc
>   :: ( Valued m a )
>   => FingerTree m a
>   -> Maybe (a, FingerTree m a)
> unsnoc w = case w of
>   Stump -> Nothing
>   Leaf _ a -> Just (a, stump)
>   Branch _ as1 as2 as3 ->
>     let (a, as) = unsnoc' as3
>     in Just (a, borrowR as1 as2 as)
>   where
>     unsnoc'
>       :: ( Valued m a )
>       => Some m a
>       -> (a, Maybe (Some m a))
>     unsnoc' w = case w of
>       Only1 _ a1 -> (a1, Nothing)
>       Only2 _ a1 a2 -> (a2, Just (only1 a1))
>       Only3 _ a1 a2 a3 -> (a3, Just (only2 a1 a2))
>       Only4 _ a1 a2 a3 a4 -> (a4, Just (only3 a1 a2 a3))
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

And some examples:

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x, y, z :: FingerTree Count Char
> --   x = fromList ['a','b','c']
> --   y = fromList ['b','c']
> --   z = fromList ['a','b']
> -- in
> --   ( Just ('a', y) == uncons x
> --   , Just ('c', z) == unsnoc x )
> -- :}
> -- (True,True)

:::



Concatenation
-------------

Despite the name, finger trees are really list-like, and on such structures it's typically handy to be able to append one list to another. The fancy word for this is _concatenation_. Before defining concatenation proper, we start with a generalized version that takes an additional list of elements to insert between the concatenands.

> concatWithList
>  :: ( Valued m a )
>  => FingerTree m a
>  -> [a]
>  -> FingerTree m a
>  -> FingerTree m a
> concatWithList u as v = case u of
>  Stump -> foldr cons v as
>  Leaf _ a -> cons a (foldr cons v as)
>  Branch _ us1 us2 us3 -> case v of
>    Stump -> foldl (flip snoc) u as
>    Leaf _ a -> snoc a (foldl (flip snoc) u as)
>    Branch _ vs1 vs2 vs3 ->
>      let ns = (toList us3) ++ as ++ (toList vs1)
>      in branch us1 (concatWithList us2 (toNodes ns) vs2) vs3
>   where
>     toNodes
>       :: ( Valued m a )
>       => [a] -> [Node m a]
>     toNodes w = case w of
>       [] -> []
>       [a1, a2] -> [node2 a1 a2]
>       [a1, a2, a3] -> [node3 a1 a2 a3]
>       [a1, a2, a3, a4] -> [node2 a1 a2, node2 a3 a4]
>       a1:a2:a3:a4:as -> (node3 a1 a2 a3) : toNodes (a4:as)
>       _ -> error "toNodes: panic"

We can test this with an example.

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x1, x2, y :: FingerTree Count Char
> --   x1 = fromList ['a','b']
> --   x2 = fromList ['d','e']
> --   y = fromList ['a','b','c','d','e']
> -- in y == concatWithList x1 ['c'] x2
> -- :}
> -- True

:::

Now the real `cat` is a specialization of `concatWithList`, and along with `empty` makes the type of finger trees into a monoid.

> instance
>  ( Valued m a
>  ) => Semigroup (FingerTree m a)
>  where
>    u <> v = concatWithList u [] v
> 
> instance
>  ( Valued m a
>  ) => Monoid (FingerTree m a)
>  where
>    mempty = empty

We end with a kind of generalized `join` on finger trees. The intuition behind `inflateWith` is that each entry in an input finger tree is expanded to a subsequence.

> inflateWith
>   :: forall m1 m2 a b
>    . ( Valued m1 a, Valued m2 b )
>   => (a -> [b]) -> FingerTree m1 a -> FingerTree m2 b
> inflateWith f w = inflate w
>   where
>     inflate :: FingerTree m1 a -> FingerTree m2 b
>     inflate w = case uncons w of
>       Nothing -> mempty
>       Just (a, as) -> (fromList (f a)) <> inflate as



Splitting
---------

The killer operation on finger trees, and the reason for caching the `m` value at each node, is efficient _splitting_. This operation takes a predicate `p` on `m` and a finger tree `w` and attempts to break it into three pieces, `as`, `x`, and `bs`. We can think of `x` as a _search result_ and of `as` and `bs` as the prefix and suffix of the search. Moreover, the search result must satisfy three _splitting properties_:

1. That `w == as <> singleton x <> bs` -- so that splitting is a bona fide cat-factorization;
2. That `p (value as)` is false -- so the portion of the list _up to_ the search result doesn't satisfy `p`; and
3. That `p (value as <> value x)` is true -- so the portion of the list _up to and including_ the search result does satisfy `p`.

The internal structure of finger trees allows this to be done efficiently. In general there may be many possible places to break the tree, but with a judicious choice of `m` and the predicate we can perform some complex operations quickly.

It's natural to wonder why this is a useful thing to do. Well, for a judiciously chosen monoid `m` and predicate `p`, splitting can do some interesting things. As a simple example, with the `Count` monoid and a predicate like $p(k) = (k \geq N)$ we can break the finger tree at a specific index. Later on we'll use it to break a text buffer at a specific line and column position or screen coordinate.

First we need a version of this operation on the fingers. This is pretty tedious because we're essentially using brute force to make sure the output satisfies the splitting properties. (`splitSome` is only used inside this module.) Tedious though it is, note that `splitSome` has constant complexity.

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

Next we define a version of split that prepends a given monoid value on the left before searching. This is a stepping stone toward our main splitting function, but is also useful in its own right; the extra `m` parameter acts like a kind of value context.

We use the cached value at each node to decide whether to continue the search down a given branch or ignore it. In the complexity analysis of this algorithm (available in the Hinze and Paterson paper) this, with the branching factor of the tree, is where the efficiency comes from.

Note also that the input to `splitWithContext` needs to satisfy a precondition: the value of the entire finger tree should be true. This is (I think) what guarantees that the search will succeed.

> splitWithContext
>  :: ( Valued m a )
>  => m -> (m -> Bool) -> FingerTree m a
>  -> Maybe (FingerTree m a, a, FingerTree m a)
> splitWithContext i p w =
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
>                 bs1 = maybeSomeToFingerTree ds1
>                 bs3 = borrowL ds3 as2 as3
>               in Just (bs1, x, bs3)
>           else
>             let vas2 = vas1 <> (value as2) in
>             if p vas2
>               -- there's a match in the spine
>               then case splitWithContext vas1 p as2 of
>                 Nothing -> error "split: panic (2)"
>                 Just (cs1, xs, cs3) ->
>                   let vs = vas1 <> (value cs1) in
>                   case splitSome p vs (toSome xs) of
>                     Nothing -> error "split: panic (3)"
>                     Just (ds1, x, ds3) ->
>                       let
>                         bs1 = borrowR as1 cs1 ds1
>                         bs3 = borrowL ds3 cs3 as3
>                       in Just (bs1, x, bs3)
>               else
>                 let vas3 = vas2 <> (value as3) in
>                 if p vas3
>                   -- there's a match in the right fingers
>                   then case (splitSome p vas2 as3) of
>                     Nothing -> error "split: panic (4)"
>                     Just (ds1, x, ds3) ->
>                       let
>                         bs1 = borrowR as1 as2 ds1
>                         bs3 = maybeSomeToFingerTree ds3
>                       in Just (bs1, x, bs3)
>                   else Nothing -- precondition violated

Now the general `split` function just specializes `splitWithContext` to the identity.

> split
>  :: ( Valued m a )
>  => (m -> Bool)
>  -> FingerTree m a
>  -> Maybe (FingerTree m a, a, FingerTree m a)
> split p w = splitWithContext mempty p w

Now's a good time for some examples.

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x :: FingerTree Count Char
> --   x = fromList ['a','b','c','d','e']
> --   --
> --   us, vs :: FingerTree Count Char
> --   us = fromList ['a','b','c']
> --   vs = fromList ['e']
> --   --
> --   p :: Count -> Bool
> --   p (Count k) = k > 3
> -- in Just (us, 'd', vs) == split p x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   x :: FingerTree Count Char
> --   x = fromList ['a','b','c','d']
> --   --
> --   us, vs :: FingerTree Count Char
> --   us = fromList ['a']
> --   vs = fromList ['c','d']
> --   --
> --   p :: Count -> Bool
> --   p (Count k) = k >= 2
> -- in Just (us, 'b', vs) == split p x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   x :: FingerTree Count Char
> --   x = fromList ['a','b','c','d']
> --   --
> --   p :: Count -> Bool
> --   p (Count k) = k >= 7
> -- in split p x
> -- :}
> -- Nothing

:::



Testing and Debugging
---------------------

Finally we define some utilities to help with testing. First, to interoperate with our testing framework, we need `FingerTree` to be an instance of some type classes.

> instance
>   ( Arb a, Valued m a
>   ) => Arb (FingerTree m a)
>   where
>     arb = fromList <$> arb
> 
> instance
>   ( Prune a, Valued m a
>   ) => Prune (FingerTree m a)
>   where
>     prune =
>       map fromList . prune . toList
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
>     makeTo = makeToExtendWith
>       makeTo toList fromList

Next, recall that our finger tree type has an internal invariant that needs to be maintained in order for the complexity and correctness proofs of our algorithms to hold. Namely, the cached monoidal value at each node must be the product of the monoidal values of the node's contents. We introduce a function (only used for testing) to check this. Note how polymorphic recursion makes this a little tricky.

> validate
>   :: ( Eq m, Valued m a )
>   => FingerTree m a -> Bool
> validate = validateNested (const True)
>   where
>     validateNested
>       :: ( Eq m, Valued m a )
>       => (a -> Bool) -> FingerTree m a -> Bool
>     validateNested valid xs = case xs of
>       Stump ->
>         True
>       Leaf m a ->
>         m == value a
>       Branch m as x bs -> and
>         [ validateSome valid as
>         , validateNested (validateNode valid) x
>         , validateSome valid bs
>         , m == mconcat [ value as, value x, value bs ]
>         ]
> 
>     validateSome
>       :: ( Eq m, Valued m a )
>       => (a -> Bool) -> Some m a -> Bool
>     validateSome valid x = case x of
>       Only1 m a1 -> and
>         [ m == value a1
>         , valid a1
>         ]
>       Only2 m a1 a2 -> and
>         [ m == mconcat [ value a1, value a2 ]
>         , valid a1, valid a2
>         ]
>       Only3 m a1 a2 a3 -> and
>         [ m == mconcat [ value a1, value a2, value a3 ]
>         , valid a1, valid a2, valid a3
>         ]
>       Only4 m a1 a2 a3 a4 -> and
>         [ m == mconcat [ value a1, value a2, value a3, value a4 ]
>         , valid a1, valid a2, valid a3, valid a4
>         ]
> 
>     validateNode
>       :: ( Eq m, Valued m a )
>       => (a -> Bool) -> Node m a -> Bool
>     validateNode valid x = case x of
>       Node2 m a1 a2 -> and
>         [ m == mconcat [ value a1, value a2 ]
>         , valid a1, valid a2
>         ]
>       Node3 m a1 a2 a3 -> and
>         [ m == mconcat [ value a1, value a2, value a3 ]
>         , valid a1, valid a2, valid a3
>         ]



::: epigraph

<blockquote>
I can whistle with my fingers, especially if I have a whistle.

<footer>Mitch Hedberg, <cite>Live at the Congress Theater in Chicago, 2004</cite></footer>
</blockquote>

:::
