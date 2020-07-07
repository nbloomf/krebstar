---
title: Finger Trees
---

::: contents
* [Introduction](#introduction): The problem we're solving
* [Internal Types](#internal-types): Building toward polymorphic recursion
* [Finger Trees](#finger-trees): Not as scary as it sounds
* [Cons and Uncons](#cons-and-uncons): Building and destructuring finger trees
* [Concatenation](#concatenation): They were lists the whole time
* [Splitting](#splitting): Efficient search
* [Testing and Debugging](#testing-and-debugging): Validation and test case generation
:::

::: frontmatter

> {-# LANGUAGE StandaloneDeriving, OverloadedStrings #-}

> module Kreb.Struct.Data.FingerTree (
>     FingerTree(..)
>   , NonEmptyFingerTree()
>   , toAnnotatedList
> 
>   , inflateWith
> 
>   , SplitResult(..)
>   , Split(splitWithContextL, splitWithContextR)
>   , splitL
>   , splitR
> 
>   , removeSplitsL
>   , removeSplitsR
> 
>   , validateFingerTree
>   , validateNonEmptyFingerTree
> ) where

> import Prelude hiding (reverse)
> import Data.Foldable
> import Data.Maybe (catMaybes)

> import           Kreb.Control
> import qualified Kreb.Format as Fmt
> import           Kreb.Format hiding (Empty)
> import           Kreb.Prop
> import           Kreb.Category

> import           Kreb.Struct.Class

:::



Introduction
------------

The most basic requirement of a text editor is surely that it provide some mechanism for modeling and manipulating strings of characters. This mechanism had better be pretty robust and efficient, too, at least for common operations, since it will be doing most of the work during everyday use. Several different data structures have been developed for this purpose; two important examples are gap buffers and piece tables. But not all data structures translate cleanly to a language like Haskell where mutation is very strictly controlled and evaluation is lazy by default.

Choosing a data structure is all about making tradeoffs. We think about our particular application and what data and operations it needs, then choose a structure that makes those operations efficient. With that in mind, what exactly do we need for our simple text editor? I can think of a few things.

1. Our data structure should essentially model a list of characters; that is, whatever weird branching or in-place mutating business is happening behind the scenes, the API is more or less that of lists.
2. Typically when editing text we've got a distinguished position called the _cursor_ where our editing actions (insert, delete) have immediate effect. It is vital that interacting with and moving the cursor around be blazing fast, since that's where most of the work happens.
3. Another very common action when editing text is _search_. This takes a few forms -- jumping to a particular line and column position, or searching for a literal substring, or looking for matches to a given regular expression. We should be able to handle this too.

There are others -- fast syntax highlighting is nice, for instance -- but these are the absolute essentials.

Lucky for us there's a powerful data structure that gives us all three, based on _finger trees_. These were introduced in the present form in the paper [_Finger trees: a simple general-purpose data structure_](http://www.staff.city.ac.uk/~ross/papers/FingerTree.html) by Hinze and Paterson. The funny name refers to the fact that trees of this type have _fingers_ -- distinguished locations where read and write access is cheap.^[Finger trees are themselves an instance of a more general and very powerful pattern for designing data structures based on _numerical representations_. Okasaki's <a href="https://www.cs.cmu.edu/~rwh/theses/okasaki.pdf">thesis</a> on _Purely Functional Data Structures_ (also available in book form) is the classic reference on this topic.]

In this module we'll develop an API for working with finger trees by following the Hinze and Paterson paper pretty closely. There are existing implementations of this that we could use instead, but finger trees are quite elegant and seeing how they work by rolling our own is a worthwhile exercise.

The secret sauce behind finger trees is that they are not a single data structure but a whole family of structures, parameterized by a _monoid_, which permits an efficient and generic _search_ algorithm. Different choices for the monoid yield specific structures with different behavior, but we can implement them all at once. This is a good case study on the principle that _throwing away detail_ can lead to clean and general code.



Internal Types
--------------

Recursive data types are the bread and butter of functional programming -- after a little practice we whip up new recursive types without even thinking about it. Finger trees are also recursively defined, but exhibit a phenomenon called _polymorphic recursion_ where at each level the "inner" and "outer" parameters have different types. To get there we first need to define some auxiliary types.

Finger trees have nested data of two forms: the _fingers_, which provide amortized constant complexity access to a small number of values on either "end" of the tree, and the _branches_, which carry more deeply nested values. These are implemented here as the types `Some` and `Node`, which are used only inside this module.

First for the fingers. The number of values on each end ranges from one to four. Why four? The short answer is that this is the number required to make the amortized complexity proofs work out; it provides just the right balance between _immediate access_ at the fingers and _deep nesting_ in the branches. The Hinze and Paterson paper has the details. We represent the fingers with the following `Some` type.^[Some is also a sum.]

> data Some a where
>   Only1 :: (Valued a) => Value a -> a                -> Some a
>   Only2 :: (Valued a) => Value a -> a -> a           -> Some a
>   Only3 :: (Valued a) => Value a -> a -> a -> a      -> Some a
>   Only4 :: (Valued a) => Value a -> a -> a -> a -> a -> Some a
> 
> deriving instance (Eq a) => Eq (Some a)

Note the `Valued` constraint on the item type `a`. This class is inhabited by types equipped with a special monoidal _valuation_ function, `value`. Each `Some` constructor takes a value of type `Value a`, which must be the product of the values of the `a`s in the constructor. Maintaining this invariant is crucial to the correctness of our code; unfortunately we can't encode it in the type so we'll just have to be really careful.

If `a` has a valuation, then `Some a` does as well in the natural way -- assuming we've maintained the invariant we can return the cached valuation to save some effort.

> instance (Valued a) => Valued (Some a) where
>   type Value (Some a) = Value a
> 
>   value :: Some a -> Value (Some a)
>   value w = case w of
>     Only1 v _       -> v
>     Only2 v _ _     -> v
>     Only3 v _ _ _   -> v
>     Only4 v _ _ _ _ -> v

To maintain the invariant that our cached value is accurate, it's important that we only create `Some` values using the following _smart constructors_. This idiom is common for Haskell types with invariants that can't (easily) be embedded in the type.

> only1
>   :: ( Valued a )
>   => a -> Some a
> only1 a1 = Only1 v a1
>   where v = value a1
> 
> only2
>   :: ( Valued a )
>   => a -> a -> Some a
> only2 a1 a2 =
>   let
>     v = mconcat
>       [ value a1, value a2 ]
>   in Only2 v a1 a2 
> 
> only3
>   :: ( Valued a )
>   => a -> a -> a -> Some a
> only3 a1 a2 a3 =
>   let
>     v = mconcat
>       [ value a1, value a2, value a3 ]
>   in Only3 v a1 a2 a3 
> 
> only4
>   :: ( Valued a )
>   => a -> a -> a -> a -> Some a
> only4 a1 a2 a3 a4 =
>   let
>     v = mconcat
>       [ value a1, value a2, value a3, value a4 ]
>   in Only4 v a1 a2 a3 a4

These constructors are not exposed outside this module since they are only used in the internal representation.

Because of the extra constraint on the type parameter, `Some` cannot be a functor -- at least not a functor on the category of all Haskell types. It is however a _constrained_ functor on the (sub)category of types inhabiting the `Valued` class. We have a special class for constrained functors.

> instance FunctorC Valued (->) Valued (->) Some where
>   fmapC
>     :: ( Valued a, Valued b )
>     => (a -> b) -> Some a -> Some b
>   fmapC f x = case x of
>     Only1 _ u1 ->
>       only1 (f u1)
>     Only2 _ u1 u2 ->
>       only2 (f u1) (f u2)
>     Only3 _ u1 u2 u3 ->
>       only3 (f u1) (f u2) (f u3)
>     Only4 _ u1 u2 u3 u4 ->
>       only4 (f u1) (f u2) (f u3) (f u4)

We should test our intuition for how `fmapC` should behave with an example.

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x, y :: Some (Counted Int)
> --   x = only3 1 2 3
> --   y = only3 2 4 6
> -- in y == fmapC @Valued @(->) @Valued @(->) @Some (*2) x
> -- :}
> -- True

:::

Next we have a `Foldable` instance (this is where we use `InstanceSigs` for clarity). Here we do have a bona fide class instance because we don't need to use the smart constructors.

> instance Foldable Some where
>   toList
>     :: Some a -> [a]
>   toList w = case w of
>     Only1 _ a1 -> [a1]
>     Only2 _ a1 a2 -> [a1, a2]
>     Only3 _ a1 a2 a3 -> [a1, a2, a3]
>     Only4 _ a1 a2 a3 a4 -> [a1, a2, a3, a4]
> 
>   foldr
>     :: (a -> b -> b) -> b -> Some a -> b
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
>     :: (b -> a -> b) -> b -> Some a -> b
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
> --   x :: Some (Counted Char)
> --   x = only3 (Counted 'a') (Counted 'b') (Counted 'c')
> -- in
> --   ( fmap unCounted $ foldr (:) [] x
> --   , fmap unCounted $ foldl (flip (:)) [] x
> --   )
> -- :}
> -- ("abc","cba")

:::

`Some` is also a contrained traversable functor:

> instance TraversableC Valued (->) Valued (->) Some where
>   traverseC
>     :: ( Applicative f, Valued a, Valued b )
>     => (a -> f b) -> Some a -> f (Some b)
>   traverseC f w = case w of
>     Only1 _ a1 ->
>       only1 <$> f a1
>     Only2 _ a1 a2 ->
>       only2 <$> f a1 <*> f a2
>     Only3 _ a1 a2 a3 ->
>       only3 <$> f a1 <*> f a2 <*> f a3
>     Only4 _ a1 a2 a3 a4 ->
>       only4 <$> f a1 <*> f a2 <*> f a3 <*> f a4
> 
>   sequenceAC
>     :: ( Applicative f, Valued a, Valued (f a) )
>     => Some (f a) -> f (Some a)
>   sequenceAC w = case w of
>     Only1 _ a1 ->
>       pure only1 <*> a1
>     Only2 _ a1 a2 ->
>       pure only2 <*> a1 <*> a2
>     Only3 _ a1 a2 a3 ->
>       pure only3 <*> a1 <*> a2 <*> a3
>     Only4 _ a1 a2 a3 a4 ->
>       pure only4 <*> a1 <*> a2 <*> a3 <*> a4
> 
>   consumeC
>     :: ( Applicative f, Valued a )
>     => (Some a -> b) -> Some (f a) -> f b
>   consumeC h w = case w of
>     Only1 _ a1 ->
>       fmap h (only1 <$> a1)
>     Only2 _ a1 a2 ->
>       fmap h (only2 <$> a1 <*> a2)
>     Only3 _ a1 a2 a3 ->
>       fmap h (only3 <$> a1 <*> a2 <*> a3)
>     Only4 _ a1 a2 a3 a4 ->
>       fmap h (only4 <$> a1 <*> a2 <*> a3 <*> a4)

Next we have a nested tree type. Finger trees are basically rearranged 2-3 trees, and the nested `Node` type represents internal branching nodes of this sort.

> data Node a where
>   Node2 :: (Valued a) => Value a -> a -> a      -> Node a
>   Node3 :: (Valued a) => Value a -> a -> a -> a -> Node a
> 
> deriving instance (Eq a) => Eq (Node a)

Again we cache the value of the internal data, using smart constructors to maintain the integrity of the cache.

> instance (Valued a) => Valued (Node a) where
>   type Value (Node a) = Value a
> 
>   value :: Node a -> Value (Node a)
>   value w = case w of
>     Node2 v _ _   -> v
>     Node3 v _ _ _ -> v
> 
> node2
>   :: ( Valued a )
>   => a -> a -> Node a
> node2 a1 a2 =
>   let
>     v = mconcat
>       [ value a1, value a2 ]
>   in Node2 v a1 a2
> 
> node3
>   :: ( Valued a )
>   => a -> a -> a -> Node a
> node3 a1 a2 a3 =
>   let
>     v = mconcat
>       [ value a1, value a2, value a3 ]
>   in Node3 v a1 a2 a3

`Node` is again a constrained functor:

> instance FunctorC Valued (->) Valued (->) Node where
>   fmapC
>     :: ( Valued a, Valued b )
>     => (a -> b) -> Node a -> Node b
>   fmapC f x = case x of
>     Node2 _ u1 u2 ->
>       node2 (f u1) (f u2)
>     Node3 _ u1 u2 u3 ->
>       node3 (f u1) (f u2) (f u3)

And `Node` is foldable.

> instance Foldable Node where
>   toList
>     :: Node a -> [a]
>   toList w = case w of
>     Node2 _ a1 a2 -> [a1, a2]
>     Node3 _ a1 a2 a3 -> [a1, a2, a3]
> 
>   foldr
>     :: (a -> b -> b) -> b -> Node a -> b
>   foldr f b w = case w of
>     Node2 _ a1 a2 ->
>       f a1 (f a2 b)
>     Node3 _ a1 a2 a3 ->
>       f a1 (f a2 (f a3 b))
> 
>   foldl
>     :: (b -> a -> b) -> b -> Node a -> b
>   foldl f b w = case w of
>     Node2 _ a1 a2 ->
>       f (f b a1) a2
>     Node3 _ a1 a2 a3 ->
>       f (f (f b a1) a2) a3

And `Node` is a constrained traversable functor.

> instance TraversableC Valued (->) Valued (->) Node where
>   traverseC
>     :: ( Applicative f, Valued a, Valued b )
>     => (a -> f b) -> Node a -> f (Node b)
>   traverseC f w = case w of
>     Node2 _ u1 u2 ->
>       node2 <$> f u1 <*> f u2
>     Node3 _ u1 u2 u3 ->
>       node3 <$> f u1 <*> f u2 <*> f u3
> 
>   sequenceAC
>     :: ( Applicative f, Valued a, Valued (f a) )
>     => Node (f a) -> f (Node a)
>   sequenceAC w = case w of
>     Node2 _ u1 u2 ->
>       pure node2 <*> u1 <*> u2
>     Node3 _ u1 u2 u3 ->
>       pure node3 <*> u1 <*> u2 <*> u3
> 
>   consumeC
>     :: ( Applicative f, Valued a )
>     => (Node a -> b) -> Node (f a) -> f b
>   consumeC h w = case w of
>     Node2 _ a1 a2 ->
>       fmap h (node2 <$> a1 <*> a2)
>     Node3 _ a1 a2 a3 ->
>       fmap h (node3 <$> a1 <*> a2 <*> a3)

Finally, note that `Node` can be thought of as a strict subset of `Some`. We use a helper function, `toSome`, to make this formal.

> toSome
>   :: ( Valued a )
>   => Node a -> Some a
> toSome w = case w of
>   Node2 v a1 a2    -> Only2 v a1 a2
>   Node3 v a1 a2 a3 -> Only3 v a1 a2 a3



Finger Trees
------------

Now for the big show. A finger tree is either empty, or consists of a single node (with its cached value), or has some left and right fingers with a nested finger tree in the middle (with the cached product of their values). We separate the possibly empty and nonempty cases into distinct types; this will allow consumers of this module to enforce a non-emptiness constraint on finger trees at the type level.

> data FingerTree a where
>   Empty
>     :: ( Valued a )
>     => FingerTree a
>   NonEmpty
>     :: ( Valued a )
>     => NonEmptyFingerTree a
>     -> FingerTree a
> 
> deriving instance (Eq a) => Eq (FingerTree a)
> 
> instance (Show a) => Show (FingerTree a) where
>   show x = case x of
>     Empty -> "Empty"
>     NonEmpty w -> "NonEmpty $ " ++ show w
> 
> data NonEmptyFingerTree a where
>   Leaf
>     :: ( Valued a )
>     => Value a
>     -> a
>     -> NonEmptyFingerTree a
>   Branch
>     :: ( Valued a )
>     => Value a
>     -> Some a                -- left fingers
>     -> FingerTree (Node a)   -- nested tree
>     -> Some a                -- right fingers
>     -> NonEmptyFingerTree a

Both versions of finger trees are constrained containers; we will use this to implement a decent chunk of their API using classes.

> instance Container NonEmptyFingerTree where
>   type ElementOf NonEmptyFingerTree = Valued
> 
> instance Container FingerTree where
>   type ElementOf FingerTree = Valued

First, finger trees inherit a valuation from their item type.

> instance (Valued a) => Valued (FingerTree a) where
>   type Value (FingerTree a) = Value a
> 
>   value :: FingerTree a -> Value (FingerTree a)
>   value w = case w of
>     Empty      -> mempty
>     NonEmpty x -> value x
> 
> instance (Valued a) => Valued (NonEmptyFingerTree a) where
>   type Value (NonEmptyFingerTree a) = Value a
> 
>   value :: NonEmptyFingerTree a -> Value (NonEmptyFingerTree a)
>   value w = case w of
>     Leaf v _       -> v
>     Branch v _ _ _ -> v

The nonempty finger trees form a subset of the possibly empty finger trees. We can express this using the `Subset` class.

> instance Subset NonEmptyFingerTree where
>   type SupersetOf NonEmptyFingerTree = FingerTree
> 
>   inject
>     :: ( Valued a )
>     => NonEmptyFingerTree a -> FingerTree a
>   inject = NonEmpty
> 
>   restrict
>     :: ( Valued a )
>     => FingerTree a -> Maybe (NonEmptyFingerTree a)
>   restrict x = case x of
>     Empty -> Nothing
>     NonEmpty w -> Just w

It isn't too surprising that we have a `Foldable` instance for finger trees.

> instance Foldable FingerTree where
>   toList
>     :: FingerTree a -> [a]
>   toList w = case w of
>     Empty      -> []
>     NonEmpty z -> toList z
> 
>   foldr
>     :: (a -> b -> b) -> b -> FingerTree a -> b
>   foldr f b w = case w of
>     Empty      -> b
>     NonEmpty z -> foldr f b z
> 
>   foldl
>     :: (b -> a -> b) -> b -> FingerTree a -> b
>   foldl f b w = case w of
>     Empty      -> b
>     NonEmpty z -> foldl f b z
> 
> instance Foldable NonEmptyFingerTree where
>   toList
>     :: NonEmptyFingerTree a -> [a]
>   toList z = case z of
>     Leaf _ a             -> [a]
>     Branch _ as1 as2 as3 -> concat
>       [ toList as1, concatMap toList as2, toList as3 ]
> 
>   foldr
>     :: (a -> b -> b) -> b -> NonEmptyFingerTree a -> b
>   foldr f b z = case z of
>     Leaf _ a -> f a b
>     Branch _ as1 as2 as3 ->
>       foldr f
>         (foldr (flip (foldr f)) (foldr f b as3) as2) as1
> 
>   foldl
>     :: (b -> a -> b) -> b -> NonEmptyFingerTree a -> b
>   foldl f b z = case z of
>     Leaf _ a -> f b a
>     Branch _ as1 as2 as3 ->
>       foldl f
>         (foldl (foldl f) (foldl f b as1) as2) as3

Note that we didn't derive the `Eq` instance for nonempty finger trees. This is because in some sense the tree structure itself is only incidental, used for maintaining the fingers and caching values. The meat of the structure is the left-to-right traversal of the leaf nodes and fingers, and the derived (structural) equality instance would be too granular. To get around this we'll instead check for equality on `FingerTree`s by converting to lists first, using the `Foldable` instance.

> instance (Eq a, Valued a) => Eq (NonEmptyFingerTree a) where
>   u == v = (toList u) == (toList v)

Next we define some basic constructors for finger trees. The relationship between the nonempty and possibly empty variants is formalized by the `NonEmpty` class.

> instance NonEmpty NonEmptyFingerTree where
>   empty
>     :: ( Valued a )
>     => FingerTree a
>   empty = Empty
> 
>   isEmpty
>     :: ( Valued a )
>     => FingerTree a -> Bool
>   isEmpty x = case x of
>     Empty      -> True
>     NonEmpty _ -> False

And we also have `Singleton` instances for both variants. This class is inhabited by container types that can contain "only one item". Note how we make sure to respect the cached value invariant.

> instance Singleton FingerTree where
>   singleton
>     :: ( Valued a )
>     => a -> FingerTree a
>   singleton = NonEmpty . singleton
> 
>   fromSingleton
>     :: ( Valued a )
>     => FingerTree a -> Maybe a
>   fromSingleton x = case x of
>     Empty      -> Nothing
>     NonEmpty w -> fromSingleton w
> 
> instance Singleton NonEmptyFingerTree where
>   singleton
>     :: ( Valued a )
>     => a -> NonEmptyFingerTree a
>   singleton a = Leaf m a
>     where m = value a
> 
>   fromSingleton
>     :: ( Valued a )
>     => NonEmptyFingerTree a -> Maybe a
>   fromSingleton x = case x of
>     Leaf _ a -> Just a
>     _        -> Nothing
> 
> instance SubsetSingleton NonEmptyFingerTree
> instance NonEmptySingleton NonEmptyFingerTree

We also need constructors for the branched case for both tree variants.

> branch
>   :: ( Valued a )
>   => Some a -> FingerTree (Node a) -> Some a
>   -> FingerTree a
> branch heads mids lasts =
>   NonEmpty $ branchNonEmpty heads mids lasts
> 
> branchNonEmpty
>   :: ( Valued a )
>   => Some a -> FingerTree (Node a) -> Some a
>   -> NonEmptyFingerTree a
> branchNonEmpty heads mids lasts =
>   let
>      m = mconcat
>        [ value heads
>        , value mids
>        , value lasts
>        ]
>   in Branch m heads mids lasts

These constructors are useful inside this module, where we know (and need to know) the internal structure of a finger tree. But outside this module it will be more useful to expose constructors with less concrete names.

Like `Some` and `Node`, both finger tree constructors are constrained functors.

> instance FunctorC Valued (->) Valued (->) FingerTree where
>   fmapC
>     :: ( Valued a, Valued b )
>     => (a -> b) -> FingerTree a -> FingerTree b
>   fmapC f x = case x of
>     Empty      -> empty
>     NonEmpty z -> NonEmpty $
>       fmapC @Valued @(->) @Valued @(->) @NonEmptyFingerTree f z
> 
> instance FunctorC Valued (->) Valued (->) NonEmptyFingerTree where
>   fmapC
>     :: ( Valued a, Valued b )
>     => (a -> b) -> NonEmptyFingerTree a -> NonEmptyFingerTree b
>   fmapC f x = case x of
>     Leaf _ a          -> singleton (f a)
>     Branch _ as bs cs -> branchNonEmpty
>       (fmapC @Valued @(->) @Valued @(->) @Some f as)
>       (fmapC @Valued @(->) @Valued @(->) @FingerTree
>         (fmapC @Valued @(->) @Valued @(->) @Node f) bs)
>       (fmapC @Valued @(->) @Valued @(->) @Some f cs)

In the constrained functor instance for nonempty finger trees, note that the type parameter of the outer appearance of `FingerTree` is simply `a`, while on the inner appearance it is `Node a`. This is polymorphic recursion, and it took me a while to wrap my head around what it means.

Both variants are also constrained traversable functors:

> instance TraversableC Valued (->) Valued (->) FingerTree where
>   traverseC
>     :: ( Applicative f, Valued a, Valued b )
>     => (a -> f b) -> FingerTree a -> f (FingerTree b)
>   traverseC f w = case w of
>     Empty      -> pure empty
>     NonEmpty z -> fmap NonEmpty $
>       traverseC @Valued @(->) @Valued @(->) @NonEmptyFingerTree f z
> 
>   sequenceAC
>     :: ( Applicative f, Valued a, Valued (f a) )
>     => FingerTree (f a) -> f (FingerTree a)
>   sequenceAC w = case w of
>     Empty      -> pure Empty
>     NonEmpty z -> fmap NonEmpty $
>       sequenceAC @Valued @(->) @Valued @(->) @NonEmptyFingerTree z
> 
>   consumeC
>     :: ( Applicative f, Valued a )
>     => (FingerTree a -> b) -> FingerTree (f a) -> f b
>   consumeC h w = case w of
>     Empty      -> pure (h Empty)
>     NonEmpty z -> fmap (h . NonEmpty) $
>       consumeC @Valued @(->) @Valued @(->) id z
> 
> instance TraversableC Valued (->) Valued (->) NonEmptyFingerTree where
>   traverseC
>     :: ( Applicative f, Valued a, Valued b )
>     => (a -> f b) -> NonEmptyFingerTree a -> f (NonEmptyFingerTree b)
>   traverseC f w = case w of
>     Leaf _ a ->
>       singleton <$> f a
>     Branch _ as bs cs ->
>       branchNonEmpty
>         <$> traverseC @Valued @(->) @Valued @(->) @Some f as
>         <*> traverseC @Valued @(->) @Valued @(->) @FingerTree
>               (traverseC @Valued @(->) @Valued @(->) @Node f) bs
>         <*> traverseC @Valued @(->) @Valued @(->) @Some f cs
> 
>   sequenceAC
>     :: ( Applicative f, Valued a, Valued (f a) )
>     => NonEmptyFingerTree (f a) -> f (NonEmptyFingerTree a)
>   sequenceAC w = case w of
>     Leaf _ a ->
>       pure singleton <*> a
>     Branch _ as bs cs ->
>       pure branchNonEmpty
>         <*> sequenceAC @Valued @(->) @Valued @(->) @Some as
>         <*> traverseC @Valued @(->) @Valued @(->) @FingerTree
>               (sequenceAC @Valued @(->) @Valued @(->) @Node) bs
>         <*> sequenceAC @Valued @(->) @Valued @(->) @Some cs
> 
>   consumeC
>     :: ( Applicative f, Valued a )
>     => (NonEmptyFingerTree a -> b) -> NonEmptyFingerTree (f a) -> f b
>   consumeC h w = case w of
>     Leaf _ a ->
>       fmap h (singleton <$> a)
>     Branch _ as bs cs ->
>       fmap h $ branchNonEmpty
>         <$> sequenceAC @Valued @(->) @Valued @(->) @Some as
>         <*> traverseC @Valued @(->) @Valued @(->) @FingerTree
>               (sequenceAC @Valued @(->) @Valued @(->) @Node) bs
>         <*> sequenceAC @Valued @(->) @Valued @(->) @Some cs

Finally, just as every `Node` can be converted into a `Some`, every `Some` can be converted to a `FingerTree`. This conversion will come in handy later so we define it here, but this code is not exposed outside of this module.

> someToFingerTree
>   :: ( Valued a )
>   => Some a -> FingerTree a
> someToFingerTree w = case w of
>   Only1 _ a1 ->
>     singleton a1
>   Only2 _ a1 a2 ->
>     branch (only1 a1) empty (only1 a2)
>   Only3 _ a1 a2 a3 ->
>     branch (only2 a1 a2) empty (only1 a3)
>   Only4 _ a1 a2 a3 a4 ->
>     branch (only2 a1 a2) empty (only2 a3 a4)
> 
> maybeSomeToFingerTree
>   :: ( Valued a )
>   => Maybe (Some a) -> FingerTree a
> maybeSomeToFingerTree w = case w of
>   Nothing -> empty
>   Just z  -> someToFingerTree z



Cons and Uncons
---------------

In this section we'll implement four of the most important functions on finger trees, which efficiently build and decompose finger trees by adding or removing items from one of the ends.

The traditional name for appending an item to a list is _cons_, after the Lisp function for this operation. Given a nonempty finger tree, if the left end does not have a full complement of fingers then `cons` is a constant time operation. If it does, then we take some of the fingers and recursively `cons` them as a node to the inner tree. This is the first place where the magic numbers 4 and 3, for the numbers of fingers and children per node, respectively, become important. `cons` also has an inverse, called `uncons`.^[The inverseness here is not literal, but can be made so if instead of cons we consider the coproduct of cons with the constant `stump` function, but for our purposes here that's splitting hairs.]

`cons` and `uncons` on list-like structures satisfy several laws and so they are implemented using a type class. Note the signature of `uncons` returns the "rest of the structure" using the same type. This means we cannot uncons a non-empty finger tree with only one item in it. To get around this limitation we have a second class for unconsing types which inhabit `NonEmpty`.

> instance Cons FingerTree where
>   cons
>     :: ( Valued a )
>     => a -> FingerTree a -> FingerTree a
>   cons a w = case w of
>     Empty      -> singleton a
>     NonEmpty z -> NonEmpty $ cons a z
> 
>   uncons
>     :: ( Valued a )
>     => FingerTree a
>     -> Maybe (a, FingerTree a)
>   uncons w = case w of
>     Empty      -> Nothing
>     NonEmpty z -> Just $ unconsNonEmpty z
> 
> instance SingletonCons FingerTree
> 
> instance Cons NonEmptyFingerTree where
>   cons
>     :: ( Valued a )
>     => a -> NonEmptyFingerTree a -> NonEmptyFingerTree a
>   cons u z = case z of
>     Leaf _ a ->
>       branchNonEmpty (only1 u) empty (only1 a)
>     Branch _ as1 as2 as3 -> case as1 of
>       Only1 _ a1 ->
>         branchNonEmpty (only2 u a1) as2 as3
>       Only2 _ a1 a2 ->
>         branchNonEmpty (only3 u a1 a2) as2 as3
>       Only3 _ a1 a2 a3 ->
>         branchNonEmpty (only4 u a1 a2 a3) as2 as3
>       Only4 _ a1 a2 a3 a4 ->
>         branchNonEmpty (only2 u a1) (cons (node3 a2 a3 a4) as2) as3
> 
>   uncons
>     :: ( Valued a )
>     => NonEmptyFingerTree a -> Maybe (a, NonEmptyFingerTree a)
>   uncons w =
>     let (a, z) = unconsNonEmpty w
>     in case z of
>       Empty -> Nothing
>       NonEmpty u -> Just (a, u)
> 
> instance SingletonCons NonEmptyFingerTree
> instance SubsetCons NonEmptyFingerTree

The real work happens in `unconsNonEmpty`. Like `cons`, `unconsNonEmpty` is very fast if the left side of the tree has fingers to spare. But if not, we have to do a kind of recursive borrowing.^[Think "borrow" as in the usual algorithm for subtracting natural numbers.] This borrowing function will be useful on its own later in this module, so we give it its own name.

> instance UnconsNonEmpty NonEmptyFingerTree where
>   unconsNonEmpty
>     :: ( Valued a )
>     => NonEmptyFingerTree a
>     -> (a, FingerTree a)
>   unconsNonEmpty w = case w of
>     Leaf _ a -> (a, empty)
>     Branch _ as1 as2 as3 ->
>       let (a, as) = unconsSome as1
>       in (a, borrowL as as2 as3)
>     where
>       unconsSome
>         :: ( Valued a )
>         => Some a
>         -> (a, Maybe (Some a))
>       unconsSome w = case w of
>         Only1 _ a1          -> (a1, Nothing)
>         Only2 _ a1 a2       -> (a1, Just (only1 a2))
>         Only3 _ a1 a2 a3    -> (a1, Just (only2 a2 a3))
>         Only4 _ a1 a2 a3 a4 -> (a1, Just (only3 a2 a3 a4))
> 
> borrowL
>  :: ( Valued a )
>  => Maybe (Some a)
>  -> FingerTree (Node a)
>  -> Some a
>  -> FingerTree a
> borrowL w as2 as3 = case w of
>  Just as1 -> branch as1 as2 as3
>  Nothing -> case uncons as2 of
>    Nothing -> someToFingerTree as3
>    Just (a, as) -> branch (toSome a) as as3

The mirror operations -- appending and decomposing on the right -- are defined similarly. These are called `snoc` and `unsnoc`.

> instance Snoc FingerTree where
>   snoc
>    :: ( Valued a )
>    => a -> FingerTree a -> FingerTree a
>   snoc a w = case w of
>     Empty -> singleton a
>     NonEmpty z -> NonEmpty $ snoc a z
> 
>   unsnoc
>     :: ( Valued a )
>     => FingerTree a
>     -> Maybe (a, FingerTree a)
>   unsnoc w = case w of
>     Empty      -> Nothing
>     NonEmpty z -> Just $ unsnocNonEmpty z
> 
> instance SingletonSnoc FingerTree
> instance ConsSnoc FingerTree
> 
> instance Snoc NonEmptyFingerTree where
>   snoc
>     :: ( Valued a )
>     => a -> NonEmptyFingerTree a -> NonEmptyFingerTree a
>   snoc u z = case z of
>     Leaf _ a ->
>       branchNonEmpty (only1 a) empty (only1 u)
>     Branch _ as1 as2 as3 -> case as3 of
>       Only1 _ a1 ->
>         branchNonEmpty as1 as2 (only2 a1 u)
>       Only2 _ a1 a2 ->
>         branchNonEmpty as1 as2 (only3 a1 a2 u)
>       Only3 _ a1 a2 a3 ->
>         branchNonEmpty as1 as2 (only4 a1 a2 a3 u)
>       Only4 _ a1 a2 a3 a4 ->
>         branchNonEmpty as1 (snoc (node3 a1 a2 a3) as2) (only2 a4 u)
> 
>   unsnoc
>     :: ( Valued a )
>     => NonEmptyFingerTree a -> Maybe (a, NonEmptyFingerTree a)
>   unsnoc w =
>     let (a, z) = unsnocNonEmpty w
>     in case z of
>       Empty -> Nothing
>       NonEmpty u -> Just (a, u)
> 
> instance SingletonSnoc NonEmptyFingerTree
> instance SubsetSnoc NonEmptyFingerTree
> instance ConsSnoc NonEmptyFingerTree

And `unsnocNonEmpty` is analogous to `unconsNonEmpty`, with a corresponding right-borrow.

> instance UnsnocNonEmpty NonEmptyFingerTree where
>   unsnocNonEmpty
>     :: ( Valued a )
>     => NonEmptyFingerTree a
>     -> (a, FingerTree a)
>   unsnocNonEmpty z = case z of
>     Leaf _ a -> (a, empty)
>     Branch _ as1 as2 as3 ->
>       let (a, as) = unsnocSome as3
>       in (a, borrowR as1 as2 as)
>     where
>       unsnocSome
>         :: ( Valued a )
>         => Some a
>         -> (a, Maybe (Some a))
>       unsnocSome w = case w of
>         Only1 _ a1          -> (a1, Nothing)
>         Only2 _ a1 a2       -> (a2, Just (only1 a1))
>         Only3 _ a1 a2 a3    -> (a3, Just (only2 a1 a2))
>         Only4 _ a1 a2 a3 a4 -> (a4, Just (only3 a1 a2 a3))
> 
> borrowR
>  :: ( Valued a )
>  => Some a
>  -> FingerTree (Node a)
>  -> Maybe (Some a)
>  -> FingerTree a
> borrowR as1 as2 w = case w of
>  Just as3 -> branch as1 as2 as3
>  Nothing -> case unsnoc as2 of
>    Nothing -> someToFingerTree as1
>    Just (a, as) -> branch as1 as (toSome a)

Now's a good time for some examples.

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x :: FingerTree (Counted Char)
> --   x = fromList [ Counted 'a', Counted 'b', Counted 'c' ]
> -- in uncons x
> -- :}
> -- Just (Counted 'a',NonEmpty $ fromList [Counted 'b',Counted 'c'])
> --
> -- >>> :{
> -- let
> --   x :: FingerTree (Counted Char)
> --   x = fromList [ Counted 'a', Counted 'b', Counted 'c' ]
> -- in unsnoc x
> -- :}
> -- Just (Counted 'c',NonEmpty $ fromList [Counted 'a',Counted 'b'])

:::

With `cons` and `snoc` in hand we can define some helper functions. These convert from ordinary lists to finger trees, and are useful for testing.

> instance FromList FingerTree where
>   fromListMaybe
>     :: ( Valued a )
>     => [a] -> Maybe (FingerTree a)
>   fromListMaybe = Just . fromList
> 
> instance FromListMonoid FingerTree where
>   fromList
>     :: ( Valued a )
>     => [a] -> FingerTree a
>   fromList = foldr cons empty

> instance FromListConsSnocReverse FingerTree
> 
> instance FromList NonEmptyFingerTree where
>   fromListMaybe
>     :: ( Valued a )
>     => [a] -> Maybe (NonEmptyFingerTree a)
>   fromListMaybe xs = case xs of
>     []   -> Nothing
>     a:as -> Just $ foldl' (flip snoc) (singleton a) as
> 
> instance FromListConsSnocReverse NonEmptyFingerTree

With `fromList` in hand we can also give a convenient `Show` instance for nonempty finger trees. Note that the derived instance would include a lot of superfluous information about the internal structure of the tree.

> instance (Show a, Valued a) => Show (NonEmptyFingerTree a) where
>   show a = "fromList " ++ show (toList a)

We can test our intuition so far with some examples.

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x :: FingerTree (Counted Char)
> --   x = fromList [ Counted 'a', Counted 'b', Counted 'c']
> -- in x
> -- :}
> -- NonEmpty $ fromList [Counted 'a',Counted 'b',Counted 'c']
> --
> -- >>> :{
> -- let
> --   x :: FingerTree (Counted Char)
> --   x = fromList [ Counted 'b', Counted 'c', Counted 'd' ]
> -- in cons (Counted 'a') x
> -- :}
> -- NonEmpty $ fromList [Counted 'a',Counted 'b',Counted 'c',Counted 'd']

:::

With `snoc` we can also reverse the tree.

> instance Reverse FingerTree where
>   reverse
>     :: ( Valued a )
>     => FingerTree a -> FingerTree a
>   reverse = foldr snoc empty
> 
> instance ReverseSemigroup FingerTree
> instance ReverseMonoid FingerTree
> instance ReverseSingleton FingerTree
> instance ReverseConsSnoc FingerTree
> 
> instance Reverse NonEmptyFingerTree where
>   reverse
>     :: ( Valued a )
>     => NonEmptyFingerTree a -> NonEmptyFingerTree a
>   reverse x =
>     let (a, as) = unsnocNonEmpty x
>     in case as of
>       Empty -> singleton a
>       NonEmpty w -> foldr snoc (singleton a) w
> 
> instance ReverseSemigroup NonEmptyFingerTree
> instance ReverseSubset NonEmptyFingerTree
> instance ReverseSingleton NonEmptyFingerTree
> instance ReverseConsSnoc NonEmptyFingerTree

For example:

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x :: FingerTree (Counted Char)
> --   x = fromList [ Counted 'a', Counted 'b' ]
> -- in reverse x
> -- :}
> -- NonEmpty $ fromList [Counted 'b',Counted 'a']
> --
> -- >>> :{
> -- let
> --   x :: FingerTree (Counted Char)
> --   x = fromList [ Counted 'a', Counted 'b', Counted 'c' ]
> -- in reverse x
> -- :}
> -- NonEmpty $ fromList [Counted 'c',Counted 'b',Counted 'a']

:::

With `uncons`, we can define an alternate version of `toList` (from the `Foldable` class) that also returns the accumulated value annotations. This will be handy in a few places.

> toAnnotatedList
>  :: ( Valued a )
>  => FingerTree a -> [(a, Value a)]
> toAnnotatedList xs = foo mempty xs
>  where
>    foo e z = case uncons z of
>      Nothing -> []
>      Just (a, ys) ->
>        let u = e <> value a
>        in (a, u) : foo u ys



Concatenation
-------------

Despite the name, finger trees are really list-like, and on such structures it's typically handy to be able to append one list to another. The fancy word for this is _concatenation_. Before defining concatenation proper we start with a generalized version that takes an additional list of elements to insert between the concatenands.

We're defining this using a class to avoid giving this operation different names for possibly empty and nonempty finger trees.

> class ConcatWithList f where
>   concatWithList :: (Valued a) => f a -> [a] -> f a -> f a
> 
> instance ConcatWithList FingerTree where
>   concatWithList
>     :: ( Valued a )
>     => FingerTree a -> [a] -> FingerTree a
>     -> FingerTree a
>   concatWithList u as v = case u of
>     Empty -> foldr cons v as
>     NonEmpty z -> case v of
>       Empty -> foldl (flip snoc) u as
>       NonEmpty w ->
>         NonEmpty $ concatWithList z as w
> 
> instance ConcatWithList NonEmptyFingerTree where
>   concatWithList
>     :: ( Valued a )
>     => NonEmptyFingerTree a -> [a] -> NonEmptyFingerTree a
>     -> NonEmptyFingerTree a
>   concatWithList u as v = case u of
>     Leaf _ a -> cons a (foldr cons v as)
>     Branch _ us1 us2 us3 -> case v of
>       Leaf _ a -> snoc a (foldl (flip snoc) u as)
>       Branch _ vs1 vs2 vs3 ->
>         let ns = (toList us3) ++ as ++ (toList vs1)
>         in branchNonEmpty us1 (concatWithList us2 (toNodes ns) vs2) vs3
>     where
>       toNodes
>         :: ( Valued a )
>         => [a] -> [Node a]
>       toNodes w = case w of
>         [] -> []
>         [a1, a2] -> [node2 a1 a2]
>         [a1, a2, a3] -> [node3 a1 a2 a3]
>         [a1, a2, a3, a4] -> [node2 a1 a2, node2 a3 a4]
>         a1:a2:a3:a4:as -> (node3 a1 a2 a3) : toNodes (a4:as)
>         _ -> error "toNodes: panic"

We can check that this works as expected with an example.

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x1, x2 :: FingerTree (Counted Char)
> --   x1 = fromList [ Counted 'a', Counted 'b' ]
> --   x2 = fromList [ Counted 'd', Counted 'e' ]
> -- in concatWithList x1 [ Counted 'c' ] x2
> -- :}
> -- NonEmpty $ fromList [Counted 'a',Counted 'b',Counted 'c',Counted 'd',Counted 'e']

:::

Now the real `cat` is a specialization of `concatWithList`, and makes both varieties of finger trees into semigroups. Along with `empty` the possibly empty finger trees form a monoid.

> instance (Valued a) => Semigroup (NonEmptyFingerTree a) where
>   u <> v = concatWithList u [] v
> 
> instance (Valued a) => Semigroup (FingerTree a) where
>   u <> v = concatWithList u [] v
> 
> instance (Valued a) => Monoid (FingerTree a) where
>   mempty = empty

There's a bit more to be said about concatenation. At this point we have a type, `NonEmptyFingerTree`, which is canonically a "subtype" of another, `FingerTree`, and both of these types are semigroups. In fact the nonempty finger trees form a _subsemigroup_ of the possibly empty finger trees (a relationship which carries laws, but no additional operations). A natural question to ask is whether a subsemigroup is an _ideal_, and in this case it is. Ideals are subsets of a semigroup which are _absorptive_ under the semigroup operation; this is reflected in the type of the left- and right- absorption operators.

> instance Subsemigroup NonEmptyFingerTree
> 
> instance Ideal NonEmptyFingerTree where
>   (@>)
>     :: ( Valued a )
>     => NonEmptyFingerTree a -> FingerTree a -> NonEmptyFingerTree a
>   u @> v = case v of
>     Empty      -> u
>     NonEmpty w -> u <> w
> 
>   (<@)
>     :: ( Valued a )
>     => FingerTree a -> NonEmptyFingerTree a -> NonEmptyFingerTree a
>   u <@ v = case u of
>     Empty      -> v
>     NonEmpty w -> w <> v

We also have a kind of specialized `join` on finger trees called `inflateWith`. The intuition behind this function is that each entry in an input finger tree is expanded to a subsequence.

> inflateWith
>   :: forall a b
>    . ( Valued a, Valued b )
>   => (a -> [b]) -> FingerTree a -> FingerTree b
> inflateWith f w = inflate w
>   where
>     inflate :: FingerTree a -> FingerTree b
>     inflate w = case uncons w of
>       Nothing -> mempty
>       Just (a, as) -> (fromList (f a)) <> inflate as



Splitting
---------

The killer operation on finger trees, and the reason for caching the value at each node, is efficient _splitting_. This operation, which comes in left and right handed variants, takes a predicate `p` on `Value a` and a finger tree `w` and attempts to break it into three pieces, `as`, `x`, and `bs`. We can think of `x` as a _search result_ and of `as` and `bs` as the prefix and suffix of the search. Moreover, the search result must satisfy three _left-splitting properties_:

1. That `w == as <> singleton x <> bs` -- so that splitting is a bona fide cat-factorization;
2. That `p (value as)` is false -- so the portion of the list _up to_ the search result doesn't satisfy `p`; and
3. That `p (value as <> value x)` is true -- so the portion of the list _up to and including_ the search result does satisfy `p`.

(The right-splitting properties are dual.) The internal structure of finger trees allows this to be done efficiently. Note that the splitting properties do not say anything about the result being _unique_, or even extremal in any sense (leftmost or rightmost), and in general there may be many possible places to break the tree, with the result not guaranteed to be special in any way. Even worse, the splitting properties to not guarantee that if a valid splitting exists, one will be found. But with a judicious choice of `Value a` and the predicate we can guarantee uniqueness of the split, and that it will be found, and can perform this operation quickly.

It's natural to wonder why this is a useful thing to do. Well, for a well-chosen monoid and predicate, splitting can do some interesting things. As a simple example, with the `Count` monoid and a predicate like $p(k) = (k \geq N)$ we can break the finger tree at a specific index. Later on we'll use it to break a text buffer at a specific line and column position or screen coordinate.

To make life a little easier for consumers of this module, we'll wrap the result of a splitting behind a type, `SplitResult`, rather than returning a tuple with no context.

> data SplitResult a
>   = NotFound
>   | Found (FingerTree a) a (FingerTree a)
>   deriving (Eq, Show)

We also wrap the splitting operation itself in a type class because (1) it has associated laws, and (2) we have two implementations, possibly empty and nonempty, and want to use the same names for both. As we'll see in a moment, splitting is implemented recursively in terms of a "split with context" operation which takes a monoidal value.

> class Split f where
>   toFingerTree
>     :: ( Valued a )
>     => f a -> FingerTree a
> 
>   splitWithContextL
>     :: ( Valued a )
>     => Value a -> (Value a -> Bool) -> f a
>     -> SplitResult a
> 
>   splitWithContextR
>     :: ( Valued a )
>     => Value a -> (Value a -> Bool) -> f a
>     -> SplitResult a

Then the actual splitting operations are contextual splits with a trivial context.

> splitL
>   :: ( Valued a, Split f )
>   => (Value a -> Bool)
>   -> f a
>   -> SplitResult a
> splitL = splitWithContextL mempty
> 
> splitR
>   :: ( Valued a, Split f )
>   => (Value a -> Bool)
>   -> f a
>   -> SplitResult a
> splitR = splitWithContextR mempty

Now we'll get to the business of implementing this code: first we need a version of splitting on the fingers. This is pretty tedious because we're essentially using brute force to make sure the output satisfies the splitting properties. (`splitSome` is only used inside this module.) Tedious though it is, note that both variants of `splitSome` have constant complexity.

> splitSomeL
>  :: ( Valued a )
>  => (Value a -> Bool) -> Value a -> Some a
>  -> Maybe (Maybe (Some a), a, Maybe (Some a))
> splitSomeL p i w = case w of
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
> 
> splitSomeR
>   :: ( Valued a )
>   => (Value a -> Bool) -> Value a -> Some a
>   -> Maybe (Maybe (Some a), a, Maybe (Some a))
> splitSomeR p i w = case w of
>   Only1 _ a1 ->
>     let v1 = (value a1) <> i in
>     if p v1
>       then Just (Nothing, a1, Nothing)
>       else Nothing
>   Only2 _ a1 a2 ->
>     let v2 = (value a2) <> i in
>     if p v2
>       then Just (Just (only1 a1), a2, Nothing)
>       else
>         let v1 = (value a1) <> v2 in
>         if p v1
>           then Just (Nothing, a1, Just (only1 a2))
>           else Nothing
>   Only3 _ a1 a2 a3 ->
>     let v3 = (value a3) <> i in
>     if p v3
>       then Just (Just (only2 a1 a2), a3, Nothing)
>       else
>         let v2 = (value a2) <> v3 in
>         if p v2
>           then Just (Just (only1 a1), a2, Just (only1 a3))
>           else
>             let v1 = (value a1) <> v2 in
>             if p v1
>               then Just (Nothing, a1, Just (only2 a2 a3))
>               else Nothing
>   Only4 _ a1 a2 a3 a4 ->
>     let v4 = (value a4) <> i in
>     if p v4
>       then Just (Just (only3 a1 a2 a3), a4, Nothing)
>       else
>         let v3 = (value a3) <> v4 in
>         if p v3
>           then Just (Just (only2 a1 a2), a3, Just (only1 a4))
>           else
>             let v2 = (value a2) <> v3 in
>             if p v2
>               then Just (Just (only1 a1), a2, Just (only2 a3 a4))
>               else
>                 let v1 = (value a1) <> v2 in
>                 if p v1
>                   then Just (Nothing, a1, Just (only3 a2 a3 a4))
>                   else Nothing

Now we can implement splitting proper. For `FingerTree` it's easy -- all the real work is offloaded to the nonempty case.

> instance Split FingerTree where
>   toFingerTree = id
> 
>   splitWithContextL
>     :: ( Valued a )
>     => Value a -> (Value a -> Bool) -> FingerTree a
>     -> SplitResult a
>   splitWithContextL i p w = case w of
>     Empty -> NotFound
>     NonEmpty z -> splitWithContextL i p z
> 
>   splitWithContextR
>     :: ( Valued a )
>     => Value a -> (Value a -> Bool) -> FingerTree a
>     -> SplitResult a
>   splitWithContextR i p w = case w of
>     Empty -> NotFound
>     NonEmpty z -> splitWithContextR i p z

The nonempty case is where all the magic happens. This code looks complicated -- but all it's doing is looking at the cached valuations at each node to see if there is definitely an interior node where the valuation switches from false to true from left-to-right or right-to-left, depending on the direction of the split. If we can't be certain such a flip occurs, then the entire subtree is thrown out. This is what allows splitting to be efficient, but it's also what means there may be points in the tree which satisfy the splitting criteria, but which the splitting operation cannot find. To guarantee that splitting will succeed it is enough to make our predicates _monotone_ -- to ensure that they switch from false to true at some point along the list and never switch back. The Hinze and Paterson paper has more detail on this, including proofs.

> instance Split NonEmptyFingerTree where
>   toFingerTree = NonEmpty
> 
>   splitWithContextL
>     :: ( Valued a )
>     => Value a -> (Value a -> Bool) -> NonEmptyFingerTree a
>     -> SplitResult a
>   splitWithContextL i p w = case w of
>     Leaf _ a ->
>       let va = i <> (value a) in
>       if p va
>         then Found empty a empty
>         else NotFound
>     Branch _ as1 as2 as3 ->
>       let vas1 = i <> (value as1) in
>       -- look for a match in the left fingers
>       case splitSomeL p i as1 of
>         Just (ds1, x, ds3) ->
>           let
>             bs1 = maybeSomeToFingerTree ds1
>             bs3 = borrowL ds3 as2 as3
>           in Found bs1 x bs3
>         Nothing ->
>           let vas2 = vas1 <> (value as2) in
>           -- look for a match in the spine
>           case splitWithContextL vas1 p as2 of
>             Found cs1 xs cs3 ->
>               let vs = vas1 <> (value cs1) in
>               case splitSomeL p vs (toSome xs) of
>                 Just (ds1, x, ds3) ->
>                   let
>                     bs1 = borrowR as1 cs1 ds1
>                     bs3 = borrowL ds3 cs3 as3
>                   in Found bs1 x bs3
>                 Nothing -> error "splitWithContextL: panic"
>             NotFound ->
>               let vas3 = vas2 <> (value as3) in
>               -- look for a match in the right fingers
>               case splitSomeL p vas2 as3 of
>                 Just (ds1, x, ds3) ->
>                   let
>                     bs1 = borrowR as1 as2 ds1
>                     bs3 = maybeSomeToFingerTree ds3
>                   in Found bs1 x bs3
>                 Nothing -> NotFound
> 
>   splitWithContextR
>     :: ( Valued a )
>     => Value a -> (Value a -> Bool) -> NonEmptyFingerTree a
>     -> SplitResult a
>   splitWithContextR i p w = case w of
>     Leaf _ a ->
>       let va = (value a) <> i in
>       if p va
>         then Found empty a empty
>         else NotFound
>     Branch _ as1 as2 as3 ->
>       let vas3 = (value as3) <> i in
>       -- look for a match in the right fingers
>       case splitSomeR p i as3 of
>         Just (ds1, x, ds3) ->
>           let
>             bs1 = borrowR as1 as2 ds1
>             bs3 = maybeSomeToFingerTree ds3
>           in Found bs1 x bs3
>         Nothing ->
>           let vas2 = (value as2) <> vas3 in
>           -- look for a match in the spine
>           case splitWithContextR vas3 p as2 of
>             Found cs1 xs cs3 ->
>               let vs = (value cs3) <> vas3 in
>               case splitSomeR p vs (toSome xs) of
>                 Just (ds1, x, ds3) ->
>                   let
>                     bs1 = borrowR as1 cs1 ds1
>                     bs3 = borrowL ds3 cs3 as3
>                   in Found bs1 x bs3
>                 Nothing -> error "splitWithContextR: panic"
>             NotFound ->
>               let vas1 = (value as1) <> vas2 in
>               -- look for a match in the left fingers
>               case splitSomeR p vas2 as1 of
>                 Just (ds1, x, ds3) ->
>                   let
>                     bs1 = maybeSomeToFingerTree ds1
>                     bs3 = borrowL ds3 as2 as3
>                   in Found bs1 x bs3
>                 Nothing -> NotFound

Now's a good time for some examples.

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x :: FingerTree (Counted Char)
> --   x = fromList [Counted 'a', Counted 'b', Counted 'c', Counted 'd', Counted 'e']
> --   --
> --   us, vs :: FingerTree (Counted Char)
> --   us = fromList [ Counted 'a', Counted 'b', Counted 'c' ]
> --   vs = fromList [ Counted 'e' ]
> --   --
> --   p :: Count -> Bool
> --   p (Count k) = k > 3
> -- in Found us (Counted 'd') vs == splitL p x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   x :: FingerTree (Counted Char)
> --   x = fromList [ Counted 'a', Counted 'b', Counted 'c', Counted 'd' ]
> --   --
> --   us, vs :: FingerTree (Counted Char)
> --   us = fromList [ Counted 'a' ]
> --   vs = fromList [ Counted 'c', Counted 'd' ]
> --   --
> --   p :: Count -> Bool
> --   p (Count k) = k >= 2
> -- in Found us (Counted 'b') vs == splitL p x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   x :: FingerTree (Counted Char)
> --   x = fromList [ Counted 'a', Counted 'b', Counted 'c', Counted 'd' ]
> --   --
> --   p :: Count -> Bool
> --   p (Count k) = k >= 7
> -- in splitL p x
> -- :}
> -- NotFound

:::



Filtering
---------

> removeSplitsL
>   :: ( Valued a, Split f )
>   => (Value a -> Bool)
>   -> f a -> FingerTree a
> removeSplitsL p x = case splitL p x of
>   NotFound -> toFingerTree x
>   Found as u bs -> as <> removeSplitsL p bs
> 
> removeSplitsR
>   :: ( Valued a, Split f )
>   => (Value a -> Bool)
>   -> f a -> FingerTree a
> removeSplitsR p x = case splitR p x of
>   NotFound -> toFingerTree x
>   Found as u bs -> as <> removeSplitsR p bs



Testing and Debugging
---------------------

Finally we define some utilities to help with testing. First, to interoperate with our testing framework, we need `FingerTree` to be an instance of some type classes.

> instance (Fmt.Display a) => Fmt.Display (Some a) where
>   display x = case x of
>     Only1 _ a1 ->
>       "Only1" <+> display a1
>     Only2 _ a1 a2 ->
>       "Only2" <+> display a1 <+> display a2
>     Only3 _ a1 a2 a3 ->
>       "Only3" <+> display a1 <+> display a2 <+> display a3
>     Only4 _ a1 a2 a3 a4 ->
>       "Only4" <+> display a1 <+> display a2 <+> display a3 <+> display a4

> instance (Fmt.Display a) => Fmt.Display (Node a) where
>   display x = case x of
>     Node2 _ a1 a2 ->
>       "Node2" <+> display a1 <+> display a2
>     Node3 _ a1 a2 a3 ->
>       "Node3" <+> display a1 <+> display a2 <+> display a3

> instance
>   ( Fmt.Display a, Valued a
>   ) => Fmt.Display (FingerTree a)
>   where
>     display x = case x of
>       Empty      -> "Empty"
>       NonEmpty z -> "NonEmpty" <+> display z
> 
> instance
>   ( Arb a, Valued a
>   ) => Arb (FingerTree a)
>   where
>     arb = fromList <$> arb
> 
> instance
>   ( Prune a, Valued a
>   ) => Prune (FingerTree a)
>   where
>     prune =
>       map fromList . prune . toList
> 
> instance
>   ( CoArb a, Valued a
>   ) => CoArb (FingerTree a)
>   where
>     coarb x = coarb (toList x)
> 
> instance
>   ( MakeTo a, Valued a
>   ) => MakeTo (FingerTree a)
>   where
>     makeTo = makeToExtendWith
>       makeTo toList fromList

Similarly for `NonEmptyFingerTree`:

> instance
>   ( Fmt.Display a, Valued a
>   ) => Fmt.Display (NonEmptyFingerTree a)
>   where
>     display x = case x of
>       Leaf _ a -> "Leaf" <+> display a
>       Branch _ as bs cs -> "Branch"
>         <+> display as <+> display bs <+> display cs
> 
> instance
>   ( Arb a, Valued a
>   ) => Arb (NonEmptyFingerTree a)
>   where
>     arb = do
>       x <- fromListMaybe <$> (pure (:) <*> arb <*> arb)
>       case x of
>         Just t -> return t
>         Nothing -> error "Arb NonEmptyFingerTree (unreachable!)"
> 
> instance
>   ( Prune a, Valued a
>   ) => Prune (NonEmptyFingerTree a)
>   where
>     prune =
>       catMaybes . map fromListMaybe . filter (not . null) . prune . toList
> 
> instance
>   ( CoArb a, Valued a
>   ) => CoArb (NonEmptyFingerTree a)
>   where
>     coarb x = coarb (toList x)
> 
> instance
>   ( MakeTo a, Valued a
>   ) => MakeTo (NonEmptyFingerTree a)
>   where
>     makeTo = makeToExtendWith
>       makeTo f g
>       where
>         f :: NonEmptyFingerTree a -> (a, [a])
>         f x = case (toList x) of
>           a:as -> (a, as)
>           _ -> error "NonEmptyFingerTree MakeTo (unreachable 1!)"
> 
>         g :: (a, [a]) -> NonEmptyFingerTree a
>         g (a, as) = case fromListMaybe (a:as) of
>           Just x -> x
>           _ -> error "NonEmptyFingerTree MakeTo (unreachable 2!)"

Finally, recall that our finger tree type has an internal invariant that needs to be maintained in order for the complexity and correctness proofs of our algorithms to hold. Namely, the cached monoidal value at each Node vust be the product of the monoidal values of the node's contents. We introduce a function (only used for testing) to check this.

> validateFingerTree
>   :: ( Valued a )
>   => FingerTree a -> Bool
> validateFingerTree =
>   validateFingerTree' (const True)
> 
> validateNonEmptyFingerTree
>   :: ( Valued a )
>   => NonEmptyFingerTree a -> Bool
> validateNonEmptyFingerTree =
>   validateNonEmptyFingerTree' (const True)

Note how polymorphic recursion makes this a little tricky -- we define our validators in terms of an auxiliary predicate that builds up nested calls to `validateNode`.

> validateFingerTree'
>   :: ( Valued a )
>   => (a -> Bool) -> FingerTree a -> Bool
> validateFingerTree' p x = case x of
>   Empty -> True
>   NonEmpty w -> validateNonEmptyFingerTree' p w
> 
> validateNonEmptyFingerTree'
>   :: ( Valued a )
>   => (a -> Bool) -> NonEmptyFingerTree a -> Bool
> validateNonEmptyFingerTree' p x = case x of
>   Leaf v a -> and
>     [ p a
>     , v == value a
>     ]
>   Branch v as x bs -> and
>     [ v == mconcat [ value as, value x, value bs ]
>     , validateSome p as
>     , validateFingerTree' (validateNode p) x
>     , validateSome p bs
>     ]
>   where
>     validateSome
>       :: ( Valued a )
>       => (a -> Bool) -> Some a -> Bool
>     validateSome valid x = case x of
>       Only1 v a1 -> and
>         [ v == value a1
>         , valid a1
>         ]
>       Only2 v a1 a2 -> and
>         [ v == mconcat [ value a1, value a2 ]
>         , valid a1, valid a2
>         ]
>       Only3 v a1 a2 a3 -> and
>         [ v == mconcat [ value a1, value a2, value a3 ]
>         , valid a1, valid a2, valid a3
>         ]
>       Only4 v a1 a2 a3 a4 -> and
>         [ v == mconcat [ value a1, value a2, value a3, value a4 ]
>         , valid a1, valid a2, valid a3, valid a4
>         ]
> 
>     validateNode
>       :: ( Valued a )
>       => (a -> Bool) -> Node a -> Bool
>     validateNode valid x = case x of
>       Node2 v a1 a2 -> and
>         [ v == mconcat [ value a1, value a2 ]
>         , valid a1, valid a2
>         ]
>       Node3 v a1 a2 a3 -> and
>         [ v == mconcat [ value a1, value a2, value a3 ]
>         , valid a1, valid a2, valid a3
>         ]



::: epigraph

<blockquote>
I can whistle with my fingers, especially if I have a whistle.

<footer>Mitch Hedberg, <cite>Live at the Congress Theater in Chicago, 2004</cite></footer>
</blockquote>

:::
