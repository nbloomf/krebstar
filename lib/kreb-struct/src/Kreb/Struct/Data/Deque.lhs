---
title: Deques
---

::: contents
* [Introduction](#introduction): The problem we're solving
* [Code for (almost) free](#code-for-almost-free): 
* [Testing and Debugging](#testing-and-debugging): When things go wrong
:::



::: frontmatter

> {-# LANGUAGE StandaloneDeriving, OverloadedStrings #-}

> module Kreb.Struct.Data.Deque (
>     Deque()
>   , NonEmptyDeque()
> ) where

> import Prelude hiding (reverse)
> import Data.Foldable

> import qualified Kreb.Format as Fmt
> import           Kreb.Format ((<+>), display)
> import           Kreb.Prop hiding (Trivial)
> import           Kreb.Control
> import           Kreb.Control.Constrained

> import           Kreb.Struct.Class
> import qualified Kreb.Struct.Data.FingerTree as FT

:::



Introduction
------------

We've built a whole family of abstract data structures, the finger trees, which is parameterized by a monoid. In this module we'll describe one of the simplest useful instances of this family -- a double ended queue, also known as a _deque_. Calling this a finger tree is a bit generous because it uses the trivial monoid, meaning we can't meaningfully use the splitting machinery. But that's ok.

> data Deque a where
>   Empty
>     :: ( Unconstrained a )
>     => Deque a
>   NonEmpty
>     :: ( Unconstrained a )
>     => NonEmptyDeque a
>     -> Deque a
> 
> deriving instance (Eq a) => Eq (Deque a)
> deriving instance (Show a) => Show (Deque a)
> 
> data NonEmptyDeque a where
>   NonEmptyDeque
>     :: ( Unconstrained a )
>     => FT.NonEmptyFingerTree (Trivial a)
>     -> NonEmptyDeque a
> 
> deriving instance (Eq a) => Eq (NonEmptyDeque a)
> deriving instance (Show a) => Show (NonEmptyDeque a)

As usual we split the type into empty and nonempty counterparts. Now both are containers, with nonempty deques a subset of possibly empty deques.

> instance Container Deque where
>   type ContainerConstraint Deque = Unconstrained
> 
> instance Container NonEmptyDeque where
>   type ContainerConstraint NonEmptyDeque = Unconstrained
> 
> instance Subset NonEmptyDeque where
>   type SupersetOf NonEmptyDeque = Deque
> 
>   inject
>     :: ( Unconstrained a )
>     => NonEmptyDeque a -> Deque a
>   inject = NonEmpty
> 
>   restrict
>     :: ( Unconstrained a )
>     => Deque a -> Maybe (NonEmptyDeque a)
>   restrict x = case x of
>     Empty -> Nothing
>     NonEmpty w -> Just w
> 
> instance NonEmpty NonEmptyDeque where
>   empty
>     :: ( Unconstrained a )
>     => Deque a
>   empty = Empty
> 
>   isEmpty
>     :: ( Unconstrained a )
>     => Deque a -> Bool
>   isEmpty x = case x of
>     Empty -> True
>     _ -> False



Code for (almost) free
----------------------

Just about everything we need out of deques is covered by our container classes, and the implementations are inherited from their finger tree counterparts.^[Don't be confused by the appearance of _class_ and _inheritance_ here.]

We can convert from lists:

> instance FromList Deque where
>   fromList
>     :: ( Unconstrained a )
>     => [a] -> Deque a
>   fromList xs = case xs of
>     [] -> Empty
>     _  -> NonEmpty $ fromList xs
> 
> instance FromListMonoid Deque
> instance FromListConsSnocReverse Deque
> 
> instance FromList NonEmptyDeque where
>   fromList
>     :: ( Unconstrained a )
>     => [a] -> NonEmptyDeque a
>   fromList = NonEmptyDeque . fromList . map Trivial
> 
> instance FromListConsSnocReverse NonEmptyDeque

Foldable:

> instance Foldable Deque where
>   foldr
>     :: (a -> b -> b) -> b -> Deque a -> b
>   foldr f e x = case x of
>     Empty -> e
>     NonEmpty w -> foldr f e w
> 
> instance Foldable NonEmptyDeque where
>   foldr
>     :: forall a b
>      . (a -> b -> b) -> b -> NonEmptyDeque a -> b
>   foldr f e (NonEmptyDeque x) =
>     let
>       g :: Trivial a -> b -> b
>       g (Trivial a) b = f a b
>     in foldr g e x

Singleton:

> instance Singleton Deque where
>   singleton
>     :: ( Unconstrained a )
>     => a -> Deque a
>   singleton = NonEmpty . singleton
> 
>   isSingleton
>     :: ( Unconstrained a )
>     => Deque a -> Bool
>   isSingleton x = case x of
>     Empty -> False
>     NonEmpty w -> isSingleton w
> 
> instance Singleton NonEmptyDeque where
>   singleton
>     :: ( Unconstrained a )
>     => a -> NonEmptyDeque a
>   singleton = NonEmptyDeque . singleton . Trivial
> 
>   isSingleton
>     :: ( Unconstrained a )
>     => NonEmptyDeque a -> Bool
>   isSingleton (NonEmptyDeque x) =
>     isSingleton x

> instance SubsetSingleton NonEmptyDeque
> instance NonEmptySingleton NonEmptyDeque

Functor:

> instance Functor Deque where
>   fmap f x = case x of
>     Empty -> Empty
>     NonEmpty w -> NonEmpty (fmap f w)
> 
> instance Functor NonEmptyDeque where
>   fmap f (NonEmptyDeque x) =
>     NonEmptyDeque $ fmapC (fmap f) x

Traversable:

> instance Traversable Deque where
>   traverse f x = case x of
>     Empty -> pure Empty
>     NonEmpty z -> fmap NonEmpty $ traverse f z
> 
> instance Traversable NonEmptyDeque where
>   traverse f (NonEmptyDeque x) =
>     fmap NonEmptyDeque $ traverseC (traverse f) x

Semigroup:

> instance (Unconstrained a) => Semigroup (Deque a) where
>   x <> y = case (x,y) of
>     (Empty,      _         ) -> y
>     (_,          Empty     ) -> x
>     (NonEmpty u, NonEmpty v) -> NonEmpty (u <> v)
> 
> instance (Unconstrained a) => Monoid (Deque a) where
>   mempty = Empty
> 
> instance (Unconstrained a) => Semigroup (NonEmptyDeque a) where
>   (NonEmptyDeque u) <> (NonEmptyDeque v) =
>     NonEmptyDeque (u <> v)

Cons:

> instance Cons Deque where
>   cons
>     :: ( Unconstrained a )
>     => a -> Deque a -> Deque a
>   cons a x = case x of
>     Empty      -> singleton a
>     NonEmpty w -> NonEmpty (cons a w)
> 
>   uncons
>     :: ( Unconstrained a )
>     => Deque a
>     -> Maybe (a, Deque a)
>   uncons x = case x of
>     Empty      -> Nothing
>     NonEmpty w -> Just $ unconsNonEmpty w
> 
> instance SingletonCons Deque
> 
> instance Cons NonEmptyDeque where
>   cons
>     :: ( Unconstrained a )
>     => a -> NonEmptyDeque a -> NonEmptyDeque a
>   cons a (NonEmptyDeque x) =
>     NonEmptyDeque (cons (Trivial a) x)
> 
>   uncons
>     :: ( Unconstrained a )
>     => NonEmptyDeque a -> Maybe (a, NonEmptyDeque a)
>   uncons x =
>     let (a,z) = unconsNonEmpty x
>     in case z of
>       Empty -> Nothing
>       NonEmpty w -> Just (a, w)
> 
> instance SingletonCons NonEmptyDeque
> instance SubsetCons NonEmptyDeque
> 
> instance UnconsNonEmpty NonEmptyDeque where
>   unconsNonEmpty
>     :: ( Unconstrained a )
>     => NonEmptyDeque a
>     -> (a, Deque a)
>   unconsNonEmpty (NonEmptyDeque x) =
>     let (Trivial a, w) = unconsNonEmpty x
>     in case w of
>       FT.Empty -> (a, Empty)
>       FT.NonEmpty u -> (a, NonEmpty $ NonEmptyDeque u)

Snoc:

> instance Snoc Deque where
>   snoc
>     :: ( Unconstrained a )
>     => a -> Deque a -> Deque a
>   snoc a x = case x of
>     Empty      -> singleton a
>     NonEmpty w -> NonEmpty (snoc a w)
> 
>   unsnoc
>     :: ( Unconstrained a )
>     => Deque a
>     -> Maybe (a, Deque a)
>   unsnoc x = case x of
>     Empty      -> Nothing
>     NonEmpty w -> Just $ unsnocNonEmpty w
> 
> instance SingletonSnoc Deque
> instance ConsSnoc Deque
> 
> instance Snoc NonEmptyDeque where
>   snoc
>     :: ( Unconstrained a )
>     => a -> NonEmptyDeque a -> NonEmptyDeque a
>   snoc a (NonEmptyDeque x) =
>     NonEmptyDeque (snoc (Trivial a) x)
> 
>   unsnoc
>     :: ( Unconstrained a )
>     => NonEmptyDeque a -> Maybe (a, NonEmptyDeque a)
>   unsnoc x =
>     let (a,z) = unsnocNonEmpty x
>     in case z of
>       Empty -> Nothing
>       NonEmpty w -> Just (a, w)
> 
> instance SingletonSnoc NonEmptyDeque
> instance SubsetSnoc NonEmptyDeque
> instance ConsSnoc NonEmptyDeque
> 
> instance UnsnocNonEmpty NonEmptyDeque where
>   unsnocNonEmpty
>     :: ( Unconstrained a )
>     => NonEmptyDeque a
>     -> (a, Deque a)
>   unsnocNonEmpty (NonEmptyDeque x) =
>     let (Trivial a, w) = unsnocNonEmpty x
>     in case w of
>       FT.Empty      -> (a, Empty)
>       FT.NonEmpty u -> (a, NonEmpty $ NonEmptyDeque u)

Reverse:

> instance Reverse Deque where
>   reverse
>     :: ( Unconstrained a )
>     => Deque a -> Deque a
>   reverse x = case x of
>     Empty      -> Empty
>     NonEmpty w -> NonEmpty (reverse w)
> 
> instance ReverseSemigroup Deque
> instance ReverseMonoid Deque
> instance ReverseSingleton Deque
> instance ReverseConsSnoc Deque
> 
> instance Reverse NonEmptyDeque where
>   reverse
>     :: ( Unconstrained a )
>     => NonEmptyDeque a -> NonEmptyDeque a
>   reverse (NonEmptyDeque a) =
>     NonEmptyDeque (reverse a)
> 
> instance ReverseSemigroup NonEmptyDeque
> instance ReverseSubset NonEmptyDeque
> instance ReverseSingleton NonEmptyDeque
> instance ReverseConsSnoc NonEmptyDeque

And Ideal:

> instance Subsemigroup NonEmptyDeque
> 
> instance Ideal NonEmptyDeque where
>   (@>)
>     :: ( Unconstrained a )
>     => NonEmptyDeque a -> Deque a -> NonEmptyDeque a
>   u @> v = case v of
>     Empty      -> u
>     NonEmpty w -> u <> w
> 
>   (<@)
>     :: ( Unconstrained a )
>     => Deque a -> NonEmptyDeque a -> NonEmptyDeque a
>   u <@ v = case u of
>     Empty      -> v
>     NonEmpty w -> w <> v

Even better, we get tests for all these instances for free.



Testing and Debugging
---------------------

Finally we need some instances to integrate with the test framework.

> instance (Fmt.Display a) => Fmt.Display (Deque a) where
>   display x = case x of
>     Empty      -> "Empty"
>     NonEmpty z -> "NonEmpty" <+> display z
> 
> instance (Arb a) => Arb (Deque a) where
>   arb = fromList <$> arb
> 
> instance (Prune a) => Prune (Deque a) where
>   prune =
>     map fromList . prune . toList
> 
> instance (CoArb a) => CoArb (Deque a) where
>   coarb x = coarb (toList x)
> 
> instance (MakeTo a) => MakeTo (Deque a) where
>   makeTo = makeToExtendWith
>     makeTo toList fromList

> instance (Fmt.Display a) => Fmt.Display (NonEmptyDeque a) where
>   display (NonEmptyDeque x) = "NonEmptyDeque" <+> display x
> 
> instance (Arb a) => Arb (NonEmptyDeque a) where
>   arb =
>     fromList <$> (pure (:) <*> arb <*> arb)
> 
> instance (Prune a) => Prune (NonEmptyDeque a) where
>   prune =
>     map fromList . filter (not . null) . prune . toList
> 
> instance (CoArb a) => CoArb (NonEmptyDeque a) where
>   coarb x = coarb (toList x)
> 
> instance (MakeTo a) => MakeTo (NonEmptyDeque a) where
>   makeTo = makeToExtendWith
>     makeTo toList fromList
