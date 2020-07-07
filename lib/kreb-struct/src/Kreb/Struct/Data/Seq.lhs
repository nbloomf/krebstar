---
title: Seq
---

::: contents
* [Introduction](#introduction): The problem we're solving
* [Code for (almost) free](#code-for-almost-free): 
* [Testing and Debugging](#testing-and-debugging): When things go wrong
:::



::: frontmatter

> {-# LANGUAGE StandaloneDeriving, OverloadedStrings #-}

> module Kreb.Struct.Data.Seq (
>     Seq()
>   , NonEmptySeq()
> ) where

> import Prelude hiding (reverse)
> import Data.Foldable
> import Data.Maybe (catMaybes)

> import           Kreb.Control
> import           Kreb.Category
> import qualified Kreb.Format as Fmt
> import           Kreb.Format (display, (<+>))
> import           Kreb.Prop

> import           Kreb.Struct.Class
> import qualified Kreb.Struct.Data.FingerTree as FT

:::



Introduction
------------

> data Seq a where
>   Empty
>     :: ( Hask a )
>     => Seq a
>   NonEmpty
>     :: ( Hask a )
>     => NonEmptySeq a
>     -> Seq a
> 
> deriving instance (Eq a) => Eq (Seq a)
> deriving instance (Show a) => Show (Seq a)
> 
> data NonEmptySeq a where
>   NonEmptySeq
>     :: ( Hask a )
>     => FT.NonEmptyFingerTree (Counted a)
>     -> NonEmptySeq a
> 
> deriving instance (Eq a) => Eq (NonEmptySeq a)
> deriving instance (Show a) => Show (NonEmptySeq a)

As usual we split the type into empty and nonempty counterparts. Now both are containers, with nonempty Seqs a subset of possibly empty Seqs.

> instance Container Seq where
>   type ElementOf Seq = Hask
> 
> instance Container NonEmptySeq where
>   type ElementOf NonEmptySeq = Hask
> 
> instance Subset NonEmptySeq where
>   type SupersetOf NonEmptySeq = Seq
> 
>   inject
>     :: ( Hask a )
>     => NonEmptySeq a -> Seq a
>   inject = NonEmpty
> 
>   restrict
>     :: ( Hask a )
>     => Seq a -> Maybe (NonEmptySeq a)
>   restrict x = case x of
>     Empty -> Nothing
>     NonEmpty w -> Just w
> 
> instance NonEmpty NonEmptySeq where
>   empty
>     :: ( Hask a )
>     => Seq a
>   empty = Empty
> 
>   isEmpty
>     :: ( Hask a )
>     => Seq a -> Bool
>   isEmpty x = case x of
>     Empty -> True
>     _ -> False



Code for (almost) free
----------------------

Just about everything we need out of Seqs is covered by our container classes, and the implementations are inherited from their finger tree counterparts.^[Don't be confused by the appearance of _class_ and _inheritance_ here.]

We can convert from lists:

> instance FromList Seq where
>   fromListMaybe
>     :: ( Hask a )
>     => [a] -> Maybe (Seq a)
>   fromListMaybe = Just . fromList
> 
> instance FromListMonoid Seq where
>   fromList
>     :: ( Hask a )
>     => [a] -> Seq a
>   fromList xs = case fromListMaybe xs of
>     Nothing -> Empty
>     Just zs -> NonEmpty zs
> 
> instance FromListConsSnocReverse Seq
> 
> instance FromList NonEmptySeq where
>   fromListMaybe
>     :: ( Hask a )
>     => [a] -> Maybe (NonEmptySeq a)
>   fromListMaybe = fmap NonEmptySeq . fromListMaybe . map Counted
> 
> instance FromListConsSnocReverse NonEmptySeq

Foldable:

> instance Foldable Seq where
>   foldr
>     :: (a -> b -> b) -> b -> Seq a -> b
>   foldr f e x = case x of
>     Empty -> e
>     NonEmpty w -> foldr f e w
> 
> instance Foldable NonEmptySeq where
>   foldr
>     :: forall a b
>      . (a -> b -> b) -> b -> NonEmptySeq a -> b
>   foldr f e (NonEmptySeq x) =
>     let
>       g :: Counted a -> b -> b
>       g (Counted a) b = f a b
>     in foldr g e x

Singleton:

> instance Singleton Seq where
>   singleton
>     :: ( Hask a )
>     => a -> Seq a
>   singleton = NonEmpty . singleton
> 
>   fromSingleton
>     :: ( Hask a )
>     => Seq a -> Maybe a
>   fromSingleton x = case x of
>     Empty      -> Nothing
>     NonEmpty w -> fromSingleton w
> 
> instance Singleton NonEmptySeq where
>   singleton
>     :: ( Hask a )
>     => a -> NonEmptySeq a
>   singleton = NonEmptySeq . singleton . Counted
> 
>   fromSingleton
>     :: ( Hask a )
>     => NonEmptySeq a -> Maybe a
>   fromSingleton (NonEmptySeq x) =
>     fmap unCounted $ fromSingleton x

> instance SubsetSingleton NonEmptySeq
> instance NonEmptySingleton NonEmptySeq

Functor:

> instance Functor Seq where
>   fmap f x = case x of
>     Empty -> Empty
>     NonEmpty w -> NonEmpty (fmap f w)
> 
> instance Functor NonEmptySeq where
>   fmap f (NonEmptySeq x) = NonEmptySeq
>     (fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree
>       (fmap f) x)

Semigroup:

> instance (Hask a) => Semigroup (Seq a) where
>   x <> y = case (x,y) of
>     (Empty,      _         ) -> y
>     (_,          Empty     ) -> x
>     (NonEmpty u, NonEmpty v) -> NonEmpty (u <> v)
> 
> instance (Hask a) => Monoid (Seq a) where
>   mempty = Empty
> 
> instance (Hask a) => Semigroup (NonEmptySeq a) where
>   (NonEmptySeq u) <> (NonEmptySeq v) =
>     NonEmptySeq (u <> v)

Cons:

> instance Cons Seq where
>   cons
>     :: ( Hask a )
>     => a -> Seq a -> Seq a
>   cons a x = case x of
>     Empty      -> singleton a
>     NonEmpty w -> NonEmpty (cons a w)
> 
>   uncons
>     :: ( Hask a )
>     => Seq a
>     -> Maybe (a, Seq a)
>   uncons x = case x of
>     Empty      -> Nothing
>     NonEmpty w -> Just $ unconsNonEmpty w
> 
> instance SingletonCons Seq
> 
> instance Cons NonEmptySeq where
>   cons
>     :: ( Hask a )
>     => a -> NonEmptySeq a -> NonEmptySeq a
>   cons a (NonEmptySeq x) =
>     NonEmptySeq (cons (Counted a) x)
> 
>   uncons
>     :: ( Hask a )
>     => NonEmptySeq a -> Maybe (a, NonEmptySeq a)
>   uncons x =
>     let (a,z) = unconsNonEmpty x
>     in case z of
>       Empty -> Nothing
>       NonEmpty w -> Just (a, w)
> 
> instance SingletonCons NonEmptySeq
> instance SubsetCons NonEmptySeq
> 
> instance UnconsNonEmpty NonEmptySeq where
>   unconsNonEmpty
>     :: ( Hask a )
>     => NonEmptySeq a
>     -> (a, Seq a)
>   unconsNonEmpty (NonEmptySeq x) =
>     let (Counted a, w) = unconsNonEmpty x
>     in case w of
>       FT.Empty -> (a, Empty)
>       FT.NonEmpty u -> (a, NonEmpty $ NonEmptySeq u)

Snoc:

> instance Snoc Seq where
>   snoc
>     :: ( Hask a )
>     => a -> Seq a -> Seq a
>   snoc a x = case x of
>     Empty      -> singleton a
>     NonEmpty w -> NonEmpty (snoc a w)
> 
>   unsnoc
>     :: ( Hask a )
>     => Seq a
>     -> Maybe (a, Seq a)
>   unsnoc x = case x of
>     Empty      -> Nothing
>     NonEmpty w -> Just $ unsnocNonEmpty w
> 
> instance SingletonSnoc Seq
> instance ConsSnoc Seq
> 
> instance Snoc NonEmptySeq where
>   snoc
>     :: ( Hask a )
>     => a -> NonEmptySeq a -> NonEmptySeq a
>   snoc a (NonEmptySeq x) =
>     NonEmptySeq (snoc (Counted a) x)
> 
>   unsnoc
>     :: ( Hask a )
>     => NonEmptySeq a -> Maybe (a, NonEmptySeq a)
>   unsnoc x =
>     let (a,z) = unsnocNonEmpty x
>     in case z of
>       Empty -> Nothing
>       NonEmpty w -> Just (a, w)
> 
> instance SingletonSnoc NonEmptySeq
> instance SubsetSnoc NonEmptySeq
> instance ConsSnoc NonEmptySeq
> 
> instance UnsnocNonEmpty NonEmptySeq where
>   unsnocNonEmpty
>     :: ( Hask a )
>     => NonEmptySeq a
>     -> (a, Seq a)
>   unsnocNonEmpty (NonEmptySeq x) =
>     let (Counted a, w) = unsnocNonEmpty x
>     in case w of
>       FT.Empty      -> (a, Empty)
>       FT.NonEmpty u -> (a, NonEmpty $ NonEmptySeq u)

Reverse:

> instance Reverse Seq where
>   reverse
>     :: ( Hask a )
>     => Seq a -> Seq a
>   reverse x = case x of
>     Empty      -> Empty
>     NonEmpty w -> NonEmpty (reverse w)
> 
> instance ReverseSemigroup Seq
> instance ReverseMonoid Seq
> instance ReverseSingleton Seq
> instance ReverseConsSnoc Seq
> 
> instance Reverse NonEmptySeq where
>   reverse
>     :: ( Hask a )
>     => NonEmptySeq a -> NonEmptySeq a
>   reverse (NonEmptySeq a) =
>     NonEmptySeq (reverse a)
> 
> instance ReverseSemigroup NonEmptySeq
> instance ReverseSubset NonEmptySeq
> instance ReverseSingleton NonEmptySeq
> instance ReverseConsSnoc NonEmptySeq

And Ideal:

> instance Subsemigroup NonEmptySeq
> 
> instance Ideal NonEmptySeq where
>   (@>)
>     :: ( Hask a )
>     => NonEmptySeq a -> Seq a -> NonEmptySeq a
>   u @> v = case v of
>     Empty      -> u
>     NonEmpty w -> u <> w
> 
>   (<@)
>     :: ( Hask a )
>     => Seq a -> NonEmptySeq a -> NonEmptySeq a
>   u <@ v = case u of
>     Empty      -> v
>     NonEmpty w -> w <> v

Even better, we get tests for all these instances for free.



Testing and Debugging
---------------------

Finally we need some instances to integrate with the test framework.

> instance (Fmt.Display a) => Fmt.Display (Seq a) where
>   display x = case x of
>     Empty      -> "Empty"
>     NonEmpty z -> "NonEmpty" <+> display z
> 
> instance (Arb a) => Arb (Seq a) where
>   arb = fromList <$> arb
> 
> instance (Prune a) => Prune (Seq a) where
>   prune =
>     map fromList . prune . toList
> 
> instance (CoArb a) => CoArb (Seq a) where
>   coarb x = coarb (toList x)
> 
> instance (MakeTo a) => MakeTo (Seq a) where
>   makeTo = makeToExtendWith
>     makeTo toList fromList
> 
> instance (Fmt.Display a) => Fmt.Display (NonEmptySeq a) where
>   display (NonEmptySeq x) = "NonEmptySeq" <+> display x
> 
> instance (Arb a) => Arb (NonEmptySeq a) where
>   arb = do
>     x <- fromListMaybe <$> (pure (:) <*> arb <*> arb)
>     case x of
>       Nothing -> error "NonEmptySeq Arb (unreachable!)"
>       Just z -> return z
> 
> instance (Prune a) => Prune (NonEmptySeq a) where
>   prune =
>     catMaybes . map fromListMaybe . filter (not . null) . prune . toList
> 
> instance (CoArb a) => CoArb (NonEmptySeq a) where
>   coarb x = coarb (toList x)
> 
> instance (MakeTo a) => MakeTo (NonEmptySeq a) where
>   makeTo = makeToExtendWith
>     makeTo f g
>     where
>       f :: NonEmptySeq a -> (a, [a])
>       f x = case toList x of
>         a:as -> (a, as)
>         _ -> error "NonEmptySeq MakeTo (unreachable 1!)"
> 
>       g :: (a, [a]) -> NonEmptySeq a
>       g (a, as) = case fromListMaybe (a:as) of
>         Just x -> x
>         _ -> error "NonEmptySeq MakeTo (unreachable 2!)"
