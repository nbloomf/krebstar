---
title: Container Classes
author: nbloomf
---

::: frontmatter

> {-# LANGUAGE TypeFamilyDependencies #-}
> {-# LANGUAGE FlexibleContexts #-}
> 
> module Kreb.Struct.Class.Container where
> 
> import Prelude hiding (reverse)
> import qualified Prelude as Prelude (reverse)
> import Control.Monad (join)
> import GHC.Exts (Constraint)
> 
> import Kreb.Category
> import Kreb.Prop

:::



Introduction
------------

In this package we'll define several kinds of data structures. The one thing they all have in common is that they contain data of some type, which in general is subject to a constraint. We'll capture this using a type class, `Container`, with an associated type family:

> class Container (t :: * -> *) where
>   type ElementOf t :: * -> Constraint

For example, lists are a container with the trivial constraint.

> instance Container [] where
>   type ElementOf [] = Hask

It is very common for the same "operation" to make sense for lots of different specific data structures; things like "append a new item to the front" or "construct an instance containing only one item". Moreover, it's very common for these operations to satisfy the same properties. And so we will bundle these, where it makes sense, into lawful type classes. This brings some benefits:

- We can avoid needing to qualify function names (this is just solving a namespacing problem)
- We can write generic code against the class interface (although in practice this is really only useful for tests)
- We can leverage a property-based understanding of core functionality across multiple structures.

I should mention here that it's not obvious that this is the best way, or necessarily a _good_ way, to structure a library like this. Some of the classes (and laws) we define will be reasonably "natural", but some will be a bit more ad hoc. And ad hoc code is bad; the best programs are the ones that write themselves. But we'll do what we can.



Subset Types
------------

The first lawful class we define is really papering over a deficiency in Haskell's type system: it cannot reason about subset relationships among types. The `Subset` class asserts that a given container type, `t`, constructs a "subset" of a **fixed** associated container type `SupersetOf t`. Right off the bat this is not optimal, because it doesn't make sense to say that a set is only a subset of _one_ other set. For our purposes this turns out to be okay.

The more important question is: what operations and laws should "subsets" satisfy? It should be possible to _inject_ a value from the subset type into the superset type. It should also be possible to _restrict_ a value from the superset type to the subset type. These should also satisfy the following laws:

Restrict/Inject Law
  : `restrict . inject` is equivalent to `Just`

Inject/Restrict Law
  : If `restrict x == Just a`, then `x == inject a`

These essentially state that `inject` is injective as a function and that `restrict` is its one-sided inverse.

> class
>   ( Container t, Container (SupersetOf t)
>   ) => Subset t
>   where
>     type SupersetOf t = (u :: * -> *) | u -> t
> 
>     inject
>       :: ( ElementOf t a )
>       => t a -> SupersetOf t a
> 
>     restrict
>       :: ( ElementOf t a )
>       => SupersetOf t a -> Maybe (t a)

Next up: if one type is a subset of another, and both types happen to also be instances of `Semigroup`, then we in fact have a `Subsemigroup` if their `<>` operations are compatible. That is, instances of `Subsemigroup` must satisfy the following law:

Subsemigroup Law
  : `inject (u <> v)` is equivalent to `inject u <> inject v`

This law is asserting that `inject` is a homomorphism.

> class
>   ( Subset t
>   , forall a. (ElementOf t a) => Semigroup (t a)
>   , forall a. (ElementOf (SupersetOf t) a) => Semigroup (SupersetOf t a)
>   ) => Subsemigroup t

If you remember your abstract algebra, recall that some subsemigroups are especially important. A subsemigroup is called an _ideal_ if it "absorbs" other semigroup elements under multiplication. That is, if $S$ is a semigroup and $I$ a subsemigroup of $S$, then $I$ is an ideal in $S$ if anytime we take $s \in S$ and $a \in I$, we have $sa \in I$ and $as \in I$. To make the types work out we need new operations to express these mixed multiplications, and that's exactly what the `Ideal` class is for.

To make sure the mixed multiplications are compatible with both the ideal and semigroup multiplications, instances of this class must satisfy the following laws:

Left Injection
  : `inject (u @> v)` is equivalent to `inject u <> v`

Right Injection
  : `inject (u <@ v)` is equivalent to `u <> inject v`

Left Associativity
  : `(u <> v) @> w` is equivalent to `u <> (v @> w)`

Right Associativity
  : `u <@ (v <> w)` is equivalent to `(u <@ v) <> w`

Inner Interchange
  : `u <@ (v @> w)` is equivalent to `(u <@ v) @> w`

Outer Interchange
  : `u <> (v <@ w)` is equivalent to `(u @> v) <> w`

These laws assert that the semigroup acts on the ideal, and this action is equivalent to multiplication (after injection).

> class (Subsemigroup t) => Ideal t where
>   (@>)
>     :: ( ElementOf t a )
>     => t a -> SupersetOf t a -> t a
> 
>   (<@)
>     :: ( ElementOf t a )
>     => SupersetOf t a -> t a -> t a

For convenience we also define mixed multiplications with outputs in the supersemigroup.

> (>@>)
>   :: ( Ideal t, ElementOf t a )
>   => t a -> SupersetOf t a -> SupersetOf t a
> u >@> v = inject (u @> v)
> 
> (<@<)
>   :: ( Ideal t, ElementOf t a )
>   => SupersetOf t a -> t a -> SupersetOf t a
> u <@< v = inject (u <@ v)

We are describing operations on container types. Very often, it makes sense to have the concept of an _empty_ container, with nothing in it. It also sometimes makes sense to have a different type of containers which cannot be empty.

The `NonEmpty` class is inhabited by `Subset`s which contain only nonempty containers. The functions on this class are `empty`, which constructs and empty value, and `isEmpty`, which detects if a given value is empty. Inhabitants of this class must satisfy the following properties:

Empty Law
  : `isEmpty empty` is equivalent to `True`

Restrict Law
  : `restrict x == Nothing` is equivalent to `isEmpty x`

Inject Law
  : `isEmpty (inject x)` is equivalent to `False`

These laws assert that `empty` is empty, that values in the subset are not empty, and that _all_ nonempty values in the superset come from values in the subset.

> class (Subset t) => NonEmpty (t :: * -> *) where
>   empty
>     :: ( ElementOf t a )
>     => SupersetOf t a
> 
>   isEmpty
>     :: ( ElementOf t a )
>     => SupersetOf t a -> Bool
>   isEmpty x = case restrict x of
>     Nothing -> True; Just _ -> False

Some container types have a natural notion of `singleton` -- a container holding exactly one item. In general we'd like to construct and detect singleton values; the `Singleton` class models this. Instances of this class should satisfy the following laws:

Singleton
  : `fromSingleton (singleton a)` is equivalent to `Just a`

FromSingleton
  : `fromSingleton x == Just a` is equivalent to `x == singleton a`

These laws assert that `singleton` is injective as a function, and moreover all of the "singletons" are in its image. The helper function `isSingleton` detects singletons.

> class (Container t) => Singleton (t :: * -> *) where
>   singleton
>     :: ( ElementOf t a )
>     => a -> t a
> 
>   fromSingleton
>     :: ( ElementOf t a )
>     => t a -> Maybe a
> 
> isSingleton
>   :: ( Singleton t, ElementOf t a )
>   => t a -> Bool
> isSingleton x = case fromSingleton x of
>   Nothing -> False; Just _ -> True

We have a `Singleton` instance for lists:

> instance Singleton [] where
>   singleton
>     :: ( Hask a )
>     => a -> [a]
>   singleton a = [a]
> 
>   fromSingleton
>     :: ( Hask a )
>     => [a] -> Maybe a
>   fromSingleton xs = case xs of
>     [a] -> Just a
>     _   -> Nothing

The basic container classes (Ideal, Singleton, NonEmpty) are intended to capture very general behavior. When two or more of them interact we impose additional laws. For example, if some type is a singleton instance as well as a subset of another singleton instance, we demand that the subset function be compatible with subsets by the following properties:

Inject/Singleton
  : `inject (singleton a)` is equivalent to `singleton a`

Restrict/Singleton
  : `restrict (singleton a)` is equivalent to `Just (singleton a)`

These are stating that injection is a singleton-homomorphism.

> class
>   ( Subset t, Singleton t, Singleton (SupersetOf t)
>   ) => SubsetSingleton (t :: * -> *)

If a singleton instance is also a non-empty instance, we demand the following:

Empty
  : `isSingleton empty` is equivalent to `False`

Singleton
  : `isEmpty (singleton a)` is equivalent to `False`

> class
>   ( NonEmpty t, SubsetSingleton t
>   ) => NonEmptySingleton (t :: * -> *)



Semigroups and Actions
----------------------

If a container type is a semigroup, it may also be the case that the element type acts on this semigroup. This isn't quite a semigroup action in the usual sense; instead of a semigroup acting on a set, we have a set acting on a semigroup. Actions come in two flavors, left and right, which we call `Cons` and `Snoc` after the traditional name for these operations on lists.

Instances of the `Cons` class must satisfy the following laws:

Product/Cons
  : `cons a (x <> y)` is equivalent to `(cons a x) <> y`

Uncons/Cons
  : `uncons (cons a x)` is equivalent to `Just (a, x)`

Inverse/Cons
  : `uncons w == Just (a, x)` is equivalent to `cons a x == w`

These essentially require that `uncurry cons` be injective with one-sided inverse `uncons`. `readFirst` is a safe version of the standard Haskell function `head` on lists.

> class
>   ( Container t
>   , forall a. (ElementOf t a) => Semigroup (t a)
>   ) => Cons (t :: * -> *)
>   where
>     cons
>       :: ( ElementOf t a )
>       => a -> t a -> t a
> 
>     uncons
>       :: ( ElementOf t a )
>       => t a -> Maybe (a, t a)
> 
> readFirst
>   :: ( Cons t, ElementOf t a )
>   => t a -> Maybe a
> readFirst = fmap fst . uncons

Of course we have a `Cons` instance for lists:

> instance Cons [] where
>   cons
>     :: ( Hask a )
>     => a -> [a] -> [a]
>   cons a as = a:as
> 
>   uncons
>     :: ( Hask a )
>     => [a] -> Maybe (a, [a])
>   uncons x = case x of
>     [] -> Nothing
>     a:as -> Just (a, as)

If a type inhabits both `Cons` and `Singleton`, we require the following additional law:

Singleton/Cons
  : `cons a x` is equivalent to `(singleton a) <> x`

> class (Singleton t, Cons t) => SingletonCons t

If a type inhabits both `Cons` and `Subset`, we require the following additional laws:

Inject/Cons
  : `inject (cons a x)` is equivalent to `cons a (inject x)`

Restrict/Cons
  : `restrict (cons a (inject x))` is equivalent to `Just (cons a x)`

> class (Subset t, Cons t, Cons (SupersetOf t)) => SubsetCons t

If a type inhabits `Cons`, `Subset`, and `NonEmpty`, we have an additional primitive, `unconsNonEmpty`. This behaves similarly to `uncons`, but is guaranteed to succeed (although the remainder structure may be empty). We also demand some additional laws:

Inject/UnconsNonEmpty
  : if `(a, as) = unconsNonEmpty x` then `inject x` is equivalent to `cons a as`

Restrict/UnconsNonEmpty
  : `fmap unconsNonEmpty (restrict (cons a x))` is equivalent to `Just (a, x)`

Empty/Uncons
  : `isEmpty x` is equivalent to `Nothing == uncons x`

Inject/Uncons
  : if `uncons w == Just (a, x)` then `unconsNonEmpty w == (a, inject x)`

> class
>   ( Cons t, NonEmpty t, Subset t, Cons (SupersetOf t)
>   ) => UnconsNonEmpty (t :: * -> *)
>   where
>     unconsNonEmpty
>       :: ( ElementOf t a )
>       => t a -> (a, SupersetOf t a)
> 
> readFirstNonEmpty
>   :: ( UnconsNonEmpty t, ElementOf t a )
>   => t a -> a
> readFirstNonEmpty = fst . unconsNonEmpty

Dual to `Cons` is the `Snoc` class, with the following laws:

Product/Snoc
  : `snoc a (x <> y)` is equivalent to `x <> (snoc a y)`

Uncons/Snoc
  : `unsnoc (snoc a x)` is equivalent to `Just (a, x)`

Inverse/Snoc
  : `unsnoc w == Just (a, x)` is equivalent to `snoc a x == w`

> class
>   ( Container t
>   , forall a. (ElementOf t a) => Semigroup (t a)
>   ) => Snoc (t :: * -> *)
>   where
>     snoc
>       :: ( ElementOf t a )
>       => a -> t a -> t a
> 
>     unsnoc
>       :: ( ElementOf t a )
>       => t a -> Maybe (a, t a)
> 
> readLast
>   :: ( Snoc t, ElementOf t a )
>   => t a -> Maybe a
> readLast = fmap fst . unsnoc
> 
> instance Snoc [] where
>   snoc
>     :: ( Hask a )
>     => a -> [a] -> [a]
>   snoc a as = as ++ [a]
> 
>   unsnoc
>     :: ( Hask a )
>     => [a] -> Maybe (a, [a])
>   unsnoc x = case Prelude.reverse x of
>     [] -> Nothing
>     a:as -> Just (a, Prelude.reverse as)

If a type inhabits both `Snoc` and `Singleton` we have an additional law:

Singleton/Snoc
  : `snoc a x` is equivalent to `x <> (singleton a)`

> class (Singleton t, Snoc t) => SingletonSnoc t

If a type inhabits both `Snoc` and `Subset`, we require the following additional laws:

Inject/Snoc
  : `inject (snoc a x)` is equivalent to `snoc a (inject x)`

Restrict/Snoc
  : `restrict (snoc a (inject x))` is equivalent to `Just (snoc a x)`

> class (Subset t, Snoc t, Snoc (SupersetOf t)) => SubsetSnoc t

If a type inhabits `Snoc`, `Subset`, and `NonEmpty`, we have an additional primitive, `unsnocNonEmpty`, analogous to `unconsNonEmpty`. We require the following laws:

Inject/UnsnocNonEmpty
  : if `(a, as) = unsnocNonEmpty x` then `inject x` is equivalent to `snoc a as`

Restrict/UnsnocNonEmpty
  : `fmap unsnocNonEmpty (restrict (snoc a x))` is equivalent to `Just (a, x)`

Empty/Unsnoc
  : `isEmpty x` is equivalent to `Nothing == unsnoc x`

Inject/Unsnoc
  : if `unsnoc w == Just (a, x)` then `unsnocNonEmpty w == (a, inject x)`

> class
>   ( Snoc t, NonEmpty t, Subset t, Snoc (SupersetOf t)
>   ) => UnsnocNonEmpty (t :: * -> *)
>   where
>     unsnocNonEmpty
>       :: ( ElementOf t a )
>       => t a -> (a, SupersetOf t a)
> 
> readLastNonEmpty
>   :: ( UnsnocNonEmpty t, ElementOf t a )
>   => t a -> a
> readLastNonEmpty = fst . unsnocNonEmpty

And if a type inhabits both `Cons` and `Snoc`, we require them to interact nicely:

Cons/Snoc
  : `cons u (snoc v x)` is equivalent to `snoc v (cons u x)`

> class (Cons t, Snoc t) => ConsSnoc t



List-like Containers
--------------------

Roughly speaking, a container is _list-like_ if it arranges a set of values in a specific order. For such structures it makes sense to _reverse_ the order, and this operation should be an involution:

Reverse Involution
  : `reverse . reverse` is equivalent to `id`

> class (Container t) => Reverse t where
>   reverse
>     :: ( ElementOf t a )
>     => t a -> t a

If our type also constructs semigroups, `reverse` should be an antihomomorphism:

Reverse Antihomomorphism
  : `reverse (x <> y)` is equivalent to `reverse y <> reverse x`

> class
>   ( Reverse t
>   , forall a. (ElementOf t a) => Semigroup (t a)
>   ) => ReverseSemigroup t

And if our type constructs monoids, `reverse` should fix the identity.

Reverse Identity
  : `reverse mempty` is equivalent to `mempty`

> class
>   ( Reverse t
>   , forall a. (ElementOf t a) => Monoid (t a)
>   ) => ReverseMonoid t

If our type inhabits both `Reverse` and `Subset`, it should commute with `inject` and `restrict`.

Reverse/Inject
  : `reverse (inject x)` is equivalent to `inject (reverse x)`

Reverse/Restrict
  : `restrict (reverse x)` is equivalent to `fmap reverse (restrict x)`

> class (Reverse t, Subset t, Reverse (SupersetOf t)) => ReverseSubset t


Reversal should fix singletons:

Reverse/Singleton
  : `reverse (singleton a)` is equivalent to `singleton a`

> class (Reverse t, Singleton t) => ReverseSingleton t

And if our type also inhabits `Cons` and `Snoc`, reversal should convert between them.

Reverse/Cons
  : `reverse (cons a x)` is equivalent to `snoc a (reverse x)`

Reverse/Snoc
  : `reverse (snoc a x)` is equivalent to `cons a (reverse x)`

> class (Reverse t, Cons t, Snoc t) => ReverseConsSnoc t

Finally we have types that are indistinguishable from lists. Since `[a]` is the free monoid on `a`, it is natural to require such types to be homomorphic images of `[a]`.

FromList/ToList
  : `fromList . toList` is equivalent to `Just`

ToList Homomorphism
  : `toList (u <> v)` is equivalent to `toList u <> toList v`

> class
>   ( Container t, Foldable t
>   , forall a. (ElementOf t a) => Semigroup (t a)
>   ) => FromList t
>   where
>     fromListMaybe :: (ElementOf t a) => [a] -> Maybe (t a)

If `t a` is always a monoid (not just a semigroup) we can demand a bit more.

ToList/FromList
  : `toList . fromList` is equivalent to `id`

FromList Homomorphism
  : `fromList (u ++ v)` is equivalent to `fromList u <> fromList v`

FromList/Maybe
  : `fromListMaybe` is equivalent to `Just . fromList`

> class
>   ( FromList t
>   , forall a. (ElementOf t a) => Monoid (t a)
>   ) => FromListMonoid t
>   where
>     fromList :: (ElementOf t a) => [a] -> t a

If we have `Cons`, `Snoc`, and `Reverse`, we require some more laws:

ToList/Cons
  : `toList (cons a x)` is equivalent to `cons a (toList x)`

ToList/Snoc
  : `toList (snoc a x)` is equivalent to `snoc a (toList x)`

Cons/FromListMaybe
  : `fromListMaybe (cons a as)` is equivalent to `fmap (cons a) (fromListMaybe x)`

Snoc/FromListMaybe
  : `fromListMaybe (snoc a as)` is equivalent to `fmap (snoc a) (fromListMaybe x)`

ToList/Reverse
  : `toList (reverse x)` is equivalent to `reverse (toList x)`

FromListMaybe/Reverse
  : `fromListMaybe (reverse x)` is equivalent to `fmap reverse (fromListMaybe x)`

> class
>   ( FromList t
>   , Cons t, Snoc t, Reverse t
>   ) => FromListConsSnocReverse t

The `FromListMonoid` class has enough machinery to define generic test case generators.

> arbFromList
>   :: ( FromListMonoid t, ElementOf t a, Arb a )
>   => Sample (t a)
> arbFromList = genFromList arb
> 
> genFromList
>   :: ( FromListMonoid t, ElementOf t a )
>   => Sample a -> Sample (t a)
> genFromList gen = do
>   NonNegative (k :: Int) <- arb
>   fmap fromList $ join $ vectOf <$> pure k <*> pure gen
