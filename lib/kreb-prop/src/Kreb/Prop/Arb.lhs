---
title: Generatable Types
author: nbloomf
---

::: frontmatter

> module Kreb.Prop.Arb where
> 
> import Data.Proxy
> import Data.Void
> import Data.Char (ord)
> import Data.Monoid
> import System.Random (newStdGen)
> 
> import qualified Data.Map as M
> 
> import Kreb.Control
> import Kreb.Format
> 
> import Kreb.Prop.Sample

:::



Introduction
------------

The `Sample` monad gives us a context for generating random data. What we need now is a class of types which can be generated. This is `Arb`:

> class Arb t where
>   arb :: Sample t

Related, for reasons we'll see later, we need a class of types which can alter the distribution of some other type. (Hint: it's how we generate functions.) This is `CoArb`:

> class CoArb t where
>   coarb :: t -> Sample u -> Sample u

Finally, it will be useful to have a class for types whose inhabitants can be "shrunk" in a meaningful sense. This is `Prune`. The `prune` function should return a list of _smaller_ but related values, or an empty list if this can't be done. In particular, iterating `prune` on a given value should eventually terminate. It's difficult to enforce this invariant with either laws or types, so we'll just need to be careful.

> class Prune t where
>   prune :: t -> [t]



Instances
---------

This part is kind of boring. To avoid orphan instances we have to define a ton of instances here.

`Void`: we cannot generate arbitrary values of type `Void`, but we can give it a `CoArb` and `Prune` instance.

> instance CoArb Void where
>   coarb _ = id
> 
> instance Prune Void where
>   prune _ = []

The unit type is boring but important.

> instance Arb () where
>   arb = return ()
> 
> instance CoArb () where
>   coarb _ = id
> 
> instance Prune () where
>   prune _ = []

`Bool` has the first interesting `CoArb` instance, and illustrates the basic structure of `coarb` -- use the internal structure of a value to pass different integers to `twiddle`.

> instance Arb Bool where
>   arb = pickFrom2
>     ( return True
>     , return False
>     )
> 
> instance CoArb Bool where
>   coarb p =
>     if p
>       then twiddle 0
>       else twiddle 1
> 
> instance Prune Bool where
>   prune _ = []

`Int` has the first interesting `Prune` instance. In this case interpreting what _smaller_ means is pretty straightforward; pruning a number moves closer to zero. Also note that pruning should generally be aggressive, shrinking as much as possible at first, and more subtly later in the list. This is because of how pruning will be used to shrink failing test cases -- we want to converge on a _minimal_ failing test case as quickly as possible.

> instance Arb Int where
>   arb = do
>     k <- size
>     randIn (-(5*k), 5*k)
> 
> instance CoArb Int where
>   coarb = twiddle
> 
> instance Prune Int where
>   prune k = case compare k 0 of
>     LT -> filter (> k) [0, -1, k`div`2, k+1]
>     EQ -> []
>     GT -> filter (< k) [0, 1, k`div`2, k-1]

`Integer`:

> instance Arb Integer where
>   arb = do
>     n <- size
>     let m = 5 * toInteger n
>     randIn (-m, m)
> 
> instance CoArb Integer where
>   coarb = twiddle
> 
> instance Prune Integer where
>   prune k = case compare k 0 of
>     LT -> filter (> k) [0, -1, k`div`2, k+1]
>     EQ -> []
>     GT -> filter (< k) [0, 1, k`div`2, k-1]

`Char` is biased toward generating ascii.

> instance Arb Char where
>   arb = freq
>     [ (4, arbAsciiChar)
>     , (1, arbUnicodeChar)
>     ]
> 
> instance CoArb Char where
>   coarb c = coarb (ord c)
> 
> instance Prune Char where
>   prune c = if c == 'a' then [] else ['a']

`Maybe` is our first type constructor. Note what's going on with `CoArb`.

> instance
>   ( Arb a
>   ) => Arb (Maybe a)
>   where
>     arb = freq
>       [ (1, return Nothing)
>       , (5, Just <$> arb)
>       ]
> 
> instance
>   ( CoArb a
>   ) => CoArb (Maybe a)
>   where
>     coarb x = case x of
>       Nothing -> twiddle 0
>       Just a  -> twiddle 1 . coarb a
> 
> instance
>   ( Prune a
>   ) => Prune (Maybe a)
>   where
>     prune x = case x of
>       Nothing -> []
>       Just a -> Nothing : (fmap Just $ prune a)

`Either`:

> instance (Arb a, Arb b) => Arb (Either a b) where
>   arb = freq
>     [ (1, fmap Left arb)
>     , (1, fmap Right arb)
>     ]
> 
> instance (CoArb a, CoArb b) => CoArb (Either a b) where
>   coarb x = case x of
>     Left a  -> twiddle 0 . coarb a
>     Right b -> twiddle 1 . coarb b
> 
> instance (Prune a, Prune b) => Prune (Either a b) where
>   prune x = case x of
>     Left a  -> fmap Left $ prune a
>     Right b -> fmap Right $ prune b

Tuples are super interesting and not at all repetitive. Note what's happening with `prune`: we are careful to shrink only one entry in the tuple at each step.

> instance
>   ( Arb a1, Arb a2
>   ) => Arb (a1, a2)
>   where
>     arb = do
>       a1 <- arb
>       a2 <- arb
>       return (a1, a2)
> 
> instance
>   ( Prune a1, Prune a2
>   ) => Prune (a1, a2)
>   where
>     prune (a1,a2) = concat
>       [ [ (u1,a2) | u1 <- prune a1 ]
>       , [ (a1,u2) | u2 <- prune a2 ]
>       ]
> 
> instance
>   ( CoArb a1, CoArb a2
>   ) => CoArb (a1,a2)
>   where
>     coarb (a1,a2) =
>       coarb a1 .
>       coarb a2
> 
> instance
>   ( Arb a1, Arb a2, Arb a3
>   ) => Arb (a1, a2, a3)
>   where
>     arb = do
>       a1 <- arb
>       a2 <- arb
>       a3 <- arb
>       return (a1, a2, a3)
> 
> instance
>   ( Prune a1, Prune a2, Prune a3
>   ) => Prune (a1, a2, a3)
>   where
>     prune (a1,a2,a3) = concat
>       [ [ (u1,a2,a3) | u1 <- prune a1 ]
>       , [ (a1,u2,a3) | u2 <- prune a2 ]
>       , [ (a1,a2,u3) | u3 <- prune a3 ]
>       ]
> 
> instance
>   ( CoArb a1, CoArb a2, CoArb a3
>   ) => CoArb (a1,a2,a3)
>   where
>     coarb (a1,a2,a3) =
>       coarb a1 .
>       coarb a2 .
>       coarb a3
> 
> instance
>   ( Arb a1, Arb a2, Arb a3, Arb a4
>   ) => Arb (a1, a2, a3, a4)
>   where
>     arb = do
>       a1 <- arb
>       a2 <- arb
>       a3 <- arb
>       a4 <- arb
>       return (a1, a2, a3, a4)
> 
> instance
>   ( Prune a1, Prune a2, Prune a3, Prune a4
>   ) => Prune (a1, a2, a3, a4)
>   where
>     prune (a1,a2,a3,a4) = concat
>       [ [ (u1,a2,a3,a4) | u1 <- prune a1 ]
>       , [ (a1,u2,a3,a4) | u2 <- prune a2 ]
>       , [ (a1,a2,u3,a4) | u3 <- prune a3 ]
>       , [ (a1,a2,a3,u4) | u4 <- prune a4 ]
>       ]
> 
> instance
>   ( CoArb a1, CoArb a2, CoArb a3, CoArb a4
>   ) => CoArb (a1,a2,a3,a4)
>   where
>     coarb (a1,a2,a3,a4) =
>       coarb a1 .
>       coarb a2 .
>       coarb a3 .
>       coarb a4
> 
> instance
>   ( Arb a1, Arb a2, Arb a3, Arb a4, Arb a5
>   ) => Arb (a1, a2, a3, a4, a5)
>   where
>     arb = do
>       a1 <- arb
>       a2 <- arb
>       a3 <- arb
>       a4 <- arb
>       a5 <- arb
>       return (a1, a2, a3, a4, a5)
> 
> instance
>   ( Prune a1, Prune a2, Prune a3, Prune a4, Prune a5
>   ) => Prune (a1, a2, a3, a4, a5)
>   where
>     prune (a1,a2,a3,a4,a5) = concat
>       [ [ (u1,a2,a3,a4,a5) | u1 <- prune a1 ]
>       , [ (a1,u2,a3,a4,a5) | u2 <- prune a2 ]
>       , [ (a1,a2,u3,a4,a5) | u3 <- prune a3 ]
>       , [ (a1,a2,a3,u4,a5) | u4 <- prune a4 ]
>       , [ (a1,a2,a3,a4,u5) | u5 <- prune a5 ]
>       ]
> 
> instance
>   ( CoArb a1, CoArb a2, CoArb a3, CoArb a4, CoArb a5
>   ) => CoArb (a1,a2,a3,a4,a5)
>   where
>     coarb (a1,a2,a3,a4,a5) =
>       coarb a1 .
>       coarb a2 .
>       coarb a3 .
>       coarb a4 .
>       coarb a5

Lists:

> instance
>   ( Arb a
>   ) => Arb [a]
>   where
>     arb = nestOf arb
> 
> instance
>   ( Prune a
>   ) => Prune [a]
>   where
>     prune z = case z of
>       [] -> []
>       x:xs -> concat
>        [ [[]]
>        , if null xs then [] else [[x], xs]
>        , map (x:) (prune xs)
>        , map (:xs) (prune x)
>        ]
> 
> instance
>   ( CoArb a
>   ) => CoArb [a]
>   where
>     coarb z = case z of
>       []   -> twiddle 0
>       x:xs -> twiddle 1 . coarb (x,xs)

We have `Arb` and `CoArb` instances for function types, but not prune. There's a really clever way around this in the paper [Shrinking and showing functions](https://dl.acm.org/doi/10.1145/2364506.2364516) by Koen Claessen, which we'll build out later.

> instance
>   ( CoArb a, Arb b
>   ) => Arb (a -> b)
>   where
>     arb = promoteSample $ \a -> coarb a arb
> 
> instance
>   ( Arb a, CoArb b
>   ) => CoArb (a -> b)
>   where
>     coarb f gen = do
>       xs <- arb
>       coarb (map f xs) gen

Now we're getting to the non-prelude standard library types. Map:

> instance
>   ( Arb k, Arb a, Ord k
>   ) => Arb (M.Map k a)
>   where
>     arb = M.fromList <$> arb
> 
> instance
>   ( Prune k, Prune a, Ord k
>   ) => Prune (M.Map k a)
>   where
>     prune =
>       map M.fromList . prune . M.toList

The wrapper types from `Monoid`:

> instance (Arb a) => Arb (Sum a) where
>   arb = Sum <$> arb
> 
> instance (Prune a) => Prune (Sum a) where
>   prune (Sum a) = fmap Sum $ prune a
> 
> instance (CoArb a) => CoArb (Sum a) where
>   coarb (Sum a) = coarb a

Now we get to the data types defined in `kreb-control`.

> instance ( Arb a ) => Arb (Identity a) where
>   arb = fmap Identity arb
> 
> instance ( CoArb a ) => CoArb (Identity a) where
>   coarb (Identity a) = coarb a
> 
> instance ( Prune a ) => Prune (Identity a) where
>   prune = map Identity . prune . unIdentity
> 
> instance
>   ( Arb (f (g a))
>   ) => Arb (Compose f g a)
>   where
>     arb = fmap Compose arb
> 
> instance
>   ( CoArb (f (g a))
>   ) => CoArb (Compose f g a)
>   where
>     coarb (Compose x) = coarb x
> 
> instance
>   ( Prune (f (g a))
>   ) => Prune (Compose f g a)
>   where
>     prune = fmap Compose . prune . unCompose
