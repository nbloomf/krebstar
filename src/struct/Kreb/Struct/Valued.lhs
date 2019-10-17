> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE DeriveGeneric #-}
> 
> module Kreb.Struct.Valued where
> 
> import GHC.Generics
> 
> import Kreb.Check


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
> instance Valued Count Bool where
>   value _ = Count 1
> 
> instance Valued Count Int where
>   value _ = Count 1
> 
> instance Valued Count Integer where
>   value _ = Count 1
> 
> instance Valued Count (a,b) where
>   value _ = Count 1
> 
> instance Valued Count (a,b,c) where
>   value _ = Count 1
> 
> instance Valued Count (Either a b) where
>   value _ = Count 1
> 
> instance Valued Count (ZZ a) where
>   value _ = Count 1
> 
> instance Valued Count [a] where
>   value _ = Count 1
