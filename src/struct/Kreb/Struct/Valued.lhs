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






> data Tup
>   = Tup Int Int
>   deriving (Eq, Show)
> 
> instance Semigroup Tup where
>   (Tup a1 b1) <> (Tup a2 b2) =
>     if a2 > 0
>       then Tup (a1+a2) b2
>       else Tup a1 (b1+b2)
> 
> instance Monoid Tup where
>   mempty = Tup 0 0
> 
> instance (Valued Tup a) => Valued Tup [a] where
>   value = mconcat . map value
> 
> instance Valued Tup Bool where
>   value p = if p
>     then Tup 0 1
>     else Tup 1 0
> 
> instance Valued Tup Int where
>   value k = if 0 == rem k 2
>     then Tup 0 1
>     else Tup 1 0
> 
> instance Valued Tup (ZZ a) where
>   value (ZZ k) = if 0 == rem k 2
>     then Tup 0 1
>     else Tup 1 0
> 
> instance Arb Tup where
>   arb = do
>     NonNegative a <- arb
>     NonNegative b <- arb
>     return $ Tup a b
> 
> instance Prune Tup where
>   prune (Tup a b) =
>     [ Tup a c | c <- prune b ] ++
>     [ Tup c b | c <- prune a ]
> 
> instance CoArb Tup where
>   coarb (Tup a b) =
>     coarb (a,b)
> 
> instance MakeTo Tup where
>   makeTo = makeToExtendWith makeTo g h
>     where
>       g :: Tup -> (Int, Int)
>       g (Tup a b) = (a,b)
> 
>       h :: (Int, Int) -> Tup
>       h (a,b) = Tup a b
