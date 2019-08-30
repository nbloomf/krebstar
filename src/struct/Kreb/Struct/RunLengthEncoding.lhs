---
title: Kreb.Struct.RunLengthEncoding
---



Contents
--------

* [Introduction](#introduction)
* [Run Length Encoding](#run-length-encoding)
    * [Queries](#queries)
    * [Monoid instance](#monoid-instance)



Introduction
============

_Run length encoding_ is a basic form of compression for lists. Rather than storing each element of the list, we break the list into contiguous _runs_ of identical elements and then just store the number of times the element appears in the run. Calling this a compression format may seem like a stretch, but it can work well on data containing many such runs or runs of long length.

There's another way to think about run length encoding that I like. Recall that the list type constructor corresponds to the free monoid construction on an arbitrary set. With an associative operator we have the handy exponential notation for abbreviating products; rather than $aaa$ we can say $a^3$. But this is precisely run length encoding!

We will need this utility type later.

> {-# LANGUAGE
>     MultiParamTypeClasses
>   , InstanceSigs
> #-}
> 
> module Kreb.Struct.RunLengthEncoding (
>     RunLengthEncoding(..)
>   , RunSize(..)
>   , Run(..)
> 
>   -- * Constructors
>   , mkRun
>   , fromFreqList
> 
>   -- * Queries
>   , isEmptyRLE
>   , firstRun
>   , lastRun
>   , toFreqList
> ) where
> 
> import Data.Foldable
> import Data.List (intercalate)
> 
> import Kreb.Struct.FingerTree



Run Length Encoding
===================

We'll be using a finger tree to store a run length encoded list, and so we'll need an appropriate monoid annotation type. We call the monoid type `RunSize`, giving it two integer parameters: 'count' measures a number of runs, and 'length' measures the total number of items appearing in all runs.

> data RunSize = RunSize
>   { runCount  :: Int
>   , runLength :: Int
>   } deriving (Eq, Show)

This type can be made a monoid in the usual way.

> instance Semigroup RunSize where
>   (RunSize s1 l1) <> (RunSize s2 l2) =
>     RunSize (s1 + s2) (l1 + l2)
> 
> instance Monoid RunSize where
>   mempty = RunSize 0 0

Next we'll need a wrapper type to represent value/run length pairs.

> newtype Run a = Run
>   { unRun :: (Int, a)
>   } deriving (Eq, Show)
> 
> mkRun
>   :: Int -> a -> Run a
> mkRun k a =
>   if k <= 0
>     then error $ "mkRun: k must be positive, but got " ++ show k
>     else Run (k, a)
> 
> instance Functor Run where
>   fmap f (Run (k, a)) = Run (k, f a)
> 
> instance
>   Valued RunSize (Run a)
>   where
>     value (Run (k, _)) =
>       RunSize 1 k

We're finally prepared to defing run length encoded lists in terms of `RunSize` and `Run`.

> newtype RunLengthEncoding a = RLE
>   { unRLE :: FingerTree RunSize (Run a)
>   } deriving Eq
> 
> instance
>   Valued RunSize (RunLengthEncoding a)
>   where
>     value (RLE x) = value x
> 
> instance
>   Functor RunLengthEncoding
>   where
>     fmap f (RLE x) = RLE $ fmapFT (fmap f) x

It will be handy to view run length encoded lists as _frequency lists_.

> toFreqList
>   :: RunLengthEncoding a -> [(Int, a)]
> toFreqList (RLE x) = map unRun $ toList x

And we can give a `Foldable` instance for run length encoded lists.

> instance Foldable RunLengthEncoding where
>   foldr
>     :: (a -> b -> b) -> b -> RunLengthEncoding a -> b
>   foldr f e (RLE xs) =
>     let
>       rep g n a = if n <= 0
>         then a
>         else rep g (n-1) (g a)
>     in case unsnoc xs of
>       Nothing -> e
>       Just (Run (k, a), ys) ->
>         foldr f (rep (f a) k e) (RLE ys)



Queries
-------

> isEmptyRLE
>   :: RunLengthEncoding a -> Bool
> isEmptyRLE (RLE xs) = isEmptyFT xs

> firstRun
>   :: RunLengthEncoding a
>   -> Maybe (Run a, RunLengthEncoding a)
> firstRun (RLE x) =
>   case uncons x of
>     Nothing -> Nothing
>     Just (a, as) -> Just (a, RLE as)

> lastRun
>   :: RunLengthEncoding a
>   -> Maybe (Run a, RunLengthEncoding a)
> lastRun (RLE x) =
>   case unsnoc x of
>     Nothing -> Nothing
>     Just (a, as) -> Just (a, RLE as)



Monoid instance
---------------

The `Semigroup` product on run length encoded lists combines the innermost runs, to maintain the invariant that no two adjacent runs have the same value.

> instance
>   ( Eq a
>   ) => Semigroup (RunLengthEncoding a)
>   where
>     (RLE as) <> (RLE bs) =
>       case uncons bs of
>         Nothing -> RLE as
>         Just (Run (kb, b), bs') -> case unsnoc as of
>           Nothing -> RLE bs
>           Just (Run (ka, a), as') -> RLE $ mconcat $ if a == b
>             then [ as', fromListFT [ Run (ka+kb, a) ], bs' ]
>             else [ as, bs ]
> 
> instance
>   ( Eq a
>   ) => Monoid (RunLengthEncoding a)
>   where
>     mempty = RLE mempty

With the semigroup instance in hand we can define a left inverse for `toFreqList`.

> fromFreqList
>   :: ( Eq a )
>   => [(Int, a)] -> RunLengthEncoding a
> fromFreqList = foldr f mempty . filter p
>   where
>     f (k,a) xs =
>       (RLE $ fromListFT [mkRun k a]) <> xs
> 
>     p (k,_) = k > 0

And with `fromFreqList` in hand we can give a handy `Show` instance.

> instance
>   ( Show a
>   ) => Show (RunLengthEncoding a)
>   where
>     show xs =
>       let
>         toPairs :: RunLengthEncoding a -> [(Int, a)]
>         toPairs z = case firstRun z of
>           Nothing -> []
>           Just (Run a, ds) -> a : toPairs ds
>       in concat
>         [ "(fromFreqList ["
>         , intercalate "," $ map show $ toPairs xs
>         , "])"
>         ]
