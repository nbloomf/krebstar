---
title: Run Length Encoding
---

::: contents
* [List Compression](#list-compression): Saying more with less
* [Auxiliary Types](#auxiliary-types): We need a couple
* [Run Length Encoding](#run-length-encoding): The main event
* [Queries](#queries): Extracting value from a list
* [Cons and Concat](#cons-and-concat): List operations
* [Testing and Debugging](#testing-and-debugging): For when things go wrong
:::



::: frontmatter

> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE InstanceSigs #-}
> 
> module Kreb.Struct.RunLengthEncoded (
>     RunSize()
>   , Run(unRun)
>   , mkRun
> 
>   , RunLengthEncoded()
>   , fromRuns
>   , empty
>   , singleton
>   , toRuns
>   , fromList
> 
>   , isEmpty
>   , countItems
>   , countRuns
>   , isSingleton
>   , firstRun
>   , lastRun
> 
>   , cons
>   , uncons
>   , snoc
>   , unsnoc
> 
>   , validate
> ) where
> 
> import Data.Foldable
> import Data.List (intercalate, genericLength)
> 
> import Kreb.Check
> 
> import           Kreb.Struct.Valued
> import qualified Kreb.Struct.FingerTree as FT

:::



List Compression
----------------

Recall that with finger trees we have a list-like data structure that supports efficient concatenation and search. We will be using that structure in a few different capacities, one of which is a little strange. For reasons we'll see later, it will be very useful to work with lists whose entries come from a very small set of only four possible values which moreover appear in long _runs_ of many identical items. In this situation we can save a lot of space by keeping track of the runs rather than the individual items.

For example, consider a list consisting of five of one item followed by three of another.

::: example
```haskell
['a','a','a','a','a','b','b','b']
```
:::

This list could be stored instead as

::: example
```haskell
[('a',5),('b',3)]
```
:::

with no loss of information. The second representation will use less space as long as the ratio of long runs to short runs in the data is large enough. This way of compressing lists is called _run length encoding_, because we're doing exactly that -- encoding the run lengths.

There's another way to think about run length encoding that I particularly like. Recall that the list type constructor corresponds to the free monoid construction on an arbitrary set. With an associative operator we have the handy exponential notation for abbreviating products; rather than $aaa$ we can say $a^3$. But this is precisely run length encoding!



Auxiliary Types
---------------

Run length encoded lists are still lists, they just keep some extra information at each index. As such they can be constructed using any type isomorphic to lists. We've got a particularly nice list-like type on hand -- finger trees -- so we'll use that.

Recall that to define a concrete finger tree type we need an appropriate annotation monoid to represent the 'measure' of both items in the list and the list as a whole. A natural choice for this is a count of the number of items in the list, as we've been using with the `Count` value type so far. But the structure of run length encoded lists offers another natural measurement, namely the number of runs. We can keep track of both with the `RunSize` type.

> data RunSize = RunSize
>   { runCount  :: Integer -- number of runs
>   , runLength :: Integer -- number of items in all runs
>   } deriving (Eq, Show)

Before moving on we should note some constraints on `RunSize`. Internally it keeps track of two integers, which may be negative. But these integers each represent a _size_, and it doesn't make sense for them to be negative. Consumers of this module won't need to worry about this but we need to keep it in mind.

We'll make `RunSize` an additive monoid: the total length of two runs put together is the sum of their individual lengths.

> instance Semigroup RunSize where
>   (RunSize s1 l1) <> (RunSize s2 l2) =
>     RunSize (s1 + s2) (l1 + l2)
> 
> instance Monoid RunSize where
>   mempty = RunSize 0 0

Next we'll need a wrapper type to represent value/run length pairs.

> newtype Run a = Run
>   { unRun :: (Integer, a)
>   } deriving (Eq, Show)

Again, to maintain the invariant that the run length must be positive we expose a smart constructor and destructor. We could in principle allow for length zero runs, but this would mean our lists no longer have a canonical representation and that any given list can have representations of arbitrarily large size.

> mkRun
>   :: Integer -> a -> Run a
> mkRun k a =
>   if k <= 0
>     then error $ concat
>       [ "mkRun: run length must be positive, "
>       , "but got ", show k ]
>     else Run (k, a)

Finally we need some class instances for `Run`.

> instance
>   Valued RunSize (Run a)
>   where
>     value (Run (k, _)) =
>       RunSize 1 k
> 
> instance Functor Run where
>   fmap f (Run (k, a)) = Run (k, f a)



Run Length Encoding
-------------------

We're now prepared to define run length encoded lists in terms of `RunSize` and `Run`.

> newtype RunLengthEncoded a = RLE
>   { unRLE :: FT.FingerTree RunSize (Run a)
>   } deriving Eq
> 
> instance
>   Valued RunSize (RunLengthEncoded a)
>   where
>     value (RLE x) = value x
> 
> instance
>   Functor RunLengthEncoded
>   where
>     fmap f (RLE x) = RLE $ FT.fmapFT (fmap f) x

We'd like for our internal representation to maintain an additional invariant: that two adjacent runs always have distinct 'bases'. For instance, using list notation, a representation like

::: example
```haskell
[('a',5),('a',2),('b',9)]
```
:::

could be written instead as

::: example
```haskell
[('a',7),('b',9)]
```
:::

with no loss of information. Our `RunLengthEncoded` type can't enforce this on its own, so we'll have to do it using a smart constructor.

> fromRuns
>   :: ( Eq a )
>   => [(Integer, a)] -> RunLengthEncoded a
> fromRuns = foldr f mempty . filter nontrivial . combine
>   where
>     f (k,a) xs =
>       (RLE $ FT.fromList [mkRun k a]) <> xs
> 
>     combine xs = case xs of
>       (k1, a1) : (k2, a2) : rest -> if a1 == a2
>         then combine ((k1 + k2, a1) : rest)
>         else (k1, a1) : combine ((k2, a2) : rest)
>       _ -> xs
> 
>     nontrivial (k,_) = k /= 0

From here we can also define the basic list-like constructors: `empty` and `singleton`.

> empty
>   :: ( Eq a )
>   => RunLengthEncoded a
> empty = fromRuns []
> 
> singleton
>   :: ( Eq a )
>   => a -> RunLengthEncoded a
> singleton a = fromRuns [(1, a)]

We'll also convert run length encoded lists back into ordinary lists of runs; this is mostly helpful for testing.

> toRuns
>   :: RunLengthEncoded a -> [(Integer, a)]
> toRuns (RLE x) = map unRun $ toList x

This is a good place to stop and see some round trip examples.

::: doctest

> -- $
> -- >>> toRuns $ fromRuns [(5,'a'),(2,'a'),(4,'b')]
> -- [(7,'a'),(4,'b')]
> --
> -- >>> toRuns $ fromRuns [(0,'a'),(1,'b'),(0,'c')]
> -- [(1,'b')]
> --
> -- >>> toRuns $ fromRuns [(0,'a'),(0,'a'),(0,'a')]
> -- []

:::

We also have a `Foldable` instance for run length encoded lists.

> instance Foldable RunLengthEncoded where
>   foldr
>     :: (a -> b -> b) -> b -> RunLengthEncoded a -> b
>   foldr f e (RLE xs) =
>     let
>       rep g n a = if n <= 0
>         then a
>         else rep g (n-1) (g a)
>     in case FT.unsnoc xs of
>       Nothing -> e
>       Just (Run (k, a), ys) ->
>         foldr f (rep (f a) k e) (RLE ys)

Run length encoded lists are isomorphic to cons lists, which we witness with explicit mappings between the two.

> fromList
>   :: ( Eq a )
>   => [a] -> RunLengthEncoded a
> fromList = fromRuns . group
>   where
>     group :: (Eq a) => [a] -> [(Integer, a)]
>     group xs = case xs of
>       [] -> []
>       a:as ->
>         let (us, rest) = span (== a) as
>         in (1 + genericLength us, a) : group rest

Note that we get the analogous `toList` function for free with `Foldable`. Here are some examples:

::: doctest

> -- $
> -- >>> toRuns (fromList ['a','a','b','c','c','c'])
> -- [(2,'a'),(1,'b'),(3,'c')]
> --
> -- >>> toList (fromRuns [(4,'a'),(3,'b')])
> -- "aaaabbb"

:::



Queries
-------

Because run length encoded lists are isomorphic to lists, the usual list operations make sense here too. We can detect when a list is empty:

> isEmpty
>   :: RunLengthEncoded a -> Bool
> isEmpty (RLE xs) = FT.isEmpty xs

Detecting when the list is a singleton is a little more involved. The simplest way to do it is to first find the number of items in the list, and see if that number is 1. Fortunately, since the underlying structure is a finger tree, this is already done.

> countItems
>   :: RunLengthEncoded a -> Integer
> countItems = runLength . value
> 
> countRuns
>   :: RunLengthEncoded a -> Integer
> countRuns = runCount . value
> 
> isSingleton
>   :: RunLengthEncoded a -> Bool
> isSingleton as = 1 == countItems as

It will also be useful to extract the first and last run of the list.

> firstRun
>   :: RunLengthEncoded a
>   -> Maybe (Run a, RunLengthEncoded a)
> firstRun (RLE x) =
>   case FT.uncons x of
>     Nothing -> Nothing
>     Just (a, as) -> Just (a, RLE as)
> 
> lastRun
>   :: RunLengthEncoded a
>   -> Maybe (Run a, RunLengthEncoded a)
> lastRun (RLE x) =
>   case FT.unsnoc x of
>     Nothing -> Nothing
>     Just (a, as) -> Just (a, RLE as)



Cons and Concat
---------------

Cons and uncons are a little more complicated than on ordinary lists.

> cons
>  :: ( Eq a )
>  => a -> RunLengthEncoded a -> RunLengthEncoded a
> cons a (RLE xs) = case FT.uncons xs of
>   Nothing -> singleton a
>   Just (Run (k, b), zs) -> if a == b
>     then RLE $ FT.cons (Run (1+k, a)) zs
>     else RLE $ FT.cons (Run (1, a)) xs
> 
> uncons
>   :: ( Eq a )
>   => RunLengthEncoded a -> Maybe (a, RunLengthEncoded a)
> uncons (RLE xs) = case FT.uncons xs of
>   Nothing -> Nothing
>   Just (Run (k, a), as) -> if k == 1
>     then Just (a, RLE as)
>     else Just (a, RLE $ FT.cons (Run (k-1, a)) as)

Likewise `snoc` and `unsnoc`.

> snoc
>   :: ( Eq a )
>   => a -> RunLengthEncoded a -> RunLengthEncoded a
> snoc a (RLE xs) = case FT.unsnoc xs of
>   Nothing -> singleton a
>   Just (Run (k, b), zs) -> if a == b
>     then RLE $ FT.snoc (Run (1+k, a)) zs
>     else RLE $ FT.snoc (Run (1, a)) xs
> 
> unsnoc
>   :: ( Eq a )
>   => RunLengthEncoded a -> Maybe (a, RunLengthEncoded a)
> unsnoc (RLE xs) = case FT.unsnoc xs of
>   Nothing -> Nothing
>   Just (Run (k, a), as) -> if k == 1
>     then Just (a, RLE as)
>     else Just (a, RLE $ FT.snoc (Run (k-1, a)) as)

When concatenating run length encoded lists, we need to be careful to combine the innermost runs to maintain the invariant that no two adjacent runs have the same base.

> instance
>   ( Eq a
>   ) => Semigroup (RunLengthEncoded a)
>   where
>     (RLE as) <> (RLE bs) =
>       case FT.uncons bs of
>         Nothing -> RLE as
>         Just (Run (kb, b), bs') -> case FT.unsnoc as of
>           Nothing -> RLE bs
>           Just (Run (ka, a), as') -> RLE $ mconcat $ if a == b
>             then [ as', FT.fromList [ Run (ka + kb, a) ], bs' ]
>             else [ as, bs ]
> 
> instance
>   ( Eq a
>   ) => Monoid (RunLengthEncoded a)
>   where
>     mempty = RLE mempty



Testing and Debugging
---------------------

I almost forgot to define a `Show` instance. This uses the run list representation so that rendered strings are valid Haskell code.

> instance
>   ( Show a
>   ) => Show (RunLengthEncoded a)
>   where
>     show xs = concat
>       [ "fromRuns "
>       , show (toRuns xs)
>       ]

And finally, we need some class instances to interact with the testing framework.

> instance Arb RunSize where
>   arb = do
>     NonNegative s <- arb
>     NonNegative l <- arb
>     return $ RunSize s l
> 
> instance Prune RunSize where
>   prune (RunSize a b) =
>     [ RunSize a c | c <- map abs $ prune b ] ++
>     [ RunSize c b | c <- map abs $ prune a ]
> 
> instance
>   ( Arb a, Eq a
>   ) => Arb (Run a)
>   where
>     arb = do
>       Positive k <- arb
>       a <- arb
>       return (mkRun k a)
> 
> instance
>   ( Prune a, Eq a
>   ) => Prune (Run a)
>   where
>     prune (Run z) = do
>       (k', a') <- prune z
>       return $ mkRun (abs k') a'
> 
> instance
>   ( Arb a, Eq a
>   ) => Arb (RunLengthEncoded a)
>   where
>     arb = do
>       xs <- listOf arb
>       return $ fromRuns $ map unRun xs
> 
> instance
>   ( Prune a, Eq a
>   ) => Prune (RunLengthEncoded a)
>   where
>     prune = map fromRuns . filter (/= []) . prune . toRuns

And because run length encoded lists have an essential invariant, we should expose a predicate to make sure the invariant is satisfied.

> validate
>   :: ( Eq a )
>   => RunLengthEncoded a -> Bool
> validate = and . map ((> 0) . fst) . toRuns
