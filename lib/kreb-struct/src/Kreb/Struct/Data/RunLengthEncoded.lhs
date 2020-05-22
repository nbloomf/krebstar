---
title: Run Length Encoding
---

::: contents
* [List Compression](#list-compression): Saying more with less
* [Auxiliary Types](#auxiliary-types): We need a couple
* [Run Length Encoding](#run-length-encoding): The main event
* [Cons, Snoc, and Concat](#cons-snoc-and-concat): List operations
* [Testing and Debugging](#testing-and-debugging): For when things go wrong
:::



::: frontmatter

> {-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

> module Kreb.Struct.Data.RunLengthEncoded (
>     RunLengthEncoded()
>   , NonEmptyRunLengthEncoded()
> 
>   , Run(..)
>   , RunSize()
> 
>   , countRuns
>   , countItems
> 
>   , mkRun
>   , FromRuns(..)
>   , firstRun
>   , lastRun
>   , firstRunNonEmpty
>   , lastRunNonEmpty
> 
>   , validateRunLengthEncoded
>   , validateNonEmptyRunLengthEncoded
> ) where

> import Prelude hiding (reverse)
> import Data.Foldable
> import Data.List (intercalate, genericLength)

> import qualified Kreb.Format as Fmt
> import           Kreb.Format (display, (<+>))
> import           Kreb.Control
> import           Kreb.Control.Constrained
> import           Kreb.Prop

> import Kreb.Struct.Class
> import qualified Kreb.Struct.Data.FingerTree as FT

:::



List Compression
----------------

Recall that with finger trees we have a list-like data structure that supports efficient concatenation and search. We will be using that structure in a few different capacities, one of which is a little strange. For reasons we'll see later, it will be very useful to work with lists whose entries come from a very small set of possible values which moreover appear in long _runs_ of many identical items. In this situation we can save a lot of space by keeping track of the runs rather than the individual items.

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
>   } deriving Eq

Again, to maintain the invariant that the run length must be positive we expose a smart constructor and destructor. We could in principle allow for length zero runs, but this would mean our lists no longer have a canonical representation and that any given list can have representations of arbitrarily large size.

> mkRun
>   :: Integer -> a -> Run a
> mkRun k a =
>   if k <= 0
>     then error $ concat
>       [ "mkRun: run length must be positive, "
>       , "but got ", show k ]
>     else Run (k, a)
> 
> instance (Show a) => Show (Run a) where
>   show (Run (k,a)) = concat
>     [ "mkRun", show k, show a ]

Finally we need some class instances for `Run`.

> instance Valued (Run a) where
>   type Value (Run a) = RunSize
> 
>   value :: Run a -> Value (Run a)
>   value (Run (k, _)) =
>     RunSize 1 k
> 
> instance Functor Run where
>   fmap f (Run (k, a)) = Run (k, f a)



Run Length Encoding
-------------------

We're now prepared to define run length encoded lists in terms of `RunSize` and `Run`. As with finger trees, we define possibly empty and nonempty variants.

> data RunLengthEncoded a
>   = Empty
>   | NonEmpty (NonEmptyRunLengthEncoded a)
>   deriving Eq
> 
> instance Container RunLengthEncoded where
>   type ContainerConstraint RunLengthEncoded = Eq
> 
> newtype NonEmptyRunLengthEncoded a = RLE
>   { unRLE :: FT.NonEmptyFingerTree (Run a)
>   } deriving Eq
> 
> instance Container NonEmptyRunLengthEncoded where
>   type ContainerConstraint NonEmptyRunLengthEncoded = Eq

Both variants are instances of `Valued`:

> instance Valued (RunLengthEncoded a) where
>   type Value (RunLengthEncoded a) = RunSize
> 
>   value
>     :: RunLengthEncoded a
>     -> Value (NonEmptyRunLengthEncoded a)
>   value x = case x of
>     Empty -> mempty
>     NonEmpty w -> value w
> 
> instance Valued (NonEmptyRunLengthEncoded a) where
>   type Value (NonEmptyRunLengthEncoded a) = RunSize
> 
>   value
>     :: NonEmptyRunLengthEncoded a
>     -> Value (NonEmptyRunLengthEncoded a)
>   value (RLE x) = value x

Nonempty lists are a "subset" of possibly empty lists:

> instance Subset NonEmptyRunLengthEncoded where
>   type SupersetOf NonEmptyRunLengthEncoded = RunLengthEncoded
> 
>   inject
>     :: ( Eq a )
>     => NonEmptyRunLengthEncoded a -> RunLengthEncoded a
>   inject = NonEmpty
> 
>   restrict
>     :: ( Eq a )
>     => RunLengthEncoded a -> Maybe (NonEmptyRunLengthEncoded a)
>   restrict x = case x of
>     Empty -> Nothing
>     NonEmpty w -> Just w

and is an instance of `NonEmpty`.

> instance NonEmpty NonEmptyRunLengthEncoded where
>   empty
>     :: ( Eq a )
>     => RunLengthEncoded a
>   empty = Empty
> 
>   isEmpty
>     :: ( Eq a )
>     => RunLengthEncoded a -> Bool
>   isEmpty x = case x of
>     Empty      -> True
>     NonEmpty _ -> False

Both variants are functors (proper functors, since the `Eq` constraint doesn't appear on the constructors).

> instance Functor RunLengthEncoded where
>   fmap f x = case x of
>     Empty -> Empty
>     NonEmpty w -> NonEmpty (fmap f w)
> 
> instance Functor NonEmptyRunLengthEncoded where
>   fmap f (RLE x) = RLE $ fmapC (fmap f) x

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

with no loss of information. Our `NonEmptyRunLengthEncoded` type can't enforce this on its own, so we'll have to do it using a smart constructor.

> class FromRuns t where
>   fromRuns :: (Eq a) => [(Integer, a)] -> t a
>   toRuns :: t a -> [(Integer, a)]
> 
>   showRuns :: (Show a) => t a -> String
>   showRuns xs = concat
>     [ "fromRuns ", show (toRuns xs) ]
> 
> instance FromRuns RunLengthEncoded where
>   fromRuns :: (Eq a) => [(Integer, a)] -> RunLengthEncoded a
>   fromRuns xs = case combineAndFilter xs of
>     [] -> Empty
>     _  -> NonEmpty $ fromRuns xs
> 
>   toRuns :: RunLengthEncoded a -> [(Integer, a)]
>   toRuns x = case x of
>     Empty -> []
>     NonEmpty w -> toRuns w
> 
> instance (Show a) => Show (RunLengthEncoded a) where
>   show = showRuns
> 
> instance FromRuns NonEmptyRunLengthEncoded where
>   fromRuns :: (Eq a) => [(Integer, a)] -> NonEmptyRunLengthEncoded a
>   fromRuns xs = case combineAndFilter xs of
>     [] -> error "NonEmptyRunLengthEncoded: fromRuns panic (empty list)"
>     ys -> RLE $ fromList $ map (uncurry mkRun) ys
> 
>   toRuns :: NonEmptyRunLengthEncoded a -> [(Integer, a)]
>   toRuns (RLE x) = map unRun $ toList x
> 
> combineAndFilter
>   :: ( Eq a )
>   => [(Integer, a)] -> [(Integer, a)]
> combineAndFilter = filter nontrivial . combine
>   where
>     combine xs = case xs of
>       (k1, a1) : (k2, a2) : rest -> if a1 == a2
>         then combine ((k1 + k2, a1) : rest)
>         else (k1, a1) : combine ((k2, a2) : rest)
>       _ -> xs
> 
>     nontrivial (k,_) = k /= 0
> 
> instance (Show a) => Show (NonEmptyRunLengthEncoded a) where
>   show = showRuns

Detecting when a list is a singleton is a little more involved. The simplest way to do it is to first find the number of items in the list, and see if that number is 1. Fortunately, since the underlying structure is a finger tree, this work already done. First we define helper functions for finding the number of _items_ in the list, as well as the number of _runs_:

> class RunLengthEncodedSize t where
>   countItems :: t a -> Integer
>   countRuns  :: t a -> Integer
> 
> instance RunLengthEncodedSize RunLengthEncoded where
>   countItems :: RunLengthEncoded a -> Integer
>   countItems x = case x of
>     Empty      -> 0
>     NonEmpty w -> countItems w
> 
>   countRuns :: RunLengthEncoded a -> Integer
>   countRuns x = case x of
>     Empty      -> 0
>     NonEmpty w -> countRuns w
> 
> instance RunLengthEncodedSize NonEmptyRunLengthEncoded where
>   countItems :: NonEmptyRunLengthEncoded a -> Integer
>   countItems = runLength . value
> 
>   countRuns :: NonEmptyRunLengthEncoded a -> Integer
>   countRuns = runCount . value

We defined these using a class so we can use the same names for both list variants. From here we can also define `singleton` lists:

> instance Singleton RunLengthEncoded where
>   singleton :: (Eq a) => a -> RunLengthEncoded a
>   singleton = NonEmpty . singleton
> 
>   isSingleton :: (Eq a) => RunLengthEncoded a -> Bool
>   isSingleton x = case x of
>     Empty -> False
>     NonEmpty w -> isSingleton w
> 
> instance Singleton NonEmptyRunLengthEncoded where
>   singleton :: (Eq a) => a -> NonEmptyRunLengthEncoded a
>   singleton a = fromRuns [(1, a)]
> 
>   isSingleton :: NonEmptyRunLengthEncoded a -> Bool
>   isSingleton as = 1 == countItems as
> 
> instance SubsetSingleton NonEmptyRunLengthEncoded
> instance NonEmptySingleton NonEmptyRunLengthEncoded

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
>   foldr f e x = case x of
>     Empty -> e
>     NonEmpty w -> foldr f e w
> 
> instance Foldable NonEmptyRunLengthEncoded where
>   foldr
>     :: forall a b
>      . (a -> b -> b) -> b -> NonEmptyRunLengthEncoded a -> b
>   foldr f e (RLE xs) =
>     let
>       rep :: Run a -> b -> b
>       rep (Run (n,a)) c = if n <= 0
>         then c
>         else rep (Run (n-1, a)) (f a c)
> 
>       (w, r) = unsnocNonEmpty xs
>     in case r of
>       FT.Empty -> rep w e
>       FT.NonEmpty z -> foldr rep (rep w e) z

Run length encoded lists are isomorphic to cons lists, which we witness with explicit mappings between the two.

> instance FromList RunLengthEncoded where
>   fromList :: (Eq a) => [a] -> RunLengthEncoded a
>   fromList xs = case xs of
>     [] -> Empty
>     _  -> NonEmpty $ fromList xs
> 
> instance FromListMonoid RunLengthEncoded
> instance FromListConsSnocReverse RunLengthEncoded
> 
> instance FromList NonEmptyRunLengthEncoded where
>   fromList :: (Eq a) => [a] -> NonEmptyRunLengthEncoded a
>   fromList = fromRuns . group
>     where
>       group :: (Eq a) => [a] -> [(Integer, a)]
>       group xs = case xs of
>         [] -> []
>         a:as ->
>           let (us, rest) = span (== a) as
>           in (1 + genericLength us, a) : group rest
> 
> instance FromListConsSnocReverse NonEmptyRunLengthEncoded

Note that we get the analogous `toList` function for free with `Foldable`. Here are some examples:

::: doctest

> -- $
> -- >>> toRuns (fromList ['a','a','b','c','c','c'])
> -- [(2,'a'),(1,'b'),(3,'c')]
> --
> -- >>> toList (fromRuns [(4,'a'),(3,'b')])
> -- "aaaabbb"

:::



Cons, Snoc, and Concat
----------------------

The basic list manipulation functions are a little more complicated than on ordinary lists. Before defining them, it will be helpful if first we can extract the first and last run of a nonempty list.

> class HasRuns t where
>   firstRun :: t a -> Maybe (Run a, RunLengthEncoded a)
>   lastRun  :: t a -> Maybe (Run a, RunLengthEncoded a)
> 
> instance HasRuns RunLengthEncoded where
>   firstRun x = case x of
>     Empty -> Nothing
>     NonEmpty z -> Just $ firstRunNonEmpty z
> 
>   lastRun x = case x of
>     Empty -> Nothing
>     NonEmpty z -> Just $ lastRunNonEmpty z
> 
> instance HasRuns NonEmptyRunLengthEncoded where
>   firstRun = Just . firstRunNonEmpty
>   lastRun  = Just . lastRunNonEmpty
> 
> firstRunNonEmpty
>   :: NonEmptyRunLengthEncoded a
>   -> (Run a, RunLengthEncoded a)
> firstRunNonEmpty (RLE x) =
>   let (r, u) = unconsNonEmpty x
>   in case u of
>     FT.Empty      -> (r, Empty)
>     FT.NonEmpty w -> (r, NonEmpty $ RLE w)
> 
> lastRunNonEmpty
>   :: NonEmptyRunLengthEncoded a
>   -> (Run a, RunLengthEncoded a)
> lastRunNonEmpty (RLE x) =
>   let (r, u) = unsnocNonEmpty x
>   in case u of
>     FT.Empty      -> (r, Empty)
>     FT.NonEmpty w -> (r, NonEmpty $ RLE w)

With these helpers in hand we can implement cons and uncons for both variants.

> instance Cons RunLengthEncoded where
>   cons
>     :: ( Eq a )
>     => a -> RunLengthEncoded a -> RunLengthEncoded a
>   cons a x = case x of
>     Empty -> singleton a
>     NonEmpty w -> NonEmpty (cons a w)
> 
>   uncons
>     :: ( Eq a )
>     => RunLengthEncoded a -> Maybe (a, RunLengthEncoded a)
>   uncons x = case x of
>     Empty -> Nothing
>     NonEmpty w -> Just $ unconsNonEmpty w
> 
> instance SingletonCons RunLengthEncoded
> 
> instance Cons NonEmptyRunLengthEncoded where
>   cons
>     :: ( Eq a )
>     => a -> NonEmptyRunLengthEncoded a -> NonEmptyRunLengthEncoded a
>   cons a x =
>     let (Run (k, b), w) = firstRunNonEmpty x
>     in case (a == b, w) of
>       (True,  Empty           ) -> fromRuns [(1+k, b)]
>       (True,  NonEmpty (RLE v)) -> RLE $ cons (mkRun (1+k) b) v
>       (False, Empty           ) -> fromRuns [(1, a), (k, b)]
>       (False, NonEmpty (RLE v)) -> RLE $ fromList [mkRun 1 a, mkRun k b] <> v
> 
>   uncons
>     :: ( Eq a )
>     => NonEmptyRunLengthEncoded a -> Maybe (a, NonEmptyRunLengthEncoded a)
>   uncons x =
>     let (a, z) = unconsNonEmpty x
>     in case z of
>       Empty -> Nothing
>       NonEmpty u -> Just (a, u)
> 
> instance SingletonCons NonEmptyRunLengthEncoded
> instance SubsetCons NonEmptyRunLengthEncoded

Again the heavy lifting is done by `unconsNonEmpty`:

> instance UnconsNonEmpty NonEmptyRunLengthEncoded where
>   unconsNonEmpty
>     :: ( Eq a )
>     => NonEmptyRunLengthEncoded a
>     -> (a, RunLengthEncoded a)
>   unconsNonEmpty x =
>     let (Run (k, a), w) = firstRunNonEmpty x
>     in case compare k 1 of
>       EQ -> (a, w)
>       GT -> case w of
>         Empty            -> (a, NonEmpty $ fromRuns [(k-1, a)])
>         NonEmpty (RLE z) -> (a, NonEmpty $ RLE $ cons (mkRun (k-1) a) z)
>       LT -> error "RunLengthEncoded: uncons panic"

Snoc is analogous.

> instance Snoc RunLengthEncoded where
>   snoc
>     :: ( Eq a )
>     => a -> RunLengthEncoded a -> RunLengthEncoded a
>   snoc a x = case x of
>     Empty -> singleton a
>     NonEmpty w -> NonEmpty (snoc a w)
> 
>   unsnoc
>     :: ( Eq a )
>     => RunLengthEncoded a -> Maybe (a, RunLengthEncoded a)
>   unsnoc x = case x of
>     Empty -> Nothing
>     NonEmpty w -> Just $ unsnocNonEmpty w
> 
> instance SingletonSnoc RunLengthEncoded
> instance ConsSnoc RunLengthEncoded
> 
> instance Snoc NonEmptyRunLengthEncoded where
>   snoc
>     :: ( Eq a )
>     => a -> NonEmptyRunLengthEncoded a -> NonEmptyRunLengthEncoded a
>   snoc a x =
>     let (Run (k, b), v) = lastRunNonEmpty x
>     in case (a == b, v) of
>       (True,  Empty           ) -> fromRuns [(1+k, b)]
>       (True,  NonEmpty (RLE w)) -> RLE $ snoc (mkRun (1+k) b) w
>       (False, Empty           ) -> fromRuns [(k, b), (1, a)]
>       (False, NonEmpty (RLE w)) -> RLE $ w <> fromList [mkRun k b, mkRun 1 a]
> 
>   unsnoc
>     :: ( Eq a )
>     => NonEmptyRunLengthEncoded a -> Maybe (a, NonEmptyRunLengthEncoded a)
>   unsnoc x =
>     let (a, z) = unsnocNonEmpty x
>     in case z of
>       Empty -> Nothing
>       NonEmpty u -> Just (a, u)
> 
> instance SingletonSnoc NonEmptyRunLengthEncoded
> instance SubsetSnoc NonEmptyRunLengthEncoded
> instance ConsSnoc NonEmptyRunLengthEncoded
> 
> instance UnsnocNonEmpty NonEmptyRunLengthEncoded where
>   unsnocNonEmpty
>     :: ( Eq a )
>     => NonEmptyRunLengthEncoded a
>     -> (a, RunLengthEncoded a)
>   unsnocNonEmpty x =
>     let (Run (k, a), w) = lastRunNonEmpty x
>     in case compare k 1 of
>       EQ -> (a, w)
>       GT -> case w of
>         Empty            -> (a, NonEmpty $ fromRuns [(k-1, a)])
>         NonEmpty (RLE z) -> (a, NonEmpty $ RLE $ snoc (mkRun (k-1) a) z)
>       LT -> error "RunLengthEncoded: unsnoc panic"

When concatenating run length encoded lists, we need to be careful to combine the innermost runs to maintain the invariant that no two adjacent runs have the same base.

> instance (Eq a) => Semigroup (RunLengthEncoded a) where
>   x1 <> x2 = case (x1, x2) of
>     (Empty,       _          ) -> x2
>     (_,           Empty      ) -> x1
>     (NonEmpty u1, NonEmpty u2) -> NonEmpty (u1 <> u2)
> 
> instance (Eq a) => Monoid (RunLengthEncoded a) where
>   mempty = Empty
> 
> instance (Eq a) => Semigroup (NonEmptyRunLengthEncoded a) where
>   u1@(RLE v1) <> u2@(RLE v2) =
>     let
>       (Run (k1, a1), w1) = lastRunNonEmpty u1
>       (Run (k2, a2), w2) = firstRunNonEmpty u2
>     in if a1 /= a2
>       then RLE (v1 <> v2)
>       else let r = singleton (mkRun (k1 + k2) a1)
>         in RLE $ case (w1, w2) of
>           (Empty,            Empty           ) -> r
>           (Empty,            NonEmpty (RLE y)) -> r <> y
>           (NonEmpty (RLE x), Empty           ) -> x <> r
>           (NonEmpty (RLE x), NonEmpty (RLE y)) -> x <> r <> y
> 
> instance Subsemigroup NonEmptyRunLengthEncoded
> 
> instance Ideal NonEmptyRunLengthEncoded where
>   (@>)
>     :: ( Eq a )
>     => NonEmptyRunLengthEncoded a -> RunLengthEncoded a -> NonEmptyRunLengthEncoded a
>   u @> v = case v of
>     Empty      -> u
>     NonEmpty w -> u <> w
> 
>   (<@)
>     :: ( Eq a )
>     => RunLengthEncoded a -> NonEmptyRunLengthEncoded a -> NonEmptyRunLengthEncoded a
>   u <@ v = case u of
>     Empty      -> v
>     NonEmpty w -> w <> v

And both list variants inherit `Reverse` instances in the natural way.

> instance Reverse RunLengthEncoded where
>   reverse :: (Eq a) => RunLengthEncoded a -> RunLengthEncoded a
>   reverse x = case x of
>     Empty -> Empty
>     NonEmpty w -> NonEmpty $ reverse w
> 
> instance ReverseSemigroup RunLengthEncoded
> instance ReverseMonoid RunLengthEncoded
> instance ReverseSingleton RunLengthEncoded
> instance ReverseConsSnoc RunLengthEncoded
> 
> instance Reverse NonEmptyRunLengthEncoded where
>   reverse :: (Eq a) => NonEmptyRunLengthEncoded a -> NonEmptyRunLengthEncoded a
>   reverse (RLE x) = RLE $ reverse x
> 
> instance ReverseSemigroup NonEmptyRunLengthEncoded
> instance ReverseSubset NonEmptyRunLengthEncoded
> instance ReverseSingleton NonEmptyRunLengthEncoded
> instance ReverseConsSnoc NonEmptyRunLengthEncoded



Testing and Debugging
---------------------

And finally, we need some class instances to interact with the testing framework.

> instance Fmt.Display RunSize where
>   display (RunSize s l) = "RunSize"
>     <+> display s <+> display l
> 
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
> instance (Fmt.Display a) => Fmt.Display (Run a) where
>   display (Run z) = "Run" <+> display z
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
> instance (Prune a, Eq a) => Prune (Run a) where
>   prune (Run z) = do
>     (k', a') <- prune z
>     return $ mkRun (abs k') a'
> 
> instance (Fmt.Display a) => Fmt.Display (RunLengthEncoded a) where
>   display x = case x of
>     Empty      -> "Empty"
>     NonEmpty z -> "NonEmpty" <+> display z
> 
> instance (Arb a, Eq a) => Arb (RunLengthEncoded a) where
>   arb = do
>     k :: Int <- arb
>     case k`mod`8 of
>       0 -> pure Empty
>       _ -> NonEmpty <$> arb
> 
> instance (Prune a, Eq a) => Prune (RunLengthEncoded a) where
>   prune x = case x of
>     Empty -> []
>     NonEmpty z -> Empty : (fmap NonEmpty $ prune z)
> 
> instance (Fmt.Display a) => Fmt.Display (NonEmptyRunLengthEncoded a) where
>   display (RLE x) = "RLE" <+> display x
> 
> instance (Arb a, Eq a) => Arb (NonEmptyRunLengthEncoded a) where
>   arb = do
>     xs <- listOf1 arb
>     return $ fromList xs
> 
> instance (Prune a, Eq a) => Prune (NonEmptyRunLengthEncoded a) where
>   prune = map fromList . filter (/= []) . prune . toList

And because run length encoded lists have an essential invariant, we should expose a predicate to make sure the invariant is satisfied.

> validateRunLengthEncoded
>   :: (Eq a) => RunLengthEncoded a -> Bool
> validateRunLengthEncoded x = case x of
>   Empty -> True
>   NonEmpty w -> validateNonEmptyRunLengthEncoded w
> 
> validateNonEmptyRunLengthEncoded
>   :: (Eq a) => NonEmptyRunLengthEncoded a -> Bool
> validateNonEmptyRunLengthEncoded =
>   and . map ((> 0) . fst) . toRuns
