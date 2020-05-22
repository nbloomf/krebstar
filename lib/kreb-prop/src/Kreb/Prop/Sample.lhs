---
title: Pseudorandom Data
author: nbloomf
---

::: frontmatter

> module Kreb.Prop.Sample where
> 
> import Control.Monad (ap)
> import System.Random hiding (next)
> import Data.Bits
> import Data.Char
> 
> import Kreb.Control

:::



Introduction
------------

Unit testing allows us to automatically check code for regressions against expected behavior by, in a nutshell, specifying a list of expected inputs and outputs for a given function. (Hair-splitting over whether this is a _unit_ test or an _integration_ test boil down to how many subsystems the function in question touches, how much control we have over them, and what possible side effects it has.) Unit testing is a powerful tool for validating the quality and correctness of our programs, but it does have weaknesses.

As a trivial example suppose we want to validate the _square_ function, which takes an `Int` and squares it:

::: example
~~~ haskell
square :: Int -> Int
square n = n ^ 2
~~~
:::

Suppose we have a unit test checking that `square` behaves as expected for some input/output pairs:

::: example
~~~ haskell
cases :: [(Int, Int)]
cases = [(0,0),(1,1),(2,4),(-1,1)]
~~~
:::

The tests pass, and all is well. Unfortunately, while this test can certainly tell us that our `square` implementation is _wrong_, it doesn't give much confidence that it's _right_. For example, this implementation passes the same unit test:

::: example
~~~ haskell
badsquare :: Int -> Int
badsquare n = if -1 <= n && n <= 2
  then n ^ 2
  else 27
~~~
:::

Now- would anyone actually write that implementation for `square`? Certianly not, unless they were specifically attacking a testing methodology. :) But `square` is as simple as functions get, and it's not difficult to design more complex functions that end up having some not-so-obviously bad behavior that our unit tests, which are by definition the test cases we could think of, don't catch.

This discussion of `square` and its correctness is a little sloppy, though. I've referred to the implementation as being either _wrong_ or _right_, but these words are meaningless on their own. It's better to say that a function either does or does not conform to a particular _specification_. We have an intuitive expectation of what the specification for `square` should be: something like

- For any `Int`, say `n`, we have `square n == n*n`.

But this is not explicit until we make it explicit. And now we can see why unit tests can't possibly give us strong guarantees about the correctness of `square`: the specification has a universal quantifier (_for any_) but the unit test can only ever check finitely many examples.

What we really want is to check the implementation against the specification, not just some specific examples. This is called _property testing_, and this approach to testing was, as far as I know, pioneered by the `QuickCheck` library.

The broad strategy for property testing goes like this:

1. Given some implementation code decide on a specification it should satisfy, expressed as one or more universally quantified boolean-valued functions. (for all $a$, $p(a)$ is true.)
2. Generate a bunch of random $a$s.
3. Evaluate the $p(a)$s, looking for any that violate the specification.

The received wisdom about randomness in test code is "don't do it", so you may feel slightly uncomfortable at step 2. The idea is that random test failures are hard to replicate and it's difficult to separate "real" failures from "unlucky rng" failures. But in practice this isn't a problem. Moreover, property testing can be devastatingly effective at both finding bugs in our implementation and uncovering hidden assumptions in our specification, drastically improving confidence that the code (and any changes we make to it) really are _correct_.

In this library we'll build a little clone of Haskell's `QuickCheck` framework for property testing. We're working from the classic QuickCheck papers, as well as the (permissively licensed) [implementation](https://hackage.haskell.org/package/QuickCheck).



A Pseudorandom Monad
--------------------

For starters we need a way to generate pseudorandom values. One way to do this is to assume we have access to an indefinite stream of pseudorandom numbers to use as an entropy source -- our `StreamT` monad transformer can do this, with the standard library `StdGen` type acting as the stream. For practical reasons it will also be useful to carry a read-only integer "size" we can use to tweak the randomness.

> type Sample a = EnvT Size (StreamT StdGen Identity) a
> 
> type Size = Int
> 
> instance IsStream StdGen where
>   data StreamValue StdGen = G StdGen
> 
>   advance x =
>     let (a, z) = split x
>     in (G a, z)

To run a `Sample` computation, we supply it with an initial size and stream:

> runSample
>   :: StdGen -> Size -> Sample a -> a
> runSample gen size =
>   unIdentity . runStreamT gen . runEnvT size

We also define some aliases for the standard `EnvT` functions. `size` gets the size parameter, while `withSize` and `adjustSize` let us run a computation with a new or altered size, respectively.

> size
>   :: Sample Size
> size = ask
> 
> withSize
>   :: Size -> Sample a -> Sample a
> withSize k =
>   local (\_ -> k)
> 
> adjustSize
>   :: (Size -> Size) -> Sample a -> Sample a
> adjustSize f =
>   local (\k -> f k)

We will also need a function with this odd signature later on:

> promoteSample
>   :: forall a b
>    . (a -> Sample b) -> Sample (a -> b)
> promoteSample f = EnvT $ \r -> StreamT $ \s ->
>   let
>     g :: a -> b
>     g a = unIdentity $ runStreamT s $ runEnvT r (f a)
>   in Identity (g, s)

(We'll use `promoteSample` to generate arbitrary functions.) Next, we'll leverage the standard library `Random` class to generate `Sample` values for some basic types.

> randIn
>   :: ( Random a )
>   => (a,a) -> Sample a
> randIn bds = do
>   G gen <- next @StdGen
>   return $ fst $ randomR bds gen
> 
> rand
>   :: ( Random a )
>   => Sample a
> rand = do
>   G gen <- next @StdGen
>   return $ fst $ random gen

And that's it for the basic API of `Sample`; we get most of it for free by virtue of it being a monad stack. Note how the `next` method for stack monads handles advancing the state of the random number generator.

Baking examples of this code into doctests is awkward, because it will give different results on every run. But we can try anyway.

::: doctest

> -- $
> -- >>> :{
> --   let
> --     gen = mkStdGen 27
> --     k = runSample gen 5 $ randIn (1 :: Int, 10)
> --   in elem k [1..10]
> -- :}
> -- True

:::



Sample Combinators
------------------

A `Sample a` is a computation which, when run, returns an `a` drawn from some probability distribution. With `randIn` and `rand` in hand we can generate sample values of any type that implements the standard `Random` class, which is basically all the built in integer-like types.

First let's define a basic function to draw values from a `Sample a` -- this is just for testing.

> draw
>   :: Int -> Sample a -> IO [a]
> draw k seeded = do
>   gen <- newStdGen
>   return $ runSample gen 0 $
>     sequence $ map (\m -> withSize m seeded) $
>     map (2*) [0..(k-1)]

`vectOf` generates a list with a given length.

> vectOf :: (Integral n) => n -> Sample a -> Sample [a]
> vectOf n gen =
>   let k = fromIntegral $ toInteger n
>   in if k <= 0
>     then pure []
>     else sequence $ replicate k $ adjustSize (`div` k) $ gen

There are lots of different ways to generate lists of unspecified length, and which is most appropriate depends on the context. All of them amount to (1) choosing what the length of the list should be, and then (2) choosing how to adjust the size parameter when generating list items. We'll define a generic combinator, `listBy`, bundling up this pattern. It takes two function parameters, `f` and `g`, and generates a list of length `f k` where `k` is between 0 and the size parameter, and the size parameter when generating list items is set to `g size (f k)`. Different choices of `f` and `g` will result in different distributions of lists. The boolean parameter lets us signal whether the list can be empty or not.

> listBy
>   :: Bool
>   -> (Int -> Int)
>   -> (Int -> Int -> Int)
>   -> Sample a -> Sample [a]
> listBy nonEmpty f g gen = do
>   z <- size
>   k <- randIn (z `div` 3, z)
>   let
>     len = max 0 (f k) +
>       if nonEmpty then 1 else 0
>   vectOf len $ withSize (g z len) gen

`listOf` bounds the list length by the base 2 log of the `size` parameter, and does not adjust the size parameter when generating the list items. Used carelessly (especially on nested structures) it can easily generate enormous data. `listOf1` is a variant that never generates the empty list.

> listOf :: Sample a -> Sample [a]
> listOf = listBy False ilog2 (\z _ -> z)
> 
> listOf1 :: Sample a -> Sample [a]
> listOf1 = listBy True ilog2 (\z _ -> z)
> 
> ilog2
>   :: ( Integral n ) => n -> Int
> ilog2 w = if w <= 0
>   then 0
>   else 1 + ilog2 (div w 2)

`longListOf` bounds the list length by the size parameter and does not adjust the size parameter when generating the list items -- even more dangerous than `listOf`.

> longListOf :: Sample a -> Sample [a]
> longListOf = listBy False id (\z _ -> z)
> 
> longListOf1 :: Sample a -> Sample [a]
> longListOf1 = listBy True id (\z _ -> z)

`nestOf` bounds the list length with the base 2 log of `size`, like `listOf`, but divides the size parameter by the list length when generating list items. Thinking of size as a measure of complexity, this bounds the complexity of the resulting list roughly by the original size parameter.

> nestOf :: Sample a -> Sample [a]
> nestOf = listBy False ilog2 (\z u -> z `div` (max 1 u))
> 
> nestOf1 :: Sample a -> Sample [a]
> nestOf1 = listBy True ilog2 (\z u -> z `div` (max 1 u))

`longNestOf` is analogous to `nestOf`, but bounds the length by `size`.

> longNestOf :: Sample a -> Sample [a]
> longNestOf = listBy False id (\z u -> z `div` (max 1 u))
> 
> longNestOf1 :: Sample a -> Sample [a]
> longNestOf1 = listBy True id (\z u -> z `div` (max 1 u))

Usually `nestOf` is a safe choice when we want an arbitrary list.

A related problem is to pick a distribution fairly from a given nonempty list: `selectFrom` does this. `oneFrom` is the special case when all the input distributions are trivial.

> selectFrom
>   :: [Sample a] -> Sample a
> selectFrom z = case z of
>   [] -> error "selectFrom called on empty list"
>   _ -> do
>     let k = length z
>     i <- randIn (0,k-1)
>     z !! i
> 
> oneFrom
>   :: [a] -> Sample a
> oneFrom = selectFrom . map return

As an alternative to `selectFrom`, we can choose fairly among a tuple of distributions.

> pickFrom2
>   :: (Sample a, Sample a)
>   -> Sample a
> pickFrom2 (a0, a1) = do
>   i <- randIn (0 :: Int, 1)
>   case i of
>     0 -> a0; _ -> a1
> 
> pickFrom3
>   :: (Sample a, Sample a, Sample a)
>   -> Sample a
> pickFrom3 (a0, a1, a2) = do
>   i <- randIn (0 :: Int, 2)
>   case i of
>     0 -> a0; 1 -> a1; _ -> a2
> 
> pickFrom4
>   :: (Sample a, Sample a, Sample a, Sample a)
>   -> Sample a
> pickFrom4 (a0, a1, a2, a3) = do
>   i <- randIn (0 :: Int, 3)
>   case i of
>     0 -> a0; 1 -> a1; 2 -> a2; _ -> a3
> 
> pickFrom5
>   :: (Sample a, Sample a, Sample a, Sample a, Sample a)
>   -> Sample a
> pickFrom5 (a1, a2, a3, a4, a5) = do
>   i <- randIn (0 :: Int, 4)
>   case i of
>     0 -> a1; 1 -> a2; 2 -> a3; 3 -> a4; _ -> a5
> 
> pickFrom6
>   :: (Sample a, Sample a, Sample a, Sample a, Sample a, Sample a)
>   -> Sample a
> pickFrom6 (a1, a2, a3, a4, a5, a6) = do
>   i <- randIn (0 :: Int, 5)
>   case i of
>     0 -> a1; 1 -> a2; 2 -> a3; 3 -> a4; 4 -> a5; _ -> a6

`freq` is a variant of `selectFrom` allowing us to weight the input distributions. For example, in

::: example
~~~ haskell
z = freq
  [ (1, x)
  , (2, y)
  ]
~~~
:::

when `z` is sampled, it draws from `x` 1/3 of the time and from `y` 2/3 of the time.

> freq
>   :: forall a. [(Int, Sample a)] -> Sample a
> freq ws = case ws of
>   [] -> error "freq: undefined on []"
>   (k0,x0):vs -> if any (< 0) $ map fst ws
>     then error "freq: negative weight"
>     else if all (== 0) $ map fst ws
>       then error "Kreb.Prop.Sample.freq: all weights zero"
>       else randIn (1, total) >>= thresh vs
>         where
>           total :: Int
>           total = sum $ map fst ws
> 
>           thresh
>             :: [(Int, Sample a)] -> Int -> Sample a
>           thresh zs n = case zs of
>             [] -> x0
>             (k,x):us -> if n <= k
>               then x
>               else thresh us (n-k)

The next combinator lets us vary the seed stream using an integer; it will be handy when we start generating random functions. This implementation comes directly from QuickCheck, and features a clever application of the [Elias gamma encoding](https://en.wikipedia.org/wiki/Elias_gamma_coding) of integers to guarantee that calling `twiddle n` for different `n` generates independent seeds.

> twiddle
>   :: ( Integral n )
>   => n -> Sample a -> Sample a
> twiddle k x =
>   fiddle (twiddleInt (toInteger k)) >> x
>   where
>     twiddleInt
>       :: Integer -> StdGen -> StdGen
>     twiddleInt k gen =
>       let (gen1, gen2) = split gen
>       in if k >= 1
>         then gamma k gen1
>         else gamma (1-k) gen2
> 
>     gamma
>       :: Integer -> StdGen -> StdGen
>     gamma n =
>       let k = ilog2 n
>       in encode k . zeroes k
>       where
>         encode :: Int -> StdGen -> StdGen
>         encode m gen = if m <= -1
>           then gen
>           else let (gen1, gen2) = split gen in
>             if testBit n m
>               then encode (m-1) gen2
>               else encode (m-1) gen1
> 
>     zeroes
>       :: Int -> StdGen -> StdGen
>     zeroes m gen = if m <= 0
>       then gen
>       else fst $ split gen

We can filter a distribution by tossing out generated values that don't satisfy a predicate. This is dangerous -- careless use of `satisfying` can make sampling fail to terminate.

> satisfying
>   :: Sample a -> (a -> Bool) -> Sample a
> satisfying gen p = do
>   mx <- gen `satisfyingMaybe` p
>   case mx of
>     Just x -> return x
>     Nothing -> do
>       k <- size
>       withSize (k+1) (gen `satisfying` p)
> 
> satisfyingMaybe
>   :: Sample a -> (a -> Bool) -> Sample (Maybe a)
> satisfyingMaybe gen p = do
>   n <- size
>   try n (2*n)
>     where
>       try m n = if m > n
>         then return Nothing
>         else do
>           x <- withSize m gen
>           if p x
>             then return $ Just x
>             else try (m+1) n

This is also a good place to dump some character specific combinators.

> arbAsciiChar
>   :: Sample Char
> arbAsciiChar = randIn ('\0', '\127')
> 
> arbPrintableAsciiChar
>   :: Sample Char
> arbPrintableAsciiChar = freq
>   [ (95, randIn ('\x20', '\x7e'))
>   , (1,  return '\t')
>   , (1,  return '\n')
>   ]
> 
> arbUnicodeChar
>   :: Sample Char
> arbUnicodeChar =
>   (randIn ('\0', '\1114111')) `satisfying` notSurrogate
>   where
>     notSurrogate c = Surrogate /= generalCategory c
