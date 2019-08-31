> module Kreb.Check where

> import System.Random

> import Test.QuickCheck
> import Test.Tasty
> import Test.Tasty.QuickCheck

A note on strategy
------------------

QuickCheck shines at random testing. But at the same time, there's value in having fixed test cases. For one thing, when QuickCheck finds a legitimately buggy example, we can keep it around as a regression test for extra goodness. To help with this we'll define a helper function, `testCases`, that just runs a given testable property against a list of named examples.

> testCases
>   :: ( Testable prop )
>   => (a -> prop) -> [(String, a)] -> TestTree
> testCases property cs =
>   let
>     caseNum (l,c) =
>       testProperty l (property c)
>   in testGroup "Fixed Test Cases" $
>     map caseNum cs

It appears that `testCases` can only work with properties that take a single argument. But with some extensions to the standard `uncurry` function this is not really a restriction.

> uncurry3
>   :: (a1 -> a2 -> a3 -> b)
>   -> (a1,a2,a3) -> b
> uncurry3 f (a1,a2,a3) =
>   f a1 a2 a3
> 
> uncurry4
>   :: (a1 -> a2 -> a3 -> a4 -> b)
>   -> (a1,a2,a3,a4) -> b
> uncurry4 f (a1,a2,a3,a4) =
>   f a1 a2 a3 a4




> data Arb a
>   = Arb (StdGen -> a)
> 
> runArb
>   :: StdGen -> Arb a -> a
> runArb seed (Arb f) = f seed
 
> runArb'
>   :: Int -> Arb a -> a
> runArb' k = runArb (mkStdGen k)

> arbBool :: Arb Bool
> arbBool = Arb $ \gen ->
>   fst $ random gen
> 
> arbInt :: Int -> Arb Int
> arbInt m = Arb $ \gen ->
>   let k = fst $ random gen
>   in mod k (1 + (abs m))



> class Checkable t where
>   checkable :: t -> Check

> data Check = Check

> data Result
>   = Success
>   | Failure
>   deriving (Eq, Show)

> data Outcome
>   = Accept
>   | Reject String
>   | Discard String
>   deriving (Eq, Show)

> data Complexity
>   = Trivial
>   | Boundary
>   | Complex Int
>   deriving (Eq, Show)

> class Construct t where
>   construct :: Complexity -> Arb t
> 











