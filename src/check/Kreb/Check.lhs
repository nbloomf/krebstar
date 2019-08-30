> module Kreb.Check where

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

It appears that `testCases` can only work with properties that take a single argument. But with some extensions to the standars `uncurry` function this is not really a restriction.

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
