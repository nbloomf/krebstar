> {-# LANGUAGE TypeFamilies, FlexibleContexts, EmptyDataDeriving, StandaloneDeriving, FlexibleInstances #-}

> module Kreb.Check (
>     module Kreb.Check.Seeded
>   , module Kreb.Check.Arb
>   , module Kreb.Check.Check
>   , module Kreb.Check.Tests
>   , module Kreb.Check.Tasty
> 
>   , module Kreb.Check.Alg
>   , module Kreb.Check.Laws
> 
>   , module Kreb.Check.Fun
> --  , module Kreb.Check.Build
>   , module Kreb.Check.StateMachine
> 
>   , uncurry3
>   , uncurry4
>   , testCases
> ) where

> import Control.Monad (ap)
> import System.Random
> import Data.List

> import Test.QuickCheck
> import Test.Tasty
> import Test.Tasty.QuickCheck

> import Kreb.Check.Seeded
> import Kreb.Check.Arb
> import Kreb.Check.Check hiding (ZZ(..))
> import Kreb.Check.Tests
> import Kreb.Check.Tasty
> import Kreb.Check.Alg
> import Kreb.Check.Laws
> import Kreb.Check.Fun
> import Kreb.Check.Build
> import Kreb.Check.StateMachine

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



> {-




> visitOutcomes :: Rose Outcome -> Outcome
> visitOutcomes z = case z of
>   Rose Accept _ -> Accept
>   Rose failure1 children ->
>     let failure2 = find (isFailure . roseVal) children
>     in case failure2 of
>       Nothing -> failure1
>       Just x -> visitOutcomes x


> isFailure Accept = False
> isFailure _ = True







> checkCase
>   :: ( Checkable check )
>   => Int -> Int -> Depth -> Size -> check -> Outcome
> checkCase num seed d s c = runAll (check c)
>   where
>     runAll :: Check -> Outcome
>     runAll pr = foldMap (runOne pr) $ take num $ [seed..]
> 
>     runOne :: Check -> Int -> Outcome
>     runOne pr sd =
>       let result = visitOutcomes $ runCheck pr $ initArbEnvWith (Seed seed) d s
>       in case result of
>         Accept -> Accept
>         Reject _ _ _ args msg -> Reject (Seed sd) d s args msg

> krebCheck :: Checkable prop => prop -> IO Outcome
> krebCheck = krebCheckWith 100 5 5

> krebCheckWith :: Checkable prop => Int -> Depth -> Size -> prop -> IO Outcome
> krebCheckWith attemptNb d s prop = do
>   seed <- randomIO
>   return $ checkCase attemptNb seed d s prop





> -}



