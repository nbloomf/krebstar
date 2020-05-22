---
title: Running Tests
author: nbloomf
---

::: frontmatter

> module Kreb.Prop.Tests (
>     testProp, testPropWith, testPropIO, testPropWithIO
>   , TestArgs(..), isSuccess
> ) where
> 
> import System.Random (split, newStdGen, StdGen)
> 
> import           Kreb.Control
> import qualified Kreb.Format as Fmt
> import           Kreb.Format
> 
> import Kreb.Prop.Sample
> import Kreb.Prop.Arb
> import Kreb.Prop.Check

:::



Introduction
------------

We're almost done with the core of our quickcheck clone -- the last major piece is a "test runner" to take a `Checkable` value, apply it to random inputs, and manage some statistics about the outcomes. The runner will need some state, so we implement it using a monad stack.

> type Test = EnvT TestArgs (StateT TestSt Identity)
> 
> execTest
>   :: StdGen -> TestArgs -> Test a -> a
> execTest gen env =
>   runState (initTestSt gen) . runEnvT env

A `Test a` is a computation that carries the `TestArgs` (read-only) and `TestSt` (read-write) state and, when executed, produces a value of type `a`.

The state types represent some parameters of the test run.

> data TestArgs = TestArgs
>   { maxSuccess      :: Int -- when to stop trying cases and declare success
>   , maxDiscardRatio :: Int -- when to stop discarding cases and declare failure
>   , maxSize         :: Int -- largest size parameter to generate data with
>   , maxPrunes       :: Int -- when to stop trying to prune failing cases
>   } deriving (Eq, Show)
> 
> defaultTestArgs :: TestArgs
> defaultTestArgs = TestArgs
>   { maxSuccess      = 100
>   , maxDiscardRatio = 10
>   , maxSize         = 100
>   , maxPrunes       = 20000
>   }
> 
> data TestSt = TestSt
>   { numSuccessfulTests   :: Int -- how many passing tests we've seen so far
>   , numDiscardedTests    :: Int -- how many cases we've discarded so far
>   , recentDiscardedTests :: Int -- cases discarded since the last success
>   , numSuccessfulPrunes  :: Int -- how many times we've pruned
>   , numPruneAttempts     :: Int -- number of attempts since the last prune
>   , randomSeed           :: StdGen -- used to execute individual tests
>   } deriving Show
> 
> initTestSt :: StdGen -> TestSt
> initTestSt gen = TestSt
>   { numSuccessfulTests   = 0
>   , numDiscardedTests    = 0
>   , recentDiscardedTests = 0
>   , numSuccessfulPrunes  = 0
>   , numPruneAttempts     = 0
>   , randomSeed           = gen
>   }



Running Tests
-------------

The result of running an individual property test is a value of type `Result`. Surprise! A property test can end in one of three ways:

- _Success_ means we did not find any failures before finding a prescribed number of successes.
- _Failure_ means we found a failure before finding enough successes.
- _Giving up_ means we didn't find any failures, but had to throw out too many test cases. (If this happens it usually means we need to use a more specific test case generator.)

> data Result
>   = Success NumTests NumDiscarded
>   | GaveUp NumTests NumDiscarded
>   | Failure
>       NumTests NumDiscarded NumPrunes
>       NumPruneTries Size Reason [ShowArg]
> 
> type NumTests      = Int
> type NumDiscarded  = Int
> type NumPrunes     = Int
> type NumPruneTries = Int
> 
> isSuccess :: Result -> Bool
> isSuccess z = case z of
>   Success _ _ -> True
>   _           -> False

The `Result` is what gets printed for analysis, so we give it a `Display` instance.

> instance Fmt.Display Result where
>   display z = unAnnotate $ case z of
>     Success good disc ->
>       if disc <= 0
>         then
>           reflow ">=> ok: passed"
>             <+> display good <+> string "tests"
>         else
>           reflow ">=> ok: passed"
>             <+> display good <+> string "tests"
>             <+> parens (display disc <+> string "discarded")
>     GaveUp good disc ->
>       reflow ">=> gave up after" <+> display good
>         <+> reflow "successful cases and"
>         <+> display disc <+> reflow "discarded cases"
>     Failure good disc prunes
>       pruneTries size msg args ->
>         let
>           d = if disc <= 0 then emptyDoc
>             else space <> parens (display disc <+> string "discarded")
>           s = if prunes <= 0 then emptyDoc
>             else space <> string "and" <+> display prunes <+> string "prunes"
>           r = if isEmptyDoc msg then emptyDoc
>             else indent 2 (string "Reason:" <> softlineS <> msg)
>           a = if null args then emptyDoc
>             else indent 2 (string "Args:" <+> parenList (map unAnnotate args))
>         in reflow ">=> failed after" <+> display good
>           <+> reflow "valid cases" <> d <> s <> lineS <> r <> lineS <> a

Turning a `Check` into a `Result` comes down to running the check on a seed, collecting stats on the outcome, and deciding when to stop.

> runTest
>   :: Check -> Test Result
> runTest ch = do
>   done <- gets numSuccessfulTests
>   goal <- asks maxSuccess
>   -- have we seen enough successes yet?
>   if done >= goal
>     -- if so, quit successfully.
>     then doneTesting
>     else do
>       discards <- gets numDiscardedTests
>       badRatio <- asks maxDiscardRatio
>       -- have we seen to many discards?
>       if discards >= badRatio * goal
>         -- if so, quit unsuccessfully.
>         then giveUp
>         -- otherwise get another test outcome.
>         else testOutcome
> 
>   where
>     doneTesting :: Test Result
>     doneTesting = do
>       good <- gets numSuccessfulTests
>       disc <- gets numDiscardedTests
>       return $ Success good disc
> 
>     giveUp :: Test Result
>     giveUp = do
>       good <- gets numSuccessfulTests
>       disc <- gets numDiscardedTests
>       return $ GaveUp good disc
> 
>     -- run the check once more
>     testOutcome
>       :: Test Result
>     testOutcome = do
>       gen <- splitSeed
>       size <- computeSize
>       let
>         -- get one check outcome
>         Rose outcome ts =
>           runSample gen size (unCheck ch)
>       case outcome of
>         Accept ->
>           recordSuccessfulTestCase gen >> runTest ch
>         Discard msg ->
>           recordDiscardedTestCase gen msg >> runTest ch
>         Reject msg0 args0 ->
>           -- shut it down
>           reportFailingTestCase size msg0 args0 ts
> 
>     -- ensure that each check gets a fresh seed.
>     splitSeed :: Test StdGen
>     splitSeed = do
>       seed <- gets randomSeed
>       let (gen1, gen2) = split seed
>       mutate $ \st -> st { randomSeed = gen1 }
>       return gen2
> 
>     -- make the size parameter start small and grow to maxSize
>     computeSize :: Test Int
>     computeSize = do
>       maxGood <- asks maxSuccess
>       largest <- asks maxSize
>       n <- gets numSuccessfulTests
>       d <- gets recentDiscardedTests
>       let
>         p1 = (largest + (roundTo n largest)) <= maxGood
>         p2 = maxGood <= n
>         p3 = 0 == mod maxGood largest
> 
>         q = if p1 || p2 || p3
>           then 1
>           else div largest (mod maxGood largest)
>       return $ min largest $ (div d 10) + (mod n largest) * q
>       where
>         roundTo :: Int -> Int -> Int
>         roundTo n m = (n `div` m) * m
> 
>     recordSuccessfulTestCase
>       :: StdGen -> Test ()
>     recordSuccessfulTestCase gen =
>       mutate $ \st -> st
>         { numSuccessfulTests   = 1 + numSuccessfulTests st
>         , recentDiscardedTests = 0
>         , randomSeed           = gen
>         }
> 
>     recordDiscardedTestCase
>       :: StdGen -> Reason -> Test ()
>     recordDiscardedTestCase gen msg =
>       mutate $ \st -> st
>         { numDiscardedTests    = 1 + numDiscardedTests st
>         , recentDiscardedTests = 1 + recentDiscardedTests st
>         , randomSeed           = gen
>         }
> 
>     reportFailingTestCase
>       :: Int -> Reason -> [ShowArg] -> [Rose Outcome]
>       -> Test Result
>     reportFailingTestCase size msg0 args0 ts = do
>       good <- gets numSuccessfulTests
>       disc <- gets numDiscardedTests
>       tries <- asks maxPrunes
>       let
>         (ps, pas, msg, args) =
>           minimizeFailure ts tries (0, 0, msg0, args0)
>       return $ Failure good disc
>         ps pas size msg args

All that business with `Test` and `Result` is only needed inside this module; all we need to export are some helper functions for setting up and running the property test for a `Checkable` value.

> testPropWith
>   :: ( Checkable check )
>   => StdGen -> TestArgs -> check -> Result
> testPropWith gen env p =
>   execTest gen env (runTest $ check p)
> 
> testProp
>   :: ( Checkable check )
>   => StdGen -> check -> Result
> testProp gen =
>   testPropWith gen defaultTestArgs

For testing, we also define runner functions that take the extra step of getting a random initial seed from IO and printing the results.

> testPropWithIO
>   :: ( Checkable check )
>   => TestArgs -> check -> IO ()
> testPropWithIO args ch = do
>   gen <- newStdGen
>   let res = testPropWith gen args ch
>   Fmt.prettyPrint res
> 
> testPropIO
>   :: ( Checkable check )
>   => check -> IO ()
> testPropIO = testPropWithIO defaultTestArgs

It's still awkward to give examples of this with doctest because to make sure it's really working we need to show that it catches failures -- but it's hard to do that in a static doctest. But we can try!

Here's what it looks like on some boolean checks.

::: doctest

> -- $
> -- >>> :{
> --   testPropIO $ claimTrue True
> -- :}
> -- >=> ok: passed 100 tests
> --
> -- $
> -- >>> :{
> --   testPropIO $ claimTrue False
> -- :}
> -- >=> failed after 0 valid cases
> --   Reason: Expected True but got False.

:::

Here's what it looks like with a function check. The `a` parameter is filled in with random inputs at runtime.

::: doctest

> -- $
> -- >>> :{
> --   testPropIO $ \(a :: Int) ->
> --     claimEqual a (a + 0)
> -- :}
> -- >=> ok: passed 100 tests

:::

And that's it -- the core of our property testing library. We have a small interface: `Arb`, `CoArb`, and `Prune`; the `claim*` helpers; and the `testProp*` runners. With these we can describe properties that code must satisfy and generate random data to test them with.

That's not to say there's nothing more to do. We can build some bonus infrastructure to make the ergonomics of property testing much better. But all that stuff is optional; all the core functionality is already done.
