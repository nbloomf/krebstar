> module Kreb.Check.Tests where

> import System.Random (split, newStdGen, StdGen)
> import Control.Monad (ap)
> import Data.List (unwords)

> import Kreb.Check.Seeded
> import Kreb.Check.Arb
> import Kreb.Check.Check



> newtype Test a = Test
>   { unTest :: TestArgs -> TestSt -> (a, TestSt) }
> 
> data TestArgs = TestArgs
>   { maxSuccess      :: Int
>   , maxDiscardRatio :: Int
>   , maxSize         :: Int
>   , maxDepth        :: Int
>   , maxPrunes       :: Int
>   } deriving (Eq, Show)
> 
> defaultTestArgs :: TestArgs
> defaultTestArgs = TestArgs
>   { maxSuccess      = 100
>   , maxDiscardRatio = 10
>   , maxSize         = 100
>   , maxDepth        = 10
>   , maxPrunes       = 20000
>   }
> 
> data TestSt = TestSt
>   { numSuccessfulTests   :: Int
>   , numDiscardedTests    :: Int
>   , recentDiscardedTests :: Int
>   , numSuccessfulPrunes  :: Int
>   , numPruneAttempts     :: Int
>   , randomSeed           :: StdGen
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

> execTest
>   :: StdGen -> TestArgs -> Test a -> a
> execTest gen env (Test x) =
>   fst $ x env (initTestSt gen)



> instance Functor Test where
>   fmap f x = x >>= (return . f)
> 
> instance Applicative Test where
>   pure = return
>   (<*>) = ap
> 
> instance Monad Test where
>   return a = Test $ \_ st -> (a, st)
> 
>   (Test x) >>= f = Test $ \env st0 ->
>     let (a, st1) = x env st0
>     in unTest (f a) env st1
> 
> asks :: (TestArgs -> a) -> Test a
> asks f = Test $ \env st ->
>   (f env, st)
> 
> gets :: (TestSt -> a) -> Test a
> gets f = Test $ \_ st ->
>   (f st, st)
> 
> puts :: (TestSt -> TestSt) -> Test ()
> puts f = Test $ \_ st ->
>   ((), f st)

> splitSeed :: Test (StdGen, StdGen)
> splitSeed = do
>   seed <- gets randomSeed
>   return $ split seed

> roundTo :: Int -> Int -> Int
> roundTo n m = (n `div` m) * m

> computeSize :: Test Int
> computeSize = do
>   maxGood <- asks maxSuccess
>   largest <- asks maxSize
>   n <- gets numSuccessfulTests
>   d <- gets recentDiscardedTests
>   let
>     p1 = (largest + (roundTo n largest)) <= maxGood
>     p2 = maxGood <= n
>     p3 = 0 == mod maxGood largest
> 
>     q = if p1 || p2 || p3
>       then 1
>       else div largest (mod maxGood largest)
>   return $ min largest $ (div d 10) + (mod n largest) * q
> 
> computeDepth :: Test Int
> computeDepth = do
>   maxGood <- asks maxSuccess
>   largest <- asks maxDepth
>   n <- gets numSuccessfulTests
>   d <- gets recentDiscardedTests
>   let
>     p1 = (largest + (roundTo n largest)) <= maxGood
>     p2 = maxGood <= n
>     p3 = 0 == mod maxGood largest
> 
>     q = if p1 || p2 || p3
>       then 1
>       else div largest (mod maxGood largest)
>   return $ min largest $ (div d 10) + (mod n largest) * q




> recordSuccessfulTestCase
>   :: StdGen -> Test ()
> recordSuccessfulTestCase gen =
>   puts $ \st -> st
>     { numSuccessfulTests   = 1 + numSuccessfulTests st
>     , recentDiscardedTests = 0
>     , randomSeed           = gen
>     }
> 
> recordDiscardedTestCase
>   :: StdGen -> Reason -> Test ()
> recordDiscardedTestCase gen msg =
>   puts $ \st -> st
>     { numDiscardedTests    = 1 + numDiscardedTests st
>     , recentDiscardedTests = 1 + recentDiscardedTests st
>     , randomSeed           = gen
>     }
> 
> reportFailingTestCase
>   :: Int -> Int -> Reason -> [ShowArg] -> [Rose Outcome]
>   -> Test Result
> reportFailingTestCase size depth msg0 args0 ts = do
>   good <- gets numSuccessfulTests
>   disc <- gets numDiscardedTests
>   tries <- asks maxPrunes
>   let
>     (ps, pas, msg, args) =
>       minimizeFailure ts tries (0, 0, msg0, args0)
>   return $ Failure (ZZ good) (ZZ disc)
>     (ZZ ps) (ZZ pas) (ZZ depth) (ZZ size) msg args





> data NumTests
> data NumDiscarded
> data NumPrunes
> data NumPruneTries

> data Result
>   = Success (ZZ NumTests) (ZZ NumDiscarded)
>   | GaveUp (ZZ NumTests) (ZZ NumDiscarded)
>   | Failure
>       (ZZ NumTests) (ZZ NumDiscarded) (ZZ NumPrunes)
>       (ZZ NumPruneTries) (ZZ Depth) (ZZ Size) Reason [ShowArg]
>   deriving (Eq, Show)
> 
> isSuccess :: Result -> Bool
> isSuccess z = case z of
>   Success _ _ -> True
>   _ -> False
> 
> prettyResult :: Result -> String
> prettyResult z = case z of
>   Success (ZZ good) (ZZ disc) ->
>     if disc <= 0
>       then unwords
>         [ ">>> ok: passed", show good, "tests" ]
>       else unwords
>         [ ">>> ok: passed", show good, "tests"
>         , "(" ++ show disc, "discarded)"]
>   GaveUp (ZZ good) (ZZ disc) -> unwords
>     [ ">>> gave up after", show good, "successful cases and"
>     , show disc, "discarded cases" ]
>   Failure (ZZ good) (ZZ disc) (ZZ prunes)
>     (ZZ pruneTries) (ZZ depth) (ZZ size) msg args ->
>       let
>         d = if disc <= 0 then ""
>           else concat [ " (", show disc, " discarded)" ]
>         s = if prunes <= 0 then ""
>           else concat [ " and ", show prunes, " prunes" ]
>         r = if msg == "" then ""
>           else concat [ "Reason:\n  ", msg, "\n" ]
>         a = if args == [] then ""
>           else concat [ "Args:\n", unlines $ map ("  > " ++) args ]
>       in concat
>         [ ">>> failed after ", show good
>         , " valid cases", d, s, "\n", r, a ]





> runTest
>   :: Check -> Test Result
> runTest ch = do
>   done <- gets numSuccessfulTests
>   goal <- asks maxSuccess
>   if done >= goal
>     then doneTesting
>     else do
>       discards <- gets numDiscardedTests
>       badRatio <- asks maxDiscardRatio
>       if discards >= badRatio * goal
>         then giveUp
>         else runOneTest ch

> doneTesting :: Test Result
> doneTesting = do
>   good <- gets numSuccessfulTests
>   disc <- gets numDiscardedTests
>   return $ Success (ZZ good) (ZZ disc)

> giveUp :: Test Result
> giveUp = do
>   good <- gets numSuccessfulTests
>   disc <- gets numDiscardedTests
>   return $ GaveUp (ZZ good) (ZZ disc)

> runOneTest
>   :: Check -> Test Result
> runOneTest ch = do
>   (gen1, gen2) <- splitSeed
>   size <- computeSize
>   depth <- computeDepth
>   let
>     Rose outcome ts =
>       runSeededWith gen1 (ZZ depth) (ZZ size) (unCheck ch)
>   case outcome of
>     Accept ->
>       recordSuccessfulTestCase gen2 >> runTest ch
>     Discard msg ->
>       recordDiscardedTestCase gen2 msg >> runTest ch
>     Reject msg0 args0 ->
>       reportFailingTestCase size depth msg0 args0 ts


> krebCheckWith
>   :: ( Checkable check )
>   => StdGen -> TestArgs -> check -> Result
> krebCheckWith gen env p =
>   execTest gen env (runTest $ check p)
> 
> krebCheck
>   :: ( Checkable check )
>   => StdGen -> check -> Result
> krebCheck gen =
>   krebCheckWith gen defaultTestArgs

> krebCheckIO
>   :: ( Checkable check )
>   => check -> IO ()
> krebCheckIO ch = do
>   gen <- newStdGen
>   let res = krebCheck gen ch
>   putStr $ prettyResult res
