> module Kreb.Check.Tasty where

> import Data.Proxy
> import Data.Typeable
> import System.Random (newStdGen)

> import Test.Tasty.Options
> import Test.Tasty.Providers

> import Kreb.Check.Check
> import Kreb.Check.Tests
> import Kreb.Check.StateMachine

> data KT = KT Check
>   deriving Typeable

> instance IsTest KT where
>   testOptions = return
>     [ Option (Proxy :: Proxy KrebCheckTests)
>     , Option (Proxy :: Proxy KrebCheckDiscard)
>     , Option (Proxy :: Proxy KrebCheckSize)
>     , Option (Proxy :: Proxy KrebCheckDepth)
>     , Option (Proxy :: Proxy KrebCheckPrunes)
>     ]
> 
>   run opts (KT ch) _ = do
>     let args = getTestArgs opts
>     gen <- newStdGen
>     let res = krebCheckWith gen args ch
>     return $ if isSuccess res
>       then testPassed $ prettyResult res
>       else testFailed $ prettyResult res

> testKreb
>   :: ( Checkable check )
>   => TestName -> check -> TestTree
> testKreb name ch =
>   singleTest name $ KT $ check ch



> getTestArgs :: OptionSet -> TestArgs
> getTestArgs opts =
>   let
>     KrebCheckTests num    = lookupOption opts
>     KrebCheckDiscard disc = lookupOption opts
>     KrebCheckSize size    = lookupOption opts
>     KrebCheckDepth depth  = lookupOption opts
>     KrebCheckPrunes prune = lookupOption opts
>   in TestArgs
>     { maxSuccess      = num
>     , maxDiscardRatio = disc
>     , maxSize         = size
>     , maxDepth        = depth
>     , maxPrunes       = prune
>     }



> newtype KrebCheckTests
>   = KrebCheckTests Int
>   deriving (Eq, Show)
> 
> instance IsOption KrebCheckTests where
>   defaultValue = KrebCheckTests 100
>   parseValue = fmap KrebCheckTests . safeRead
>   optionName = return "krebcheck-tests"
>   optionHelp = return "Number of test cases to generate"

> newtype KrebCheckDiscard
>   = KrebCheckDiscard Int
>   deriving (Eq, Show)
> 
> instance IsOption KrebCheckDiscard where
>   defaultValue = KrebCheckDiscard 5
>   parseValue = fmap KrebCheckDiscard . safeRead
>   optionName = return "krebcheck-discard"
>   optionHelp = return "Max ratio of discarded cases to successful cases"

> newtype KrebCheckSize
>   = KrebCheckSize Int
>   deriving (Eq, Show)
> 
> instance IsOption KrebCheckSize where
>   defaultValue = KrebCheckSize 100
>   parseValue = fmap KrebCheckSize . safeRead
>   optionName = return "krebcheck-size"
>   optionHelp = return "Maximum size parameter"

> newtype KrebCheckDepth
>   = KrebCheckDepth Int
>   deriving (Eq, Show)
> 
> instance IsOption KrebCheckDepth where
>   defaultValue = KrebCheckDepth 10
>   parseValue = fmap KrebCheckDepth . safeRead
>   optionName = return "krebcheck-depth"
>   optionHelp = return "Maximum depth parameter"

> newtype KrebCheckPrunes
>   = KrebCheckPrunes Int
>   deriving (Eq, Show)
> 
> instance IsOption KrebCheckPrunes where
>   defaultValue = KrebCheckPrunes 200
>   parseValue = fmap KrebCheckPrunes . safeRead
>   optionName = return "krebcheck-prunes"
>   optionHelp = return "Maximum number of pruning steps on failure"
