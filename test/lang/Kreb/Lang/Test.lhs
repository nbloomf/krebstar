> module Main where

> import System.Environment

> import Data.Proxy

> import Test.QuickCheck
> import Test.Tasty
> import Test.Tasty.QuickCheck

> import Kreb.Lang.Type.Test
> import Kreb.Lang.Module.Test
> import Kreb.Lang.Interpreter.Test
> import Kreb.Lang.Parser.Test


> main :: IO ()
> main = do
>   setEnv "TASTY_NUM_THREADS" "6"
>   setEnv "TASTY_QUICKCHECK_MAX_SIZE" "300"
>   setEnv "TASTY_QUICKCHECK_TESTS" "10000"
>   putStrLn "\n"
>   defaultMain $ testGroup "All Tests"
>     [ test_Type
>     , test_Module
>     , test_Interpreter
>     , test_Parser
>     ]
