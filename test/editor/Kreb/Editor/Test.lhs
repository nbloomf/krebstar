> module Main where

> import System.Environment

> import Data.Proxy

> import Test.Tasty

> import Kreb.Editor.Test.Suite



> main :: IO ()
> main = do
>   setEnv "TASTY_NUM_THREADS" "6"
>   setEnv "TASTY_QUICKCHECK_MAX_SIZE" "300"
>   setEnv "TASTY_QUICKCHECK_TESTS" "10000"
>   putStrLn "\n"
>   defaultMain kreb_editor_test_suite
