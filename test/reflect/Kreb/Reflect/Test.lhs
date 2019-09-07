> module Main where

> import System.Environment
> import Data.Proxy

> import Test.Tasty

> import Kreb.Reflect.Nat.Test

> main :: IO ()
> main = do
>   setEnv "TASTY_NUM_THREADS" "6"
>   setEnv "TASTY_KREBCHECK_MAX_SIZE" "300"
>   setEnv "TASTY_KREBCHECK_TESTS" "1000"
>   putStrLn "\n"
> 
>   defaultMain $ testGroup "All Tests"
>     [ test_ReflectNat
>     ]