> module Main where

> import System.Environment

> import Test.Tasty

> import Kreb.Unit.Test.Suite

> main :: IO ()
> main = do
>   setEnv "TASTY_NUM_THREADS" "6"
>   setEnv "TASTY_HIDE_SUCCESSES" "TRUE"
>   putStrLn "\n"
>   defaultMain kreb_unit_test_suite
