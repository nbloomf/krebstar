> module Main where

> import System.Environment

> import Test.Tasty

> import Kreb.Category.Test.Suite

> main :: IO ()
> main = do
>   setEnv "TASTY_NUM_THREADS" "6"
>   setEnv "TASTY_HIDE_SUCCESSES" "TRUE"
>   setEnv "TASTY_KREBCHECK_MAX_SIZE" "300"
>   setEnv "TASTY_KREBCHECK_TESTS" "1000"
>   putStrLn "\n"
>   defaultMain kreb_category_test_suite
