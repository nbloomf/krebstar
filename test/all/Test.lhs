> module Main where

> import System.Environment

> import Test.Tasty

> import Kreb.Check.Test.Suite
> import Kreb.Reflect.Test.Suite
> import Kreb.Struct.Test.Suite
> import Kreb.Text.Test.Suite
> import Kreb.Lang.Test.Suite
> import Kreb.Editor.Test.Suite

> main :: IO ()
> main = do
>   setEnv "TASTY_NUM_THREADS" "6"
>   setEnv "TASTY_HIDE_SUCCESSES" "TRUE"
>   setEnv "TASTY_KREBCHECK_MAX_SIZE" "300"
>   setEnv "TASTY_KREBCHECK_TESTS" "1000"
>   putStrLn "\n"
>   defaultMain $
>     testGroup "All Tests"
>       [ kreb_check_test_suite
>       , kreb_reflect_test_suite
>       , kreb_struct_test_suite
>       , kreb_text_test_suite
>       , kreb_lang_test_suite
>       , kreb_editor_test_suite
>       ]
