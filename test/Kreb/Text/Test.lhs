> module Main where

> import System.Environment

> import Test.QuickCheck
> import Test.Tasty
> import Test.Tasty.QuickCheck

> import Kreb.Text.Buffer.Test
> import Kreb.Text.TextBox.Test
> import Kreb.Text.MeasureText.Test
> import Kreb.Text.ScreenOffset.Test

> main :: IO ()
> main = do
>   setEnv "TASTY_NUM_THREADS" "6"
>   setEnv "TASTY_QUICKCHECK_MAX_SIZE" "300"
>   setEnv "TASTY_QUICKCHECK_TESTS" "10000"
>   putStrLn "\n"
>   defaultMain $ testGroup "kreb-struct"
>     [ test_ScreenOffset
>     , test_MeasureText
>     , test_Buffer
>     , test_TextBox
>     ]
