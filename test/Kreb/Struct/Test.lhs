> module Main where

> import System.Environment

> import Test.QuickCheck
> import Test.Tasty
> import Test.Tasty.QuickCheck

> import Kreb.Struct.FingerTree.Test
> import Kreb.Struct.FingerTreeZip.Test
> import Kreb.Struct.Seq.Test
> import Kreb.Struct.RunLengthEncoding.Test

> main :: IO ()
> main = do
>   setEnv "TASTY_NUM_THREADS" "6"
>   setEnv "TASTY_QUICKCHECK_MAX_SIZE" "300"
>   setEnv "TASTY_QUICKCHECK_TESTS" "10000"
>   putStrLn "\n"
>   defaultMain $ testGroup "kreb-struct"
>     [ test_FingerTree
>     , test_FingerTreeZip
>     , test_Seq
>     , test_RunLengthEncoding
>     ]
