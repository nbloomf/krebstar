> module Kreb.Reflect.Test.Suite where

> import Test.Tasty

> import Kreb.Reflect.Nat.Test

> kreb_reflect_test_suite :: TestTree
> kreb_reflect_test_suite =
>   testGroup "All Tests"
>     [ test_ReflectNat
>     ]