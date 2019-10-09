> module Main where

> import Test.DocTest

> main :: IO ()
> main = doctest
>   [ "src/check/Kreb/Check.lhs"
>   , "src/check/Kreb/Check/Seeded.lhs"
>   , "src/check/Kreb/Check/Arb.lhs"
>   , "src/check/Kreb/Check/Check.lhs"
>   , "src/check/Kreb/Check/Tests.lhs"
>   , "src/check/Kreb/Check/Tasty.lhs"
>   , "src/check/Kreb/Check/Alg.lhs"
>   , "src/check/Kreb/Check/Laws.lhs"
>   , "src/check/Kreb/Check/Laws/Eq.lhs"
>   , "src/check/Kreb/Check/Laws/Semigroup.lhs"
>   , "src/check/Kreb/Check/Laws/Monoid.lhs"
>   , "src/check/Kreb/Check/Laws/Functor.lhs"
>   , "src/check/Kreb/Check/Laws/Foldable.lhs"
>   , "src/check/Kreb/Check/Laws/Ringlike.lhs"
>   , "src/check/Kreb/Check/Fun.lhs"
>   , "src/check/Kreb/Check/Build.lhs"
>   , "src/check/Kreb/Check/StateMachine.lhs"
>   , "src/control/Kreb/Control/ReplT.lhs"
>   , "src/struct/Kreb/Struct/FingerTree.lhs"
>   , "src/struct/Kreb/Struct/OnePointedList.lhs"
>   ]
