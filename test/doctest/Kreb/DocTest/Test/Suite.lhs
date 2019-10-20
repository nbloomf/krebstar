> module Kreb.DocTest.Test.Suite where

> import Test.DocTest

> all_doctests :: IO ()
> all_doctests = doctest
>   [ "src/check/Kreb/Check.lhs"
>   , "src/check/Kreb/Check/Seeded.lhs"
>   , "src/check/Kreb/Check/Arb.lhs"
>   , "src/check/Kreb/Check/Check.lhs"
>   , "src/check/Kreb/Check/Tests.lhs"
>   , "src/check/Kreb/Check/Tasty.lhs"
>   , "src/check/Kreb/Check/Alg.lhs"
>   , "src/check/Kreb/Check/Fun.lhs"
>   , "src/check/Kreb/Check/Build.lhs"
>   , "src/check/Kreb/Check/StateMachine.lhs"
>   , "src/check/Kreb/Check/Laws.lhs"
>   , "src/check/Kreb/Check/Laws/Eq.lhs"
>   , "src/check/Kreb/Check/Laws/Semigroup.lhs"
>   , "src/check/Kreb/Check/Laws/Monoid.lhs"
>   , "src/check/Kreb/Check/Laws/Functor.lhs"
>   , "src/check/Kreb/Check/Laws/Foldable.lhs"
>   , "src/check/Kreb/Check/Laws/Ringlike.lhs"
> 
>   , "src/control/Kreb/Control.lhs"
>   , "src/control/Kreb/Control/LocalSt.lhs"
>   , "src/control/Kreb/Control/ReplT.lhs"
>   , "src/control/Kreb/Control/Trans.lhs"
>   , "src/control/Kreb/Control/StateT.lhs"
>   , "src/control/Kreb/Control/EnvT.lhs"
> 
>   , "src/reflect/Kreb/Reflect.lhs"
>   , "src/reflect/Kreb/Reflect/Nat.lhs"
> 
>   , "src/struct/Kreb/Struct.lhs"
>   , "src/struct/Kreb/Struct/Valued.lhs"
>   , "src/struct/Kreb/Struct/FingerTree.lhs"
>   , "src/struct/Kreb/Struct/OnePointedList.lhs"
>   , "src/struct/Kreb/Struct/Seq.lhs"
>   , "src/struct/Kreb/Struct/TwoPointedList.lhs"
>   , "src/struct/Kreb/Struct/RunLengthEncoded.lhs"
> 
>   , "src/text/Kreb/Text.lhs"
>   , "src/text/Kreb/Text/ScreenOffset.lhs"
>   , "src/text/Kreb/Text/MeasureText.lhs"
>   , "src/text/Kreb/Text/Glyph.hs"
>   , "src/text/Kreb/Text/Cell.lhs"
>   , "src/text/Kreb/Text/Buffer.lhs"
>   , "src/text/Kreb/Text/TextBox.lhs"
>   ]