> module Kreb.Struct.DocTest.Suite where

> import Test.DocTest

> all_doctests :: IO ()
> all_doctests = doctest
>   [ "-XRankNTypes"
>   , "-XGADTs"
>   , "-XScopedTypeVariables"
>   , "-XInstanceSigs"
>   , "-XKindSignatures"
>   , "-XTypeFamilies"
> 
>   , "-package text"
>   , "-package kreb-prop"
>   , "-package kreb-category"
> 
>   , "src/Kreb/Struct.lhs"
>   , "src/Kreb/Struct/Data.lhs"
>   , "src/Kreb/Struct/Data/FingerTree.lhs"
>   ]

 >   , "src/struct/Kreb/Struct/Valued.lhs"
 >   , "src/struct/Kreb/Struct/OnePointedList.lhs"
 >   , "src/struct/Kreb/Struct/Sequence.lhs"
 >   , "src/struct/Kreb/Struct/TwoPointedList.lhs"
 >   , "src/struct/Kreb/Struct/RunLengthEncoded.lhs"
 >   , "src/struct/Kreb/Struct/RoseTree.lhs"
 >   , "src/struct/Kreb/Struct/PointedRoseTree.lhs"
 >   , "src/struct/Kreb/Struct/RedBlackTree.lhs"
 >   , "src/struct/Kreb/Struct/FiniteMap.lhs"
