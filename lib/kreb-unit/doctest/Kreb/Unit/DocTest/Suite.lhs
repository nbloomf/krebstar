> module Kreb.Unit.DocTest.Suite where
> 
> import Test.DocTest
> 
> kreb_unit_doctests :: IO ()
> kreb_unit_doctests = doctest
>   [ "-XRankNTypes"
>   , "-XGADTs"
>   , "-XScopedTypeVariables"
>   , "-XInstanceSigs"
>   , "-XKindSignatures"
>   , "-XTypeFamilies"
> 
>   , "-package text"
>   , "-package tasty"
> 
>   , "src/Kreb/Unit.lhs"
>   , "src/Kreb/Unit/Declare.lhs"
>   , "src/Kreb/Unit/Declarable.lhs"
>   , "src/Kreb/Unit/Helpers.lhs"
>   , "src/Kreb/Unit/Tasty.lhs"
>   ]
