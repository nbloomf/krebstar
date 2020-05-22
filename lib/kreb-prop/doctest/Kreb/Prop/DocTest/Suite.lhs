> module Kreb.Prop.DocTest.Suite where

> import Test.DocTest

> all_doctests :: IO ()
> all_doctests = doctest
>   [ "-XRankNTypes"
>   , "-XGADTs"
>   , "-XScopedTypeVariables"
>   , "-XInstanceSigs"
>   , "-XKindSignatures"
>   , "-XTypeFamilies"
>   , "-XTypeApplications"
>   , "-XOverloadedStrings"
> 
>   , "-package random"
>   , "-package containers"
> 
>   , "src/Kreb/Prop/Sample.lhs"
>   , "src/Kreb/Prop/Arb.lhs"
>   , "src/Kreb/Prop/Check.lhs"
>   , "src/Kreb/Prop/Tests.lhs"
>   ]
