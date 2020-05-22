> module Kreb.Format.DocTest.Suite where

> import Test.DocTest

> all_doctests :: IO ()
> all_doctests = doctest
>   [ "-XRankNTypes"
>   , "-XGADTs"
>   , "-XScopedTypeVariables"
>   , "-XInstanceSigs"
>   , "-XKindSignatures"
>   , "-XTypeFamilies"
>   , "-XOverloadedStrings"
> 
>   , "-package text"
> 
>   , "src/Kreb/Format.lhs"
>   , "src/Kreb/Format/Util.lhs"
>   , "src/Kreb/Format/Doc.lhs"
>   , "src/Kreb/Format/Combinator.lhs"
>   , "src/Kreb/Format/Display.lhs"
>   ]
