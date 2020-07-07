> module Kreb.Text.DocTest.Suite where
> 
> import Test.DocTest
> 
> kreb_text_doctests :: IO ()
> kreb_text_doctests = doctest
>   [ "-XRankNTypes"
>   , "-XGADTs"
>   , "-XScopedTypeVariables"
>   , "-XInstanceSigs"
>   , "-XKindSignatures"
>   , "-XTypeFamilies"
>   , "-XTypeApplications"
> 
>   , "-package kreb-prop"
> 
>   , "src/Kreb/Text/ScreenOffset.lhs"
>   , "src/Kreb/Text/MeasureText.lhs"
>   , "src/Kreb/Text/Pigment.lhs"
>   , "src/Kreb/Text/Grapheme.lhs"
>   , "src/Kreb/Text/Buffer.lhs"
>   , "src/Kreb/Text/BufferOp.lhs"
>   , "src/Kreb/Text/SizedBuffer.lhs"
>   , "src/Kreb/Text/TextBox.lhs"
>   , "src/Kreb/Text/TextBox/EditsT.lhs"
>   , "src/Kreb/Text/TextBox/Action.lhs"
>   , "src/Kreb/Text/CursorWord.lhs"
>   ]
