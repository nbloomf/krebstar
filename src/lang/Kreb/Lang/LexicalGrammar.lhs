> module Kreb.Lang.LexicalGrammar where

> data KeyWord
>   = KW_Define
>   | KW_Data
>   | KW_End
>   deriving (Eq, Show)

> data Symbol
>   = SY_ClosedBrack
>   | SY_ClosedParen
>   | SY_Dollar
>   | SY_Dot
>   | SY_DoubleColon
>   | SY_DoubleEqual
>   | SY_DoubleQuote
>   | SY_Equal
>   | SY_ForAll
>   | SY_MinusGreater
>   | SY_OpenBrack
>   | SY_OpenParen
>   | SY_Pipe
>   | SY_SingleQuote
>   deriving (Eq, Show)
