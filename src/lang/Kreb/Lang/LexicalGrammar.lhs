> module Kreb.Lang.LexicalGrammar where

> data KeyWord
>   = KW_Define
>   | KW_Data
>   | KW_End
>   deriving (Eq, Show)

> data Symbol
>   = SY_DoubleColon
>   | SY_MinusGreater
>   | SY_DoubleEqual
>   | SY_OpenBrack
>   | SY_ClosedBrack
>   | SY_OpenParen
>   | SY_ClosedParen
>   | SY_Dollar
>   | SY_Equal
>   | SY_Pipe
>   | SY_ForAll
>   | SY_Dot
>   deriving (Eq, Show)
