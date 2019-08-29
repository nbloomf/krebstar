> module Lang.Data.Expr where

> import Prelude hiding (Word)

> import Lang.Data.PrettyPrint


Term Grammar
============

> data Atom
>   = Atom String
>   deriving (Eq, Ord, Show)
> 
> data Word
>   = Only Atom
>   | Quote Phrase
>   | BuiltIn BuiltIn
>   deriving (Eq, Show)
> 
> data Phrase
>   = Silence
>   | Then Word Phrase
>   deriving (Eq, Show)

> data BuiltIn
>   = BuiltIn_Int Int
>   | BuiltIn_Char Char
>   | BuiltIn_String String
> 
>   | BuiltIn_Int_Plus
>   | BuiltIn_Int_Times
> 
>   | BuiltIn_Id
>   | BuiltIn_Swap
>   | BuiltIn_Apply
>   | BuiltIn_Quote
>   | BuiltIn_Compose
> 
>   | BuiltIn_Ext String
>   deriving (Eq, Ord, Show)


> instance PrettyPrint Atom where
>   pretty (Atom x) = x
> 
> instance PrettyPrint Word where
>   pretty z = case z of
>     Only a -> pretty a
>     Quote p -> "[" ++ pretty p ++ "]"
>     BuiltIn b -> pretty b
> 
> instance PrettyPrint Phrase where
>   pretty z = case z of
>     Silence -> ""
>     Then w Silence -> pretty w
>     Then w r -> pretty w ++ " " ++ pretty r
> 
> instance PrettyPrint BuiltIn where
>   pretty z = case z of
>     BuiltIn_Int k -> show k
>     BuiltIn_Char c -> show c
>     BuiltIn_String s -> show s
> 
>     BuiltIn_Int_Plus -> "#int_plus"
>     BuiltIn_Int_Times -> "#int_times"
> 
>     BuiltIn_Id -> "#id"
>     BuiltIn_Swap -> "#swap"
>     BuiltIn_Apply -> "#apply"
>     BuiltIn_Quote -> "#quote"
>     BuiltIn_Compose -> "#compose"
















