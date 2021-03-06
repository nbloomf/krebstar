> {-# LANGUAGE OverloadedStrings #-}

> module Kreb.Lang.Expr where

> import Prelude hiding (Word)

> import qualified Kreb.Format as Fmt
> import           Kreb.Format ((<+>), display, string, bracks)
> import           Kreb.Prop

> import Kreb.Lang.PrettyPrint


Term Grammar
============

> data Atom
>   = Atom String
>   deriving (Eq, Ord, Show)
> 
> instance Fmt.Display Atom where
>   display (Atom str) = string str
> 
> instance Arb Atom where
>   arb = do
>     k <- randIn (1::Int,7)
>     Atom <$> vectOf k arbAsciiChar
> 
> instance Prune Atom where
>   prune (Atom s) =
>     map Atom $ prune s
> 
> data Word
>   = Noop
>   | Only Atom
>   | Quote Phrase
>   | BuiltIn BuiltIn
>   deriving (Eq, Show)
> 
> instance Fmt.Display Word where
>   display w = case w of
>     Noop -> "noop"
>     Only a -> display a
>     Quote ph -> case ph of
>       Silence -> "[]"
>       _       -> bracks (display ph)
>     BuiltIn b -> display b
> 
> instance Arb Word where
>   arb = do
>     k <- size
>     p <- arb
>     if p || (k <= 0)
>       then pickFrom3
>         ( pure Noop
>         , Only <$> arb
>         , BuiltIn <$> arb
>         )
>       else adjustSize (`div` 2) $ pickFrom4
>         ( pure Noop
>         , Only <$> arb
>         , Quote <$> arb
>         , BuiltIn <$> arb
>         )
> 
> instance Prune Word where
>   prune z = case z of
>     Noop -> []
>     Only a -> map Only $ prune a
>     Quote q -> map Quote $ prune q
>     BuiltIn b -> map BuiltIn $ prune b

> 
> data Phrase
>   = Silence
>   | Then Word Phrase
>   deriving (Eq, Show)
> 
> instance Fmt.Display Phrase where
>   display ph = case ph of
>     Silence -> ""
>     Then w ph -> case ph of
>       Silence -> display w
>       _       -> display w <+> display ph
> 
> instance Arb Phrase where
>   arb = do
>     k <- size
>     p <- arb
>     if p || (k <= 0)
>       then return Silence
>       else adjustSize (`div` 2)
>         (Then <$> arb <*> arb)
> 
> instance Prune Phrase where
>   prune z = case z of
>     Silence -> []
>     Then w p ->
>       [ p ] ++
>       [ Then u p | u <- prune w ] ++
>       [ Then w u | u <- prune p ]

> data BuiltIn
>   = BuiltIn_Int Int
>   | BuiltIn_Char Char
>   | BuiltIn_String String
> 
>   | BuiltIn_Int_Plus
>   | BuiltIn_Int_Times
> 
>   | BuiltIn_String_Concat
> 
>   | BuiltIn_Id
>   | BuiltIn_Swap
>   | BuiltIn_Apply
>   | BuiltIn_Quote
>   | BuiltIn_Compose
> 
>   | BuiltIn_Repeat
> 
>   | BuiltIn_Ext String
>   deriving (Eq, Ord, Show)
> 
> instance Fmt.Display BuiltIn where
>   display x = case x of
>     BuiltIn_Int k -> display k
>     BuiltIn_Char c -> string $ show c
>     BuiltIn_String s -> string $ show s
> 
>     BuiltIn_Int_Plus -> "#int_plus"
>     BuiltIn_Int_Times -> "#int_times"
>     BuiltIn_String_Concat -> "#string_concat"
> 
>     BuiltIn_Id -> "#id"
>     BuiltIn_Swap -> "#swap"
>     BuiltIn_Apply -> "#apply"
>     BuiltIn_Quote -> "#quote"
>     BuiltIn_Compose -> "#compose"
> 
>     BuiltIn_Repeat -> "#repeat"
>     BuiltIn_Ext s -> string s
> 
> instance Arb BuiltIn where
>   arb = selectFrom
>     [ BuiltIn_Int <$> arb
>     , BuiltIn_Char <$> arb
>     , BuiltIn_String <$> arb
>     , return BuiltIn_Int_Plus
>     , return BuiltIn_Int_Times
>     , return BuiltIn_String_Concat
>     , return BuiltIn_Id
>     , return BuiltIn_Swap
>     , return BuiltIn_Apply
>     , return BuiltIn_Quote
>     , return BuiltIn_Compose
>     , return BuiltIn_Repeat
>     , BuiltIn_Ext <$> arb
>     ]
> 
> instance Prune BuiltIn where
>   prune z = case z of
>     BuiltIn_Int k -> map BuiltIn_Int $ prune k
>     BuiltIn_Char c -> map BuiltIn_Char $ prune c
>     BuiltIn_String s -> map BuiltIn_String $ prune s
>     BuiltIn_Ext s -> map BuiltIn_Ext $ prune s
>     _ -> []


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
>     BuiltIn_String_Concat -> "#string_concat"
> 
>     BuiltIn_Id -> "#id"
>     BuiltIn_Swap -> "#swap"
>     BuiltIn_Apply -> "#apply"
>     BuiltIn_Quote -> "#quote"
>     BuiltIn_Compose -> "#compose"
> 
>     BuiltIn_Repeat -> "#repeat"

> readStrChr :: String -> Char
> readStrChr cs = case cs of
>   [c]    -> c
>   '\\':[c] -> case c of
>     'n' -> '\n'
>     't' -> '\t'
>     'r' -> '\r'
>     _   -> c
>   _ -> error $ "Parsing error: unrecognized StrChr " ++ cs
