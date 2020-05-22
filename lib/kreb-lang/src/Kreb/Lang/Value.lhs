> {-# LANGUAGE OverloadedStrings #-}

> module Kreb.Lang.Value where

> import Data.List (unwords, intercalate)

> import qualified Kreb.Format as Fmt
> import           Kreb.Format ((<+>), char, emptyDoc, string, parens, display)

> import Kreb.Lang.Expr
> import Kreb.Lang.Error
> import Kreb.Lang.PrettyPrint


> data Val
>   = V_Prim Pri
>   | V_Eff
>   | V_Func String [Val]
>   | V_Quote [Sus]
>   deriving (Eq, Show)

> instance Fmt.Display Val where
>   display x = case x of
>     V_Prim p -> display p
>     V_Eff -> "@Eff"
>     V_Func str vs -> case vs of
>       [] -> string str
>       _  -> string str <+> (foldr (<+>) emptyDoc $ map (parens . display) vs)
>     V_Quote ps -> char '[' <+> (foldr (<+>) emptyDoc $ map display ps) <+> char ']'

> data Sus
>   = Sus_Put Val
>   | Sus_Say Phrase
>   deriving (Eq, Show)

> instance Fmt.Display Sus where
>   display x = case x of
>     Sus_Put z -> display z
>     Sus_Say z -> display z

> data Pri
>   = Prim_Int Int
>   | Prim_Char Char
>   | Prim_String String
>   deriving (Eq, Show)

> instance Fmt.Display Pri where
>   display x = case x of
>     Prim_Int k -> display k
>     Prim_Char c -> string $ show c
>     Prim_String s -> string $ show s



> instance PrettyPrint Val where
>   pretty x = case x of
>     V_Prim p -> pretty p
>     V_Eff -> "@Eff"
>     V_Func f xs -> case xs of
>       [] -> f
>       _ -> "(" ++ f ++ " " ++ unwords (map pretty xs) ++ ")"
>     V_Quote s ->
>       "'[" ++ (unwords $ map pretty s) ++ "]"

> instance PrettyPrint Sus where
>   pretty x = case x of
>     Sus_Put v -> pretty v
>     Sus_Say p -> pretty p

> instance PrettyPrint Pri where
>   pretty x = case x of
>     Prim_Int k -> show k
>     Prim_Char c -> show c
>     Prim_String s -> show s



> data DataStack
>   = Empty
>   | Cons DataStack Val
>   deriving (Eq, Show)

> instance Fmt.Display DataStack where
>   display x = case x of
>     Empty -> "ok."
>     Cons Empty v -> display v
>     Cons st v -> disp st <+> display v
>       where
>         disp w = case w of
>           Empty -> ""
>           Cons Empty k -> display k
>           Cons h k -> disp h <+> display k

> instance PrettyPrint DataStack where
>   pretty x = case x of
>     Empty -> ""
>     Cons st v -> pretty st ++ " " ++ pretty v

> peel :: Int -> DataStack -> Either RuntimeError (DataStack, [Val])
> peel k st =
>   if k <= 0
>     then Right (st, [])
>     else case st of
>       Empty -> Left StackHeadMismatch
>       Cons st1 v -> do
>         (st2, vs) <- peel (k-1) st1
>         return (st2, v:vs)

> peelAndStick
>   :: String -> Int -> DataStack -> Either RuntimeError DataStack
> peelAndStick name num st = do
>   (st', vals) <- peel num st
>   return $ Cons st' (V_Func name vals)

