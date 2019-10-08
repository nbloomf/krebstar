> module Kreb.Lang.Value where

> import Kreb.Format
> import Data.List (unwords, intercalate)
> import Kreb.Lang.Expr
> import Kreb.Lang.Error
> import Kreb.Lang.PrettyPrint


> data Val
>   = V_Prim Pri
>   | V_Eff
>   | V_Func String [Val]
>   | V_Quote [Sus]
>   deriving (Eq, Show)

> instance DisplayNeat Val where
>   displayNeat x = case x of
>     V_Prim p -> displayNeat p
>     V_Eff -> "@Eff"
>     V_Func str vs -> case vs of
>       [] -> str
>       _ -> str ++ " " ++ intercalate " " (map (paren . displayNeat) vs)
>       where paren str = if elem ' ' str then "(" ++ str ++ ")" else str
>     V_Quote ps -> "[" ++ (intercalate " " $ map displayNeat ps) ++ "]"

> data Sus
>   = Sus_Put Val
>   | Sus_Say Phrase
>   deriving (Eq, Show)

> instance DisplayNeat Sus where
>   displayNeat x = case x of
>     Sus_Put z -> displayNeat z
>     Sus_Say z -> displayNeat z

> data Pri
>   = Prim_Int Int
>   | Prim_Char Char
>   | Prim_String String
>   deriving (Eq, Show)

> instance DisplayNeat Pri where
>   displayNeat x = case x of
>     Prim_Int k -> show k
>     Prim_Char c -> show c
>     Prim_String s -> show s



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

> instance DisplayNeat DataStack where
>   displayNeat x = case x of
>     Empty -> "ok."
>     Cons Empty v -> displayNeat v
>     Cons st v -> concat
>       [ disp st, " ", displayNeat v ]
>       where
>         disp w = case w of
>           Empty -> ""
>           Cons Empty k -> displayNeat k
>           Cons h k -> concat [disp h, " ", displayNeat k]

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

