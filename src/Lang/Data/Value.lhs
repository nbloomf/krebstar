> module Lang.Data.Value where

> import Data.List (unwords)
> import Lang.Data.Expr
> import Lang.Data.Error
> import Lang.Data.PrettyPrint


> data Val
>   = V_Prim Pri
>   | V_Eff
>   | V_Func String [Val]
>   | V_Quote Phrase
>   deriving (Eq, Show)

> data Pri
>   = Prim_Int Int
>   | Prim_Char Char
>   | Prim_String String
>   deriving (Eq, Show)



> instance PrettyPrint Val where
>   pretty x = case x of
>     V_Prim p -> pretty p
>     V_Eff -> "#Eff"
>     V_Func f xs -> case xs of
>       [] -> f
>       _ -> "(" ++ f ++ " " ++ unwords (map pretty xs) ++ ")"
>     V_Quote expr ->
>       "'[" ++ pretty expr ++ "]"

> instance PrettyPrint Pri where
>   pretty x = case x of
>     Prim_Int k -> show k
>     Prim_Char c -> show c
>     Prim_String s -> show s



> data DataStack
>   = Empty
>   | Cons DataStack Val
>   deriving (Eq, Show)

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

