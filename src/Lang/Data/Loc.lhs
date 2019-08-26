> module Lang.Data.Loc where

> data Loc
>   = Q
>   | Loc Int Int Int
>   deriving (Eq, Show)
