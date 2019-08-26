> module Lang.Data.PrettyPrint where

> class PrettyPrint t where
>   pretty :: t -> String