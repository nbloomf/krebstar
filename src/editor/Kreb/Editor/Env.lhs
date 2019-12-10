> module Kreb.Editor.Env (
>     AppEnv(..)
> ) where

> import Kreb.Effect

> -- Side effects are managed here.
> data AppEnv m = AppEnv
>   { logMessage :: String -> m ()
> 
>   , fileReader :: FileReader m
>   , fileWriter :: FileWriter m
>   }
