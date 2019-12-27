> module Kreb.Editor.Core.Env (
>     AppEnv(..)
> ) where

> import Kreb.Effect

> data AppEnv m = AppEnv
>   { logWriter :: LogWriter m
>   , fileReader :: FileReader m
>   , fileWriter :: FileWriter m
>   }
