> module Kreb.Editor.Core.Data.Env (
>     AppEnv(..)
> ) where

> import Kreb.Effect

> data AppEnv m = AppEnv
>   { logWriter       :: LogWriter m
>   , fileReader      :: FileReader m
>   , fileWriter      :: FileWriter m
>   , clipboardReader :: ClipboardReader m
>   , clipboardWriter :: ClipboardWriter m
>   }
