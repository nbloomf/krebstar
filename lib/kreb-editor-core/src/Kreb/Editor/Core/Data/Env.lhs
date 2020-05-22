> module Kreb.Editor.Core.Data.Env (
>     AppEnv(..)
> ) where

> import Kreb.Text.TextBox

> data AppEnv m = AppEnv
>   { textboxFX :: TextBoxFX m
>   }
