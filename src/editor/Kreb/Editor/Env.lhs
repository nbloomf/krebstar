> module Kreb.Editor.Env (
>     AppEnv(..)
> ) where

> -- Side effects are managed here.
> data AppEnv m = AppEnv
>   { logMessage :: String -> m ()
> 
>   , loadFile :: FilePath -> m (Either IOError String)
>   , saveFile :: FilePath -> String -> m (Maybe IOError)
>   }
