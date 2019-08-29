> module Ned.App.Hooks where

> import Control.Monad (ap)

> import Lang
> import Ned.App.State

> newtype Hook m a = Hook
>   { unHook :: AppState m -> m (a, AppState m)
>   }
> 
> instance
>   ( Monad m
>   ) => Monad (Hook m) where
>     return a = Hook $ \st -> return (a, st)
> 
>     (Hook x) >>= f = Hook $ \st -> do
>       (a, st2) <- x st
>       unHook (f a) st2
> 
> instance ( Monad m ) => Applicative (Hook m) where
>   pure = return
>   (<*>) = ap
> 
> instance ( Monad m ) => Functor (Hook m) where
>   fmap f x = x >>= (return . f)

> runHook
>   :: ( Monad m )
>   => Hook m a -> AppState m -> m (a, (AppState m))
> runHook (Hook x) st = x st

> evalHook
>   :: ( Monad m )
>   => String -> AppState m -> m (Either (Either Error ReplError) (AppState m))
> evalHook str st =
>   case runParser pPhrase str of
>     Left err -> return $ Left (Left err)
>     Right ph -> do
>       (r, x) <- runHook (Hook $ \z -> do {y <- evalRuntime (doActionFor ph) (runtimeSt st); return (y,z)}) st
>       case r of
>         Left err -> return $ Left (Right err)
>         Right () -> return $ Right x


> runtimeStateIO :: RuntimeState IO
> runtimeStateIO =
>   initRuntimeState editorActionsIO editorTypes

> editorTypes
>   :: String -> Maybe Scheme
> editorTypes str = case str of
>   _ -> Nothing

> editorActionsIO
>   :: String -> Maybe (Runtime IO ())
> editorActionsIO str = case str of
>   _ -> Nothing
