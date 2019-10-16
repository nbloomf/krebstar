> {-# LANGUAGE KindSignatures #-}

> module Kreb.Editor.CLI.Monad where

> import Kreb.Control

> newtype TermUI (m :: * -> *) a = TermUI
>   { unTermUI :: EnvT (TermEnv m) (StateT (TermState m) m) a }

> runTermUI
>   :: ( Monad m )
>   => TermEnv m -> TermState m -> TermUI m a -> m a
> runTermUI env st =
>   runStateT st . runEnvT env . unTermUI

> instance
>   ( Monad m
>   ) => Functor (TermUI m)
>   where
>     fmap f = TermUI . fmap f . unTermUI
> 
> instance
>   ( Monad m
>   ) => Applicative (TermUI m)
>   where
>     pure = TermUI . pure
> 
>     (TermUI f) <*> (TermUI x) =
>       TermUI (f <*> x)
> 
> instance
>   ( Monad m
>   ) => Monad (TermUI m)
>   where
>     return = pure
> 
>     (TermUI x) >>= f =
>       TermUI (x >>= (unTermUI . f))
> 
> instance MonadTrans TermUI where
>   lift = TermUI . lift . lift


> data TermEnv (m :: * -> *) = TermEnv

> termEnvIO :: TermEnv IO
> termEnvIO = TermEnv


> data TermState (m :: * -> *) = TermState

> initTermState :: TermState m
> initTermState = TermState
