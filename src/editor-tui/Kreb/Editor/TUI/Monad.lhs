> {-# LANGUAGE KindSignatures #-}

> module Kreb.Editor.TUI.Monad where

> import Kreb.Control

> import Kreb.Editor.TUI.Data.Layout

> newtype TermUI (m :: * -> *) a = TermUI
>   { unTermUI :: TermEnv m -> TermState m -> m (a, TermState m) }

> runTermUI
>   :: ( Monad m )
>   => TermEnv m -> TermState m -> TermUI m a -> m a
> runTermUI env st (TermUI x) =
>   fst <$> x env st

> instance
>   ( Monad m
>   ) => Functor (TermUI m)
>   where
>     fmap f x = TermUI $ \env st1 -> do
>       (a, st2) <- unTermUI x env st1
>       return (f a, st2)
> 
> instance
>   ( Monad m
>   ) => Applicative (TermUI m)
>   where
>     pure a = TermUI $ \_ st ->
>       return (a, st)
> 
>     f <*> x = TermUI $ \env st1 -> do
>       (g, st2) <- unTermUI f env st1
>       (a, st3) <- unTermUI x env st2
>       return (g a, st3)
> 
> instance
>   ( Monad m
>   ) => Monad (TermUI m)
>   where
>     return = pure
> 
>     x >>= f = TermUI $ \env st1 -> do
>       (a, st2) <- unTermUI x env st1
>       unTermUI (f a) env st2
> 
> instance MonadTrans TermUI where
>   lift x = TermUI $ \_ st -> do
>     a <- x
>     return (a, st)
> 
> instance (LiftIO m) => LiftIO (TermUI m) where
>   liftIO x = TermUI $ \_ st -> do
>     a <- liftIO x
>     return (a, st)

> modifyTermState
>   :: ( Monad m )
>   => (TermState m -> TermState m)
>   -> TermUI m ()
> modifyTermState f = TermUI $ \_ st ->
>   return ((), f st)

> getsTermState
>   :: ( Monad m )
>   => (TermState m -> a) -> TermUI m a
> getsTermState f = TermUI $ \_ st ->
>   return (f st, st)


> data TermEnv (m :: * -> *) = TermEnv

> termEnvIO :: TermEnv IO
> termEnvIO = TermEnv


> data TermState (m :: * -> *) = TermState
>   { screenLayout :: Layout
>   } deriving (Eq, Show)

> initTermState :: TermState m
> initTermState = TermState
>   { screenLayout = emptyLayout
>   }
