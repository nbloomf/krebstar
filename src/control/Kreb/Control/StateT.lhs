> module Kreb.Control.StateT where

> import Kreb.Control.Trans

> newtype StateT s m a
>   = StateT (s -> m (a, s))

> instance
>   ( Monad m
>   ) => Functor (StateT s m)
>   where
>     fmap f (StateT x) = StateT $ \st1 -> do
>       (a, st2) <- x st1
>       return (f a, st2)

> evalStateT
>   :: ( Monad m )
>   => s -> StateT s m a -> m (a, s)
> evalStateT s (StateT f) = f s

> runStateT
>   :: ( Monad m )
>   => s -> StateT s m a -> m a
> runStateT s (StateT f) = fst <$> f s

> instance
>   ( Monad m
>   ) => Applicative (StateT s m)
>   where
>     pure a = StateT $ \st ->
>       return (a, st)
> 
>     (StateT f') <*> (StateT x') =
>       StateT $ \st1 -> do
>         (f, st2) <- f' st1
>         (x, st3) <- x' st2
>         return (f x, st3)

> instance
>   ( Monad m
>   ) => Monad (StateT s m)
>   where
>     return = pure
> 
>     (StateT x) >>= f =
>       StateT $ \st1 -> do
>         (a, st2) <- x st1
>         let StateT y = f a
>         y st2

> instance MonadTrans (StateT s) where
>   lift x = StateT $ \st -> do
>     a <- x
>     return (a, st)

> getStateT
>   :: ( Monad m )
>   => StateT s m s
> getStateT = StateT $ \st ->
>   return (st, st)

> putStateT
>   :: ( Monad m )
>   => s -> StateT s m ()
> putStateT st = StateT $ \_ ->
>   return ((), st)

> mutateStateT
>   :: ( Monad m )
>   => (s -> s) -> StateT s m ()
> mutateStateT f = StateT $ \st ->
>   return ((), f st)
