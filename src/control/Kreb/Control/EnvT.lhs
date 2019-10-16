> module Kreb.Control.EnvT where

> import Kreb.Control.Trans

> newtype EnvT r m a = EnvT
>   { unEnvT :: r -> m a }

> runEnvT
>   :: ( Monad m )
>   => r -> EnvT r m a -> m a
> runEnvT r (EnvT x) = x r

> instance
>   ( Monad m
>   ) => Functor (EnvT r m)
>   where
>     fmap f (EnvT x) =
>       EnvT $ \r -> do
>         a <- x r
>         return (f a)

> instance
>   ( Monad m
>   ) => Applicative (EnvT r m)
>   where
>     pure a =
>       EnvT $ \_ -> return a
> 
>     (EnvT f') <*> (EnvT x') =
>       EnvT $ \r -> do
>         f <- f' r
>         a <- x' r
>         return (f a)

> instance
>   ( Monad m
>   ) => Monad (EnvT r m)
>   where
>     return = pure
> 
>     (EnvT x) >>= f =
>       EnvT $ \r -> do
>         a <- x r
>         unEnvT (f a) r

> instance MonadTrans (EnvT r) where
>   lift x = EnvT $ \_ -> x

> askEnvT
>   :: ( Monad m )
>   => EnvT r m r
> askEnvT =
>   EnvT $ \r -> return r

> localEnvT
>   :: ( Monad m )
>   => (r2 -> r1) -> EnvT r1 m a -> EnvT r2 m a
> localEnvT f (EnvT x) =
>   EnvT $ \r -> x (f r)
