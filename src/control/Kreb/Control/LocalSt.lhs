> module Kreb.Control.LocalSt where

> import Kreb.Control.Trans.Class

> newtype LocalT s m a = LocalT
>   { unLocalT :: s -> m (a, s) }

> runLocalT
>   :: ( Monad m )
>   => LocalT s m () -> s -> m s
> runLocalT (LocalT x) st =
>   fmap snd $ x st

> instance
>   ( Monad m
>   ) => Functor (LocalT s m)
>   where
>     fmap f x = LocalT $ \st1 -> do
>       (a, st2) <- unLocalT x st1
>       return (f a, st2)
> 
> instance
>   ( Monad m
>   ) => Applicative (LocalT s m)
>   where
>     pure a = LocalT $ \st ->
>       return (a, st)
> 
>     f <*> x = LocalT $ \st1 -> do
>       (g, st2) <- unLocalT f st1
>       (a, st3) <- unLocalT x st2
>       return (g a, st3)
> 
> instance
>   ( Monad m
>   ) => Monad (LocalT s m)
>   where
>     return = pure
> 
>     x >>= f = LocalT $ \st1 -> do
>       (a, st2) <- unLocalT x st1
>       unLocalT (f a) st2

> instance MonadTrans (LocalT s) where
>   lift x = LocalT $ \st -> do
>     a <- x
>     return (a, st)

> getsLocalT
>   :: ( Monad m )
>   => (s -> a) -> LocalT s m a
> getsLocalT f = LocalT $ \st ->
>   return (f st, st)
> 
> mutateLocalT
>   :: ( Monad m )
>   => (s -> s) -> LocalT s m ()
> mutateLocalT f = LocalT $ \st ->
>   return ((), f st)



> newtype Local s a = Local
>   { unLocal :: s -> (s,a)
>   }
> 
> localSt
>   :: s -> Local s a -> s
> localSt s x =
>   fst $ unLocal x s
> 
> instance Functor (Local s) where
>   fmap f x = Local $ \s1 ->
>     let
>       (s2, a) = unLocal x s1
>     in (s2, f a)
> 
> instance Applicative (Local s) where
>   pure a = Local $ \s -> (s, a)
> 
>   f <*> x = Local $ \s1 ->
>     let
>       (s2, f') = unLocal f s1
>       (s3, x') = unLocal x s2
>     in (s3, f' x')
> 
> instance Monad (Local s) where
>   return = pure
> 
>   x >>= f = Local $ \s1 ->
>     let
>       (s2, x') = unLocal x s1
>     in unLocal (f x') s2

> readSt
>   :: (s -> t) -> Local s t
> readSt f = Local $ \s -> (s, f s)

> editSt
>   :: (s -> s) -> Local s ()
> editSt f = Local $ \s -> (f s, ())









