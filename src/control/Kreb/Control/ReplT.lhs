---
title: "The Read, Eval, Print Loop"
subtitle: "Kreb.Control.ReplT"
---



> module Kreb.Control.ReplT where

> data ReplEnv act env sig st m = ReplEnv
>   { _Init  :: env -> st -> m (Either sig st)
>   , _Read  :: env -> st -> m act
>   , _Eval  :: env -> st -> act -> m (Either sig st)
>   , _Print :: env -> st -> m ()
>   , _Exit  :: sig -> m ()
>   }

> newtype ReplT act env sig st m a = ReplT
>   { unReplT
>       :: ReplEnv act env sig st m
>       -> env -> st
>       -> m (a, st)
>   }

> _init
>   :: ( Monad m )
>   => ReplT act env sig st m (Maybe sig)
> _init = ReplT $ \replEnv env st1 -> do
>   result <- _Init replEnv env st1
>   case result of
>     Left sig -> return (Just sig, st1)
>     Right st2 -> return (Nothing, st2)

> _read
>   :: ( Monad m )
>   => ReplT act env sig st m act
> _read = ReplT $ \replEnv env st -> do
>   act <- _Read replEnv env st
>   return (act, st)

> _eval
>   :: ( Monad m )
>   => act -> ReplT act env sig st m (Maybe sig)
> _eval act = ReplT $ \replEnv env st1 -> do
>   result <- _Eval replEnv env st1 act
>   return $ case result of
>     Right st2 -> (Nothing, st2)
>     Left sig -> (Just sig, st1)

> _print
>   :: ( Monad m )
>   => ReplT act env sig st m ()
> _print = ReplT $ \replEnv env st -> do
>   _Print replEnv env st
>   return ((), st)

> _exit
>   :: ( Monad m )
>   => sig -> ReplT act env sig st m ()
> _exit sig = ReplT $ \replEnv _ st -> do
>   _Exit replEnv sig
>   return ((), st)

> runReplT
>   :: ( Monad m )
>   => ReplEnv act env sig st m -> env -> st
>   -> ReplT act env sig st m a -> m a
> runReplT next env st (ReplT x) =
>   fst <$> x next env st

> instance
>   ( Monad m
>   ) => Functor (ReplT act env sig st m)
>   where
>     fmap f (ReplT x) =
>       ReplT $ \next env st1 -> do
>         (a, st2) <- x next env st1
>         return (f a, st2)

> instance
>   ( Monad m
>   ) => Applicative (ReplT act env sig st m)
>   where
>     pure a = ReplT $ \_ _ st ->
>       pure (a, st)
> 
>     (ReplT f) <*> (ReplT x) =
>       ReplT $ \next env st1 -> do
>         (g, st2) <- f next env st1
>         (a, st3) <- x next env st2
>         return (g a, st3)

> instance
>   ( Monad m
>   ) => Monad (ReplT act env sig st m)
>   where
>     return = pure
> 
>     (ReplT x) >>= f =
>       ReplT $ \next env st1 -> do
>         (a, st2) <- x next env st1
>         unReplT (f a) next env st2

> liftReplT
>   :: ( Monad m )
>   => m a -> ReplT act env sig st m a
> liftReplT x = ReplT $ \_ _ st -> do
>   a <- x
>   return (a, st)





> loopReplT
>   :: ( Monad m )
>   => ReplT act env sig st m ()
> loopReplT = do
>   result <- _init
>   case result of
>     Nothing -> _print >> _loop
>     Just sig -> _exit sig

> _loop
>   :: ( Monad m )
>   => ReplT act env sig st m ()
> _loop = do
>   result <- _read >>= _eval
>   case result of
>     Nothing -> _print >> _loop
>     Just sig -> _exit sig


