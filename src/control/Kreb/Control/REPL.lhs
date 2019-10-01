> {-# LANGUAGE RecordWildCards #-}

> module Kreb.Control.REPL where

> data ReplEnv act env sig st m = ReplEnv
>   { _Init  :: m ()
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

> getSt
>   :: ( Monad m )
>   => (st -> a)
>   -> ReplT act env sig st m a
> getSt f = ReplT $ \_ _ st ->
>   return (f st, st)

> putSt
>   :: ( Monad m )
>   => st -> ReplT act env sig st m ()
> putSt st = ReplT $ \_ _ _ ->
>   return ((), st)

> askEnv
>   :: ( Monad m )
>   => (env -> a)
>   -> ReplT act env sig st m a
> askEnv f = ReplT $ \_ env st ->
>   return (f env, st)

> askReplEnv
>   :: ( Monad m )
>   => (ReplEnv act env sig st m -> a)
>   -> ReplT act env sig st m a
> askReplEnv f = ReplT $ \renv _ st ->
>   return (f renv, st)



> loopRepl
>   :: ( Monad m )
>   => ReplT act env sig st m ()
> loopRepl = do
>   askReplEnv _Init
>   (askReplEnv _Print) <*> (askEnv id) <*> (getSt id)
>   loopRepl'

> loopRepl'
>   :: ( Monad m )
>   => ReplT act env sig st m ()
> loopRepl' = do
>   act <- (askReplEnv _Read) <*> (askEnv id) <*> (getSt id)
>   eval <- (askReplEnv _Eval) <*> (askEnv id) <*> (getSt id)
>   result <- liftReplT (act >>= eval)
>   case result of
>     Left sig -> do
>       exit <- askReplEnv _Exit
>       liftReplT $ exit sig
>     Right st2 -> do
>       (askReplEnv _Print) <*> (askEnv id) <*> (pure st2)
>       putSt st2
>       loopRepl'
