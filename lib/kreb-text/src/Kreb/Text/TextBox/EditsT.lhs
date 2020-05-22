> module Kreb.Text.TextBox.EditsT (
>     EditsT()
>   , runEditsT
>   , evalEditsT
> 
>   , query
>   , edit
>   , editWithOutput
> 
>   , askEventId
>   , writeLog
> ) where

> import Kreb.Control
> import Kreb.Effect

> import Kreb.Text.MeasureText

> newtype EditsT s m a = EditsT
>   { unEditsT :: EnvT (EditsEnv m) (StateT s m) a
>   }

> data EditsEnv m = EditsEnv
>   { eventId :: EventId
>   , logger :: LogWriter m
>   }



> instance (Monad m) => Functor (EditsT s m) where
>   fmap f = EditsT . fmap f . unEditsT
> 
> instance (Monad m) => Applicative (EditsT s m) where
>   pure = EditsT . pure
>   (EditsT f) <*> (EditsT x) = EditsT (f <*> x)
> 
> instance (Monad m) => Monad (EditsT s m) where
>   return = pure
>   (EditsT x) >>= f = EditsT (x >>= (unEditsT . f))
> 
> instance MonadTrans (EditsT s) where
>   lift = EditsT . lift . lift



> runEditsT
>   :: ( Monad m )
>   => LogWriter m -> EventId -> s -> EditsT s m ()
>   -> m s
> runEditsT writer eId s x =
>   fmap snd
>     $ evalStateT s
>     $ runEnvT (EditsEnv eId writer)
>     $ unEditsT x

> evalEditsT
>   :: ( Monad m )
>   => LogWriter m -> EventId -> s -> EditsT s m a
>   -> m (a,s)
> evalEditsT writer eId s x =
>   evalStateT s
>     $ runEnvT (EditsEnv eId writer)
>     $ unEditsT x



> query
>   :: ( Monad m )
>   => (s -> u)
>   -> EditsT s m u
> query = EditsT . lift . gets
> 
> edit
>   :: ( Monad m )
>   => (s -> s)
>   -> EditsT s m ()
> edit = EditsT . lift . mutate
> 
> editWithOutput
>   :: ( Monad m )
>   => (s -> m (s, u))
>   -> EditsT s m u
> editWithOutput f = EditsT . lift . StateT $ (fmap swap . f)



> asksEditsT
>   :: ( Monad m )
>   => (EditsEnv m -> u)
>   -> EditsT s m u
> asksEditsT = EditsT . asks

> askEventId
>   :: ( Monad m )
>   => EditsT s m EventId
> askEventId = asksEditsT eventId

> writeLog
>   :: ( Monad m )
>   => String -> Severity -> String -> EditsT s m ()
> writeLog feature level msg = do
>   writer <- asksEditsT logger
>   result <- lift $ logMessageWith writer level ("textbox:" ++ feature ++ " " ++ msg)
>   case result of
>     Nothing -> return ()
>     Just err -> return () -- error "TextBox: writeLog panic!"
