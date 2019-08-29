module Ned.Monad (
    App()
  , runApp
  , AppEnv(..)

  , _renderState
  , _getNextEvent
  , _handleEvent
  , _logDebugMessages

  , ConsoleT()
  , runConsoleT
  , get
  , put
  , mutate
  , lift
) where

import Ned.App.State
import Ned.App.Event
import Ned.App.Error
import Ned.App.Action
import Ned.App.Handler

type App m a = ConsoleT (AppEnv m) (AppState m) m a

runApp :: (Monad m) => AppEnv m -> AppState m -> App m a -> m a
runApp = runConsoleT


-- Side effects are managed here.
data AppEnv m = AppEnv
  { renderState :: AppState m -> m ()
  , getNextEvent :: m AppEvent
  , cleanup :: m ()
  , logMessage :: String -> m ()
  }

-- Effects

_renderState :: ( Monad m ) => App m ()
_renderState =
  get >>= askM1 renderState

_getNextEvent :: ( Monad m ) => App m AppEvent
_getNextEvent = do
  event <- askM getNextEvent
  askM1 logMessage (show event ++ "\n\n\n\n")
  return event

_handleEvent :: ( Monad m ) => AppEvent -> App m Then
_handleEvent event = do
  st <- get
  (next, st') <- lift $ handler st event
  put $ updateStateCache st'
  return next

_logDebugMessages :: ( Monad m ) => App m ()
_logDebugMessages = do
  st <- get
  askM1 logMessage $ renderDebugMessage st
  return ()

newtype ConsoleT r s m a = ConsoleT
  { unConsoleT :: r -> s -> m (a,s)
  }

runConsoleT
  :: ( Monad m )
  => r -> s -> ConsoleT r s m a -> m a
runConsoleT env st (ConsoleT act) =
  fst <$> act env st

instance
  ( Monad m
  ) => Functor (ConsoleT r s m)
  where
    fmap f (ConsoleT x) =
      ConsoleT $ \r s1 -> do
        (a, s2) <- x r s1
        return (f a, s2)

instance
  ( Monad m
  ) => Applicative (ConsoleT r s m)
  where
    pure a = ConsoleT $ \_ s ->
      return (a, s)

    (ConsoleT f) <*> (ConsoleT x) =
      ConsoleT $ \r s1 -> do
        (g, s2) <- f r s1
        (a, s3) <- x r s2
        return $ (g a, s3)

instance
  ( Monad m
  ) => Monad (ConsoleT r s m)
  where
    return a = ConsoleT $ \_ s ->
      return (a, s)

    (ConsoleT x) >>= f =
      ConsoleT $ \r s1 -> do
        (a, s2) <- x r s1
        unConsoleT (f a) r s2

lift
  :: ( Monad m )
  => m a -> ConsoleT r s m a
lift x = ConsoleT $ \_ s -> do
  a <- x
  return (a, s)



get
  :: ( Monad m )
  => ConsoleT r s m s
get = ConsoleT $ \_ s ->
  return (s, s)

put
  :: ( Monad m )
  => s
  -> ConsoleT r s m ()
put s = ConsoleT $ \_ _ ->
  return ((), s)

mutate
  :: ( Monad m )
  => (s -> s)
  -> ConsoleT r s m ()
mutate f =
  ConsoleT $ \_ s ->
    return ((), f s)

ask
  :: ( Monad m )
  => (r -> a)
  -> ConsoleT r s m a
ask f = ConsoleT $ \r s ->
  return (f r, s)

askM
  :: ( Monad m )
  => (r -> m a)
  -> ConsoleT r s m a
askM f = ask f >>= lift

askM1
  :: ( Monad m )
  => (r -> a -> m b)
  -> a -> ConsoleT r s m b
askM1 f a = do
  g <- ask f
  lift $ g a

askM2
  :: ( Monad m )
  => (r -> a -> b -> m c)
  -> a -> b -> ConsoleT r s m c
askM2 f a b = do
  g <- ask f
  lift $ g a b
