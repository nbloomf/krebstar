module Kreb.Editor.Monad (
    App()
  , runApp
  , AppEnv(..)

  , primaryEventLoop

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

import System.IO.Error

import Kreb.Editor.State
import Kreb.Editor.Error
import Kreb.Editor.Action

type App m a = ConsoleT (AppEnv m) (AppState m) m a

runApp :: (Monad m) => AppEnv m -> AppState m -> App m a -> m a
runApp = runConsoleT


-- Side effects are managed here.
data AppEnv m = AppEnv
  { renderState :: AppState m -> m ()
  , getNextEvent :: EditorMode -> m Action
  , cleanup :: m ()
  , logMessage :: String -> m ()

  , loadFile :: FilePath -> m (Either IOError String)
  , saveFile :: FilePath -> String -> m (Maybe IOError)
  }

-- Effects

_renderState :: ( Monad m ) => App m ()
_renderState =
  get >>= askM1 renderState

_getNextEvent :: ( Monad m ) => App m Action
_getNextEvent = do
  mode <- gets editorMode
  event <- askM (\st -> getNextEvent st mode)
  askM1 logMessage (show event ++ "\n\n\n\n")
  return event

_handleEvent :: ( Monad m ) => Action -> App m Then
_handleEvent event = do
  st <- get
  (next, st') <- lift $ performAction event st
  put $ updateStateCache st'
  return next

_logDebugMessages :: ( Monad m ) => App m ()
_logDebugMessages = do
  -- st <- get
  -- askM1 logMessage $ renderDebugMessage st
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

gets
  :: ( Monad m )
  => (s -> a)
  -> ConsoleT r s m a
gets f = ConsoleT $ \_ s ->
  return (f s, s)

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


primaryEventLoop
  :: ( Monad m )
  => App m (Maybe AppError)
primaryEventLoop = loop'
  where
    loop' = do
      _renderState
      next <- _getNextEvent >>= _handleEvent
      _logDebugMessages
      case next of
        Bail err -> return (Just err)
        Stop     -> return Nothing
        GoOn     -> loop'
