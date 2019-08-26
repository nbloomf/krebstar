module Ned.App (
    App
  , runApp
  , AppEnv(..)
  , AppState(..)
  , AppEvent(..)
  , AppError(..)
  , Then(..)
  , Key(..)
  , Modifier(..)
  , Button(..)
  , handler

  , Buffer()

  , EditorMode(..)

  , primaryEventLoop
  , initAppState
) where

import Ned.Monad
import Ned.App.State
import Ned.App.Event
import Ned.App.Error
import Ned.App.Action
import Ned.App.Handler
import Ned.Data




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
