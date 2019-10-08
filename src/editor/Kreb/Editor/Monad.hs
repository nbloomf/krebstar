module Kreb.Editor.Monad (
    runKrebEd
  , KrebEdReplParams
) where

import System.IO.Error

import Kreb.Control
import Kreb.Editor.State
import Kreb.Editor.Signal
import Kreb.Editor.Action
import Kreb.Editor.Env



type KrebEd m a
  = ReplT Action (AppEnv m) AppSignal (AppState m) m a

runKrebEd
  :: ( Monad m )
  => KrebEdReplParams m -> AppEnv m -> AppState m
  -> KrebEd m a -> m a
runKrebEd = runReplT

type KrebEdReplParams m
  = ReplParams Action (AppEnv m) AppSignal (AppState m) m
