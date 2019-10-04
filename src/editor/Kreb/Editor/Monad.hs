module Kreb.Editor.Monad (
    runKrebEd
  , KrebEdReplEnv
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
  => KrebEdReplEnv m -> AppEnv m -> AppState m
  -> KrebEd m a -> m a
runKrebEd = runReplT

type KrebEdReplEnv m
  = ReplEnv Action (AppEnv m) AppSignal (AppState m) m
