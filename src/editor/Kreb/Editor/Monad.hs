module Kreb.Editor.Monad (
    runKrebEd
  , KrebEdReplParams
  , initAppState
) where

import System.IO.Error

import Kreb.Control
import Kreb.Text
import Kreb.Lang
import Kreb.Editor.State
import Kreb.Editor.Signal
import Kreb.Editor.Action
import Kreb.Editor.Env
import Kreb.Editor.Tab



type KrebEd m a
  = ReplT [Action] (AppEnv m) AppSignal (AppState m) m a

runKrebEd
  :: ( Monad m )
  => KrebEdReplParams m -> AppEnv m -> EventId -> FilePath -> (Int, Int) -> m ()
runKrebEd params env eId path dim =
  runReplT params env (initAppState env eId path dim) loopReplT

type KrebEdReplParams m
  = ReplParams [Action] (AppEnv m) AppSignal (AppState m) m

initAppState :: (Monad m) => AppEnv m -> EventId -> FilePath -> (Int, Int) -> AppState m
initAppState env eId stdLib (w,h) =
  let rts = runtimeState env eId in
  AppState
    { windowDim            = (w,h)
    , editorMode           = NormalMode
    , absCursorPos         = (0,0)
    , tabWidth             = 4
    , tabbedBuffers        = initTabs "" (w,h) (initPanelDim (w,h)) 4
    , glyphRenderSettings  = defaultGlyphRenderSettings

    , runtimeSt            = rts { _rtStack = Cons Empty V_Eff }
    , stdLibPath           = stdLib
    }
