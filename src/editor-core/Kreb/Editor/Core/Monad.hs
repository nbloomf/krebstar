module Kreb.Editor.Core.Monad (
    runEditorCore
  , KrebEdReplParams
  , buildInitialAppState
) where

import System.IO.Error

import Kreb.Control
import Kreb.Text
import Kreb.Lang
import Kreb.Editor.Core.Action
import Kreb.Editor.Core.Data



type KrebEd m a
  = ReplT [Action] (AppEnv m) AppSignal (AppState m) m a

type KrebEdReplParams m
  = ReplParams [Action] (AppEnv m) AppSignal (AppState m) m

runEditorCore
  :: ( Monad m )
  => KrebEdReplParams m -> AppEnv m -> AppState m
  -> m (Maybe AppSignal)
runEditorCore params env st =
  runReplT params env st loopReplT >> return Nothing

buildInitialAppState
  :: ( Monad m )
  => AppEnv m -> EventId -> FilePath -> (Int, Int) -> m (Either AppSignal (AppState m))
buildInitialAppState env eId stdLib (w,h) = do
  let
    rts = runtimeState env eId
    st = AppState
      { windowDim            = (w,h)
      , editorMode           = NormalMode
      , absCursorPos         = (0,0)
      , tabWidth             = 4
      , tabbedBuffers        = initTabs "" (w,h) (initPanelDim (w,h)) 4
      , glyphRenderSettings  = defaultGlyphRenderSettings
      , runtimeSt            = rts { _rtStack = Cons Empty V_Eff }
      , stdLibPath           = stdLib
      , actionCounter        = 0
      }
  result <- loadStdLib stdLib env st eId
  case result of
    Left err -> return (Left err)
    Right rts' -> return (Right (st { runtimeSt = rts' }))
