module Kreb.Editor.TUI.Run (
    consoleIO
) where

import Graphics.Vty
import qualified System.Console.Terminal.Size as TS

import System.IO
import System.Exit

import Kreb.Effect
import Kreb.Control
import Kreb.Text
import Kreb.Editor.Core

import Kreb.Editor.TUI.Render
import Kreb.Editor.TUI.Handler
import Kreb.Editor.TUI.Monad



-- this path should go in preferences
consoleIO :: FilePath -> IO ()
consoleIO stdLibPath = do
  (replParams, env, dim) <- appEnvIO
  let eId = EventId 0 "init"

  -- construct the initial application state
  st <- do
    st' <- runTermUI TermEnv initTermState $
      buildInitialAppState env eId stdLibPath dim
    case st' of
      Left err -> do
        putStrLn $ "Initialization Error: " ++ show err
        exitFailure
      Right w -> return w

  let layout = snd $ renderState NormalMode st
  result <- runTermUI TermEnv (TermState layout NormalMode) $
    runEditorCore replParams env st
  case result of
    Just err -> putStrLn $ "Error! " ++ show err
    Nothing -> return ()


getTerminalSize :: TermUI IO (Int, Int)
getTerminalSize = do
   result <- lift TS.size
   case result of
     Nothing -> error "getTerminalSize: could not get size"
     Just w -> return (TS.width w, TS.height w)


getTerminalSize' :: IO (Int, Int)
getTerminalSize' = do
  result <- TS.size
  case result of
    Nothing -> error "getTerminalSize: could not get size"
    Just w -> return (TS.width w, TS.height w)

appEnvIO :: IO (KrebEdReplParams (TermUI IO), AppEnv (TermUI IO), (Int, Int))
appEnvIO = do
  config <- standardIOConfig
  vty <- mkVty $ config
    { mouseMode = Just True
    , debugLog = Nothing -- Just "/Users/nathan/code/ned/vty-debug.txt"
    }
  setMode (outputIface vty) Mouse True
  setMode (outputIface vty) Focus True
  setMode (outputIface vty) BracketedPaste True
  (w0, h0) <- getTerminalSize'

  let logPath = "/Users/nathan/code/krebstar/zzz.txt"
  writeFile logPath ""
  logHandle <- openFile logPath WriteMode

  return
    -- Loop callbacks
    ( ReplParams
      { _Init = \env st -> do
          let eId = EventId 0 "load"
          result <- loadStdLib (stdLibPath st) env st eId
          return $ case result of
            Left sig -> Left sig
            Right rts -> Right $ st { runtimeSt = rts }
      , _Read = \_ st -> do
          mode <- getEditorMode
          layout <- getsTermState screenLayout
          ev <- lift $ nextEvent vty
          let (newMode, acts) = eventMapping mode layout ev
          setEditorMode newMode
          return acts
      , _Eval = \env st act -> do
          mode <- getEditorMode
          let eId = EventId (1 + getActionCounter st) "foo"
          performActions env (tickActionCounter st) eId act
      , _Print = \_ st -> do
          mode <- getEditorMode
          let (pic, layout) = renderState mode $ updateAbsCursorPos mode st
          modifyTermState $ \x -> x { screenLayout = layout }
          lift $ update vty pic
      , _Exit = \sig -> do
          liftIO $ hClose logHandle
          liftIO $ shutdown vty
          case sig of
            ExitNormally -> return ()
            _ -> liftIO $ putStrLn $ show sig
      }

    -- Effect callbacks
    , AppEnv
      { logWriter = logWriterIO logHandle
      , fileReader = fileReaderIO
      , fileWriter = fileWriterIO
      , clipboardReader = clipboardReaderIO
      , clipboardWriter = clipboardWriterIO
      }
    , (w0, h0)
    )


