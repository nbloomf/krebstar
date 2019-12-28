module Kreb.Editor.TUI.Run (
    consoleIO
) where

import Graphics.Vty
import qualified System.Console.Terminal.Size as TS

import System.IO

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
  result <- runTermUI TermEnv TermState $
    runKrebEd replParams env eId stdLibPath dim
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

  let
    render st =
      update vty (imageAppState st)

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
          let mode = editorMode st
          ev <- lift $ nextEvent vty
          return $ eventMapping mode ev
      , _Eval = \env st act -> do
          let eId = EventId (1 + getActionCounter st) "foo"
          performActions env (tickActionCounter st) eId act
      , _Print = \_ st ->
          lift $ render $ updateAbsCursorPos st
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
      }
    , (w0, h0)
    )


