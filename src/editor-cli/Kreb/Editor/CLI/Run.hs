module Kreb.Editor.CLI.Run (
    consoleIO
) where

import Graphics.Vty
import qualified System.Console.Terminal.Size as TS

import System.IO

import Kreb.Effect
import Kreb.Control
import Kreb.Text
import Kreb.Editor

import Kreb.Editor.CLI.Render
import Kreb.Editor.CLI.Handler



-- this path should go in preferences
consoleIO :: FilePath -> IO ()
consoleIO stdLibPath = do
  (replParams, env, dim) <- appEnvIO
  let eId = EventId 0 "init"
  runKrebEd replParams env eId stdLibPath dim




getTerminalSize :: IO (Int, Int)
getTerminalSize = do
  result <- TS.size
  case result of
    Nothing -> error "getTerminalSize: could not get size"
    Just w -> return (TS.width w, TS.height w)

appEnvIO :: IO (KrebEdReplParams IO, AppEnv IO, (Int, Int))
appEnvIO = do
  config <- standardIOConfig
  vty <- mkVty $ config
    { mouseMode = Just True
    , debugLog = Nothing -- Just "/Users/nathan/code/ned/vty-debug.txt"
    }
  setMode (outputIface vty) Mouse True
  setMode (outputIface vty) Focus True
  setMode (outputIface vty) BracketedPaste True
  (w0, h0) <- getTerminalSize

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
          ev <- nextEvent vty
          return $ eventMapping mode ev
      , _Eval = \env st act -> do
          let eId = EventId 0 "foo"
          performActions env st eId act
      , _Print = \_ st ->
          render $ updateStateCache st
      , _Exit = \sig -> do
          hClose logHandle
          shutdown vty
          case sig of
            ExitNormally -> return ()
            _ -> putStrLn $ show sig
      }
    -- Effect callbacks
    , AppEnv
      { logWriter = logWriterIO logHandle
      , fileReader = fileReaderIO
      , fileWriter = fileWriterIO
      }
    , (w0, h0)
    )


