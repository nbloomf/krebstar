module Kreb.Editor.CLI.Run (
    consoleIO
) where

import Graphics.Vty
import qualified System.Console.Terminal.Size as TS

import System.IO.Error
import Control.Exception

import Kreb.Control
import Kreb.Text
import Kreb.Editor

import Kreb.Editor.CLI.Render
import Kreb.Editor.CLI.Handler



consoleIO :: FilePath -> IO ()
consoleIO stdLibPath = do
  (replParams, env, dim) <- appEnvIO
  let panelDim = initPanelDim dim
  runKrebEd replParams env (initAppState stdLibPath panelDim (runtimeState env) dim) loopReplT

initPanelDim
  :: (Int, Int) -> PanelDim
initPanelDim (width, height) =
  let
    w1 = max 4 $ width `div` 2
    w2 = width - w1 - 1
    h = height
  in PanelDim
    { _textLabelDim = (2, h-2)
    , _textDim = (w1-3, h-2)
    , _historyDim = (w2, h-4)
    , _commandDim = (w2, 1)
    , _statusDim = (width, 1)
    }


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

  return
    -- Loop callbacks
    ( ReplParams
      { _Init = \env st -> do
          result <- loadStdLib (stdLibPath st) env st
          return $ case result of
            Left sig -> Left sig
            Right rts -> Right $ st { runtimeSt = rts }
      , _Read = \_ st -> do
          let mode = editorMode st
          ev <- nextEvent vty
          return $ eventMapping mode ev
      , _Eval = performActions
      , _Print = \_ st -> render $ updateStateCache st
      , _Exit = \sig -> do
          shutdown vty
          case sig of
            ExitNormally -> return ()
            _ -> putStrLn $ show sig
      }
    -- Effect callbacks
    , AppEnv
      { logMessage = appendFile "/Users/nathan/code/ned/logs.txt"
      , loadFile = loadFileIO
      , saveFile = saveFileIO
      }
    , (w0, h0)
    )



loadFileIO :: FilePath -> IO (Either IOError String)
loadFileIO path = catch read handle
  where
    read :: IO (Either IOError String)
    read = fmap Right $ readFile path

    handle :: IOError -> IO (Either IOError String)
    handle err = return $ Left err

saveFileIO :: FilePath -> String -> IO (Maybe IOError)
saveFileIO path str = catch write handle
  where
    write :: IO (Maybe IOError)
    write = writeFile path str >> return Nothing

    handle :: IOError -> IO (Maybe IOError)
    handle err = return $ Just err


