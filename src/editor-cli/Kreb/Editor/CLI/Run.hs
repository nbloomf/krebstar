module Kreb.Editor.CLI.Run (
    consoleIO
) where

import Graphics.Vty
import qualified System.Console.Terminal.Size as TS

import System.IO.Error
import Control.Exception

import Kreb.Text
import Kreb.Editor

import Kreb.Editor.CLI.Render
import Kreb.Editor.CLI.Handler



consoleIO :: IO ()
consoleIO = do
  (env, dim) <- appEnvIO
  runApp env (updateStateCache $ initAppState runtimeStateIO dim) primaryEventLoop
  cleanup env

getTerminalSize :: IO (Int, Int)
getTerminalSize = do
  result <- TS.size
  case result of
    Nothing -> error "getTerminalSize: could not get size"
    Just w -> return (TS.width w, TS.height w)

appEnvIO :: IO (AppEnv IO, (Int, Int))
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
    render st = do
      let
        (x,y) = getAbsCursorPos st
        pic = Picture
          (Cursor x y)
          [imageAppState st]
          (ClearBackground)
      update vty pic

  return 
    ( AppEnv
      { renderState = render
      , getNextEvent = \mode -> do
          e <- nextEvent vty
          return $ eventMapping mode e
      , cleanup = shutdown vty
      , logMessage = appendFile "/Users/nathan/code/ned/logs.txt"
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


