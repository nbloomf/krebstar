module Ned.App.IO (
    consoleIO
) where

import Graphics.Vty
import qualified System.Console.Terminal.Size as TS

import Ned
import Ned.App.IO.Event
import Ned.App.IO.Render
import Ned.App.State
import Ned.App.Hooks
import Ned.Data



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
    , debugLog = Just "/Users/nathan/code/ned/vty-debug.txt"
    }
  setMode (outputIface vty) Mouse True
  setMode (outputIface vty) Focus True
  setMode (outputIface vty) BracketedPaste True
  (w0, h0) <- getTerminalSize

  let
    render st = do
      let
        (x,y) = getAbsCursorPos st
        blk = Picture
          (Cursor x y)
          [charFill defAttr ' ' w0 h0]
          (ClearBackground)
        pic = Picture
          (Cursor x y)
          [imageAppState st]
          (ClearBackground)
      update vty blk
      update vty pic

  return 
    ( AppEnv
      { renderState = render
      , getNextEvent = fmap convert $ nextEvent vty
      , cleanup = shutdown vty
      , logMessage = appendFile "/Users/nathan/code/ned/logs.txt"
      }
    , (w0, h0)
    )
