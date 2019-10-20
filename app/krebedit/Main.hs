module Main where

import Control.Monad (when)
import System.Console.GetOpt
import System.Environment
import System.IO
import System.Exit

import Kreb.Editor.CLI

main :: IO ()
main = do
  opts <- getOptions

  -- show usage and quit
  () <- when (True == optHelp opts) $
          showUsage >> exitSuccess

  -- show version info and quit
  () <- when (True == optVersion opts) $
          showVersion >> exitSuccess

  path <- return $ case optStdLibPath opts of
    Nothing -> "/Users/nathan/code/krebstar/stdlib.txt"
    Just p -> p

  consoleIO path


data Options = Options
  { optHelp       :: Bool
  , optVersion    :: Bool
  , optStdLibPath :: Maybe FilePath
  } deriving (Eq, Show)

defaults :: Options
defaults = Options
  { optHelp       = False
  , optVersion    = False
  , optStdLibPath = Nothing
  }


options :: [OptDescr (Options -> Options)]
options =
  [ let
      munge opts = opts { optHelp = True }
    in
      Option ['?'] ["help"] (NoArg munge)
        "show usage"

  , let
      munge opts = opts { optVersion = True }
    in
      Option [] ["version"] (NoArg munge)
        "show version information"

  , let
      munge p opts = opts { optStdLibPath = Just p }
    in
      Option ['p'] ["path"] (ReqArg munge "FILE")
        "path to definitions file"
  ]

-- Parse the options, reporting any errors.
getOptions :: IO Options
getOptions = do
  argv <- getArgs

  case getOpt Permute options argv of
    -- no errors, no unrecognized options
    (actions, [], []) -> return $ compose actions defaults

    -- unrecognized options
    (_, nonopts, []) -> do
      errPutStrLn "krebed: unrecognized option(s)"
      mapM_ errPutStrLn nonopts
      showUsage
      exitFailure

    -- option errors
    (_, _, errs) -> do
      errPutStrLn "krebed: option error(s)"
      mapM_ errPutStrLn errs
      showUsage
      exitFailure

showUsage :: IO ()
showUsage = do
  let header = "USAGE: krebed [OPTION...]"
  putStrLn $ usageInfo header options

showVersion :: IO ()
showVersion = putStrLn versionString

versionString :: String
versionString = "lang 0.0"

-- Like putStrLn, but for stderr.
errPutStrLn :: String -> IO ()
errPutStrLn msg = hPutStr stderr msg >> hPutChar stderr '\n'

compose :: (Traversable t) => t (a -> a) -> a -> a
compose = foldr (.) id

