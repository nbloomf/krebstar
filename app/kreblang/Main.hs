{-# LANGUAGE
    ScopedTypeVariables
#-}

module Main where


import Prelude hiding (read)

import Kreb.Lang

import Control.Monad (when)
import System.Console.GetOpt
import System.Exit
import System.IO
import System.Environment

main :: IO ()
main = do
  opts <- getOptions

  -- show usage and quit
  () <- when (True == optHelp opts) $
          showUsage >> exitSuccess

  -- show version info and quit
  () <- when (True == optVersion opts) $
          showVersion >> exitSuccess

  case optPath opts of
    Just path -> do
      env <- readFile path >>= load
      case optQuery opts of
        Nothing -> loop (Just path) env
        Just qs -> singleQuery qs env
    Nothing -> do
      env <- if optStream opts
        then getContents >>= load
        else return $
          initRuntimeState (const Nothing) (const Nothing)
      case optQuery opts of
        Nothing -> loop Nothing env
        Just qs -> singleQuery qs env



singleQuery :: String -> RuntimeState IO -> IO ()
singleQuery qs env =
  case runParser pPhrase qs of
    Left err -> hPutStrLn stderr $ show err
    Right expr -> do
      x <- runRuntime (interpret (Query expr)) env
      case x of
        Left err -> do
          hPutStrLn stderr $ show err
        Right (_, env1) -> do
          putStrLn $ dropWhile (== ' ') $ pretty (_rtStack env1)


load :: String -> IO (RuntimeState IO)
load str = do
  ast <- case runParser pModule str of
    Left err -> do
      hPutStrLn stderr $ show err
      exitFailure
    Right x -> return x

  let
    Module ds = ast

  (env :: RuntimeState IO) <- do
    r <- runRuntime (applyDecls ds) (initRuntimeState (const Nothing) (const Nothing))
    case r of
      Left err -> do
        hPutStrLn stderr $ show err
        exitFailure
      Right (_,e) -> return e

  return env

data Meta
  = QuitCmd
  | ReloadCmd
  | TypeQueryCmd Phrase
  deriving (Eq, Show)

read :: IO (Either Meta Command)
read = do
  putStr "> " >> hFlush stdout
  str <- getLine
  case str of
    ":q" -> return (Left QuitCmd)
    ":r" -> return (Left ReloadCmd)
    ':':'t':rest -> do
      case runParser pPhrase rest of
        Left err -> do
          hPutStrLn stderr $ show err
          read
        Right expr -> return $ Left $ TypeQueryCmd expr
    _ -> do
      case runParser pPhrase str of
        Left err -> do
          hPutStrLn stderr $ show err
          read
        Right e -> return $ Right $ Query e

loop :: Maybe FilePath -> RuntimeState IO -> IO ()
loop path env = do
  cmd <- read
  case cmd of
    Left QuitCmd -> exitSuccess
    Left ReloadCmd ->
      case path of
        Nothing -> loop path env
        Just p -> do
          env1 <- readFile p >>= load
          loop path env1
    Left (TypeQueryCmd expr) -> do
      x <- runRuntime (inferType expr) env
      case x of
        Left err -> do
          hPutStrLn stderr $ show err
        Right (arr, _) -> do
          putStrLn $ pretty arr
      loop path env
    Right query -> do
      x <- runRuntime (interpret query) env
      case x of
        Left err -> do
          hPutStrLn stderr $ show err
          loop path env
        Right (_, env1) -> do
          putStrLn $ pretty (_rtStack env1)
          loop path env1



data Options = Options
  { optHelp :: Bool
  , optVersion :: Bool
  , optPath :: Maybe FilePath
  , optQuery :: Maybe String
  , optStream :: Bool
  } deriving (Eq, Show)

defaults :: Options
defaults = Options
  { optHelp = False
  , optVersion = False
  , optPath = Nothing
  , optQuery = Nothing
  , optStream = False
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
      munge opts = opts { optStream = True }
    in
      Option ['s'] ["stream"] (NoArg munge)
        "read module from stdin"

  , let
      munge p opts = opts { optPath = Just p }
    in
      Option ['p'] ["path"] (ReqArg munge "FILE")
        "path to definitions file"

  , let
      munge p opts = opts { optQuery = Just p }
    in
      Option ['q'] ["query"] (ReqArg munge "STRING")
        "run query in batch mode"


  ]

showUsage :: IO ()
showUsage = do
  let header = "USAGE: lang [OPTION...]"
  putStrLn $ usageInfo header options

showVersion :: IO ()
showVersion = putStrLn versionString

versionString :: String
versionString = "lang 0.0"

-- Parse the options, reporting any errors.
getOptions :: IO Options
getOptions = do
  argv <- getArgs

  case getOpt Permute options argv of
    -- no errors, no unrecognized options
    (actions, [], []) -> return $ compose actions defaults

    -- unrecognized options
    (_, nonopts, []) -> do
      errPutStrLn "hostgroup: unrecognized option(s)"
      mapM_ errPutStrLn nonopts
      showUsage
      exitFailure

    -- option errors
    (_, _, errs) -> do
      errPutStrLn "hostgroup: option error(s)"
      mapM_ errPutStrLn errs
      showUsage
      exitFailure

-- Like putStrLn, but for stderr.
errPutStrLn :: String -> IO ()
errPutStrLn msg = hPutStr stderr msg >> hPutChar stderr '\n'

compose :: (Traversable t) => t (a -> a) -> a -> a
compose = foldr (.) id


