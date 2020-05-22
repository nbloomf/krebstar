> {-# LANGUAGE GADTs, DataKinds, ScopedTypeVariables, DeriveGeneric, OverloadedStrings #-}

> module Main where

> import           GHC.Generics
> import           Control.Monad ((>=>))
> import           Control.Monad.Loops (concatM)
> import           Control.Exception
> import           System.IO
> import           System.IO.Error
> import           System.Exit
> import           System.Environment
> import           System.Path.Glob (glob)
> import           System.Console.GetOpt
> import qualified System.Posix.Files.ByteString as U
> import           System.Directory
> import           System.Path
> import           Data.List
> import           Data.Foldable
> import qualified Data.Text as T
> import qualified Data.Text.Encoding as T
> import qualified Data.Text.IO as T
> import qualified Data.ByteString.Lazy as B
> import qualified Data.Map.Strict as M
> import qualified Data.HashMap.Strict as HM
> import qualified Data.Aeson as A
> import qualified Data.Aeson.Types as A
> import qualified Text.Pandoc as P

> import Kreb.Pandoc



Magic Strings
-------------

> _VERSION :: String
> _VERSION = "krebdoc-0.0"
> 
> _DEFAULT_CONFIG_FILE :: String
> _DEFAULT_CONFIG_FILE = "krebdoc.config.json"

> _MATHJAX :: String
> _MATHJAX = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/?config=TeX-AMS_CHTML-full"



> main :: IO ()
> main = do
>   -- parse command line args
>   flags <- getFlags
> 
>   let debug = _debug flags
>   if debug
>     then showDebug
>     else return ()
> 
>   if debug
>     then showFlags flags
>     else return ()
> 
>   -- show version
>   if _version flags
>     then showVersion >> exitSuccess
>     else return ()
> 
>   -- show usage
>   if _help flags
>     then showUsage >> exitSuccess
>     else return ()
> 
>   -- read config file
>   rawConfig <- readConfig (_dir flags)
>   if debug
>     then showConfig False rawConfig
>     else return ()
> 
>   -- process config file
>   config <- expandGlobs rawConfig
>   if debug
>     then showConfig True config
>     else return ()
> 
>   -- get slug->url mapping
>   slugs <- getAllSlugs config
>   if debug
>     then showSlugMap slugs
>     else return ()
> 
>   copyDir "aux/styles" "docs/styles"
>     `catches`
>       [ Handler $ \(e :: SomeException) -> do
>           putStrLn ">>> Error while trying to copy aux/styles"
>           putStrLn $ show e
>           exitFailure
>       ]
> 
>   let
>     root = _rootDir config
>     sources = _sources config
> 
>     pipeline :: FilePath -> T.Text -> P.PandocIO T.Text
>     pipeline path =
>       readMarkdownWithOpts
>         >=> addMeta "mathjaxurl" (P.MetaString _MATHJAX)
>         >=> addMeta "css" (P.MetaString $ relativize "docs/styles/style.css" ("docs/html/" ++ path))
>         >=> expandSlugs (changeExtensionTo HTML path) slugs
>         >=> convertTikZImages (changeExtensionTo HTML path)
>         >=> convertSidenotes
>         >=> writeHtmlWithOpts root
> 
>   mapM_ (\path -> processFile (pipeline path) root path) sources

> processFile
>   :: (T.Text -> P.PandocIO T.Text)
>   -> FilePath -> FilePath -> IO ()
> processFile pipeline dir infile = do
>   let
>     inpath  = dir ++ "/" ++ infile
>     outfile = changeExtensionTo HTML infile
>     outpath = dir ++ "/docs/html/" ++ outfile
>   createDirectoryIfMissing True (removeLastPathComponent outpath)
>   input <- T.readFile inpath
>     `catches`
>       [ Handler $ \(e :: SomeException) -> do
>           putStrLn $ ">>> Error while attempting to read " ++ inpath
>           putStrLn $ show e
>           exitFailure
>       ]
>   result <- run pipeline input
>   putStrLn $ ">>> Writing " ++ outfile
>   T.writeFile outpath result
>     `catches`
>       [ Handler $ \(e :: SomeException) -> do
>           putStrLn $ ">>> Error while attempting to write " ++ outpath
>           putStrLn $ show e
>           exitFailure
>       ]





Command line arguments
----------------------

> data Flags = Flags
>   { _help    :: Bool
>   , _dir     :: FilePath
>   , _version :: Bool
>   , _debug   :: Bool
>   } deriving (Eq, Show)

> getDefaultFlags :: IO Flags
> getDefaultFlags = do
>   pwd <- getEnv "PWD"
>   return $ Flags
>     { _help = False
>     , _dir = pwd
>     , _version = False
>     , _debug = False
>     }

> options :: [OptDescr (Flags -> IO Flags)]
> options =
>   [ Option ['?'] ["help"]
>       (NoArg $ \flags -> do
>         return $ flags { _help = True })
>       "show usage"
> 
>   , Option ['v'] ["version"]
>       (NoArg $ \flags -> do
>         return $ flags { _version = True })
>       "show version"
> 
>   , Option ['d'] ["dir"]
>       (ReqArg (\arg flags -> do
>         return $ flags { _dir = arg }) "PATH")
>       "full path to root directory (default: $PWD)"
> 
>   , Option [] ["debug"]
>       (NoArg $ \flags -> do
>         return $ flags { _debug = True })
>       "show diagnostic logs"
>   ]

> -- process command line args
> getFlags :: IO Flags
> getFlags = do
>   args <- getArgs
>   let
>     (actions, nonargs, errors) =
>       getOpt Permute options args
>   defaultFlags <- getDefaultFlags
> 
>   -- print errors to stderr
>   areErrors <- if null errors
>     then return False
>     else do
>       showErrors [ ">>>>> Error(s) invoking 'krebdoc':" ]
>       showErrors errors
>       return True
> 
>   -- print non args to stderr
>   areNonArgs <- if null nonargs
>     then return False
>     else do
>       showErrors [ ">>>>> Nonargument(s) passed to 'krebdoc':" ]
>       showErrors (map (++ "\n") nonargs)
>       return True
> 
>   -- bail now if any problems
>   if areErrors || areNonArgs
>     then do
>       showUsage
>       exitFailure
>     else return ()
> 
>   concatM actions defaultFlags

> showUsage :: IO ()
> showUsage = putStrLn $
>   usageInfo "Usage: krebdoc [OPTION...]" options
> 
> showVersion :: IO ()
> showVersion = putStrLn _VERSION
> 
> showErrors :: [String] -> IO ()
> showErrors errors = do
>   mapM (hPutStr stderr) errors
>   hPutStrLn stderr ""

> showFlags :: Flags -> IO ()
> showFlags f = do
>   putStrLn ">>> Debug: Flags"
>   putStrLn $ ">>>   _help = " ++ show (_help f)
>   putStrLn $ ">>>   _version = " ++ show (_version f)
>   putStrLn $ ">>>   _debug = " ++ show (_debug f)
>   putStrLn $ ">>>   _dir = " ++ show (_dir f)
>   putStrLn ""

> showDebug :: IO ()
> showDebug = do
>   putStrLn ">>> Running krebdoc in debug mode."
>   putStrLn ""





Configuration
-------------

> readConfig :: FilePath -> IO Config
> readConfig dir = do
>   let
>     configFile =
>       dir ++ "/" ++ _DEFAULT_CONFIG_FILE
>   raw <- T.readFile configFile
>     `catches`
>       [ Handler $ \(e :: IOException) -> do
>           putStrLn ">>>>> Exception:"
>           if isDoesNotExistError e
>             then do
>               putStrLn $ ">>> A config file could not be found at '" ++ configFile ++ "'."
>             else do
>               putStrLn $ ">>>   " ++ show e
>           exitFailure
>       ]
>   let decodeResult = A.eitherDecode (B.fromStrict $ T.encodeUtf8 raw)
>   val <- case decodeResult of
>     Left err -> do
>       putStrLn $ ">>> Could not decode config file at '" ++ configFile ++ "'."
>       putStrLn $ ">>> " ++ err
>       exitFailure
>     Right x -> return x
>   config <- case A.parse A.parseJSON val of
>     A.Error err -> do
>       putStrLn $ ">>> Could not parse config format in '" ++ configFile ++ "'."
>       putStrLn $ ">>>   " ++ err
>       exitFailure
>     A.Success x -> return x
>   if null (_sources config)
>     then do
>       putStrLn $ ">>> No sources specified. Nothing to do!"
>       exitSuccess
>     else return config { _rootDir = dir }

> expandGlobs :: Config -> IO Config
> expandGlobs c = do
>   let src = _sources c
>   expSrc <- concat <$> mapM glob src
>     `catches`
>       [ Handler $ \(e :: SomeException) -> do
>           putStrLn $ ">>> Glob exception:"
>           putStrLn $ ">>>   " ++ show e
>           exitFailure
>       ]
>   let
>     -- let's be opinionated
>     hasExtension path = or
>       [ ".lhs" `isSuffixOf` path
>       , ".md"  `isSuffixOf` path
>       ]
>     files = filter hasExtension expSrc
>   if null files
>     then do
>       putStrLn ">>> There were no source files specified after expanding wildcards."
>       putStrLn ">>> Nothing to do! Did you do this on purpose?"
>       exitSuccess
>     else return ()
>   return c
>     { _sources = files
>     }



> data Config = Config
>   { _rootDir   :: FilePath
>   , _configVer :: String
>   , _sources   :: [SourcePath]
>   } deriving (Eq, Show, Generic)
> 
> type SourcePath = String
> 
> defaultConfig :: Config
> defaultConfig = Config
>   { _rootDir   = ""
>   , _configVer = "0.1"
>   , _sources   = []
>   }

> instance A.FromJSON Config where
>   parseJSON x = case x of
>     A.Object v ->
>       let
>         ver = case HM.lookup "configVersion" v of
>           Nothing -> fail "missing key '$.configVersion'"
>           Just (A.String x) -> pure $ T.unpack x
>           Just invalid -> fail "expected string at $.configVersion"
>         src = case HM.lookup "sources" v of
>           Nothing -> fail "missing key '$.sources'"
>           Just (A.Array w) ->
>             let
>               f :: A.Value -> A.Parser String
>               f u = case u of
>                 A.String x -> pure $ T.unpack x
>                 invalid -> A.typeMismatch "String" invalid
>             in sequenceA $ map f $ toList w
>           Just invalid -> fail "expected array at $.sources"
>       in Config <$> pure "" <*> ver <*> src
>     invalid -> A.typeMismatch "Object" invalid



> showConfig :: Bool -> Config -> IO ()
> showConfig isCooked c = do
>   let
>     _src :: [SourcePath] -> IO ()
>     _src x = case x of
>       [] ->
>         putStrLn ">>>     []"
>       [a] -> do
>         putStrLn $ ">>>     [ " ++ show a
>         putStrLn ">>>     ]"
>       a:as -> do
>         putStrLn $ ">>>     [ " ++ show a
>         mapM_ (\z -> putStrLn $ ">>>     , " ++ show z) as
>         putStrLn ">>>     ]"
> 
>   putStrLn $ ">>> Debug: Config " ++ if isCooked then "(Cooked)" else "(Raw)"
>   putStrLn $ ">>>   _configVer = " ++ show (_configVer c)
>   putStrLn $ ">>>   _sources = "
>   _src (_sources c)
>   putStrLn ""





Anchor Slugs
------------

> getAllSlugs
>   :: Config -> IO SlugMap
> getAllSlugs c = do
>   all <- getAllForPreprocessing c
>   SlugMap m <- run getSlugMap all
>   return $ SlugMap $ M.map
>     (\(path, fragment) -> (changeExtensionTo HTML path, fragment)) m


> getAllForPreprocessing
>   :: Config -> IO [(FilePath, P.Pandoc)]
> getAllForPreprocessing c = do
>   let
>     getSource :: FilePath -> IO (FilePath, P.Pandoc)
>     getSource path = do
>       let root = _rootDir c
>       raw <- T.readFile (root ++ "/" ++ path)
>         `catches`
>           [ Handler $ \(e :: SomeException) -> do
>               putStrLn $ "Cannot read file '" ++ path ++ "'"
>               putStrLn $ show e
>               exitFailure
>           ]
>       doc <- (run readMarkdownWithOpts) raw
>       return (path, doc)
>   mapM getSource (_sources c)






asdf






processing steps for each file are something like this:

1. Get the relative directory portion of the file path; call it $dir
2. make sure the directory docs/$dir exists








> run
>   :: ( a -> P.PandocIO b )
>   -> a -> IO b
> run p x = do
>   result <- P.runIO (p x)
>   case result of
>     Right b -> return b
>     Left err -> do
>       putStrLn $ show err
>       exitFailure

> debugMeta
>   :: P.Pandoc -> P.PandocIO P.Pandoc
> debugMeta doc = do
>   let P.Pandoc meta _ = doc
>   liftIO $ putStrLn $ show meta
>   return doc

> addMeta
>   :: ( P.PandocMonad m )
>   => String -> P.MetaValue
>   -> P.Pandoc -> m P.Pandoc
> addMeta k v doc = do
>   let P.Pandoc (P.Meta m) bs = doc
>   return $ P.Pandoc (P.Meta $ M.insert k v m) bs

> readMarkdownWithOpts
>   :: ( P.PandocMonad m )
>   => T.Text -> m P.Pandoc
> readMarkdownWithOpts txt =
>   let
>     opts = P.def
>       { P.readerExtensions = P.extensionsFromList
>           [ P.Ext_fenced_code_attributes	
>           , P.Ext_fenced_code_blocks
>           , P.Ext_fenced_divs
>           , P.Ext_definition_lists
>           , P.Ext_literate_haskell
>           , P.Ext_inline_notes
>           , P.Ext_tex_math_dollars
>           , P.Ext_yaml_metadata_block
>           ]
>       }
>   in P.readMarkdown opts txt

> writeHtmlWithOpts
>   :: FilePath -> P.Pandoc -> P.PandocIO T.Text
> writeHtmlWithOpts root doc = do
>   template <- readTemplate root
>   let
>     opts = P.def
>       { P.writerHTMLMathMethod = P.MathJax _MATHJAX
>       , P.writerSectionDivs = True
>       , P.writerTemplate = Just (T.unpack template)
>       }
>   P.writeHtml5String opts doc
> 
> readTemplate
>   :: FilePath -> P.PandocIO T.Text
> readTemplate root = liftIO $
>   T.readFile (root ++ "/aux/doc.html5")
>     `catches`
>       [ Handler $ \(e :: SomeException) -> do
>           putStrLn $ ">>> Error while attempting to read template file at"
>           putStrLn $ ">>>   " ++ (root ++ "/aux/doc.html5")
>           exitFailure
>       ]
