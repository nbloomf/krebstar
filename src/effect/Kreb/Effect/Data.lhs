> {-# LANGUAGE FlexibleInstances #-}

> module Kreb.Effect.Data (
>     FileReader(..)
>   , fileReaderIO
> 
>   , FileWriter(..)
>   , fileWriterIO
> 
>   , LogWriter(..)
>   , logWriterIO
>   , Severity(..)
> 
>   , ClipboardWriter(..)
>   , clipboardWriterIO
> 
>   , ClipboardReader(..)
>   , clipboardReaderIO
> ) where
> 
> import System.IO
> import System.IO.Error
> import Data.Time.Clock.POSIX (getPOSIXTime)
> import Data.List (lines)
> import Control.Exception

> import System.Hclip (getClipboard, setClipboard)
> 
> import Kreb.Control.LiftIO




> newtype FileReader m = FileReader
>   { readFileWith :: FilePath -> m (Either IOError String) }
> 
> fileReaderIO
>   :: ( LiftIO m )
>   => FileReader m
> fileReaderIO = FileReader $
>   \path -> liftIO $ catch (read path) handler
>   where
>     read :: FilePath -> IO (Either IOError String)
>     read path = fmap Right $ readFile path
> 
>     handler :: IOError -> IO (Either IOError String)
>     handler err = return $ Left err
> 
> instance Show (FileReader IO) where
>   show _ = "<FileReader IO>"



> newtype FileWriter m = FileWriter
>   { writeFileWith :: FilePath -> String -> m (Maybe IOError) }
> 
> fileWriterIO
>   :: ( LiftIO m )
>   => FileWriter m
> fileWriterIO = FileWriter $
>   \path content -> liftIO $ catch (write path content) handler
>   where
>     write :: FilePath -> String -> IO (Maybe IOError)
>     write path content = writeFile path content >> return Nothing
> 
>     handler :: IOError -> IO (Maybe IOError)
>     handler err = return $ Just err
> 
> instance Show (FileWriter IO) where
>   show _ = "<FileWriter IO>"



> newtype LogWriter m = LogWriter
>   { logMessageWith :: Severity -> String -> m (Maybe IOError) }
> 
> data Severity
>   = Debug_
>   | Info_
>   | Notice_
>   | Warning_
>   | Error_
>   | Critical_
>   | Alert_
>   | Emergency_
>   deriving (Eq, Ord)
> 
> instance Show Severity where
>   show x = case x of
>     Debug_ -> "DEBUG"
>     Info_ -> "INFO"
>     Notice_ -> "NOTICE"
>     Warning_ -> "WARNING"
>     Error_ -> "ERROR"
>     Critical_ -> "CRITICAL"
>     Alert_ -> "ALERT"
>     Emergency_ -> "EMERGENCY"
> 
> logWriterIO
>   :: ( LiftIO m )
>   => Handle -> LogWriter m
> logWriterIO handle = LogWriter $ \sev msg -> liftIO $ do
>   t <- round <$> getPOSIXTime
>   let
>     prefix = concat
>       [ show t, "\t", show sev, "\t" ]
>   eachM log (map (prefix ++) $ lines msg)
>   where
>     log :: String -> IO (Maybe IOError)
>     log msg =
>       catch (hPutStrLn handle msg >> hFlush handle >> return Nothing) handler
> 
>     handler :: IOError -> IO (Maybe IOError)
>     handler err = return $ Just err
> 
>     eachM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
>     eachM f = each
>       where
>         each w = case w of
>           [] -> return Nothing
>           a:as -> do
>             r <- f a
>             case r of
>               Nothing -> each as
>               Just b -> return (Just b)



> newtype ClipboardReader m = ClipboardReader
>   { readClipboardWith :: m (Either IOError String) }
> 
> clipboardReaderIO
>   :: ( LiftIO m )
>   => ClipboardReader m
> clipboardReaderIO = ClipboardReader $
>   liftIO $ catch read handler
>   where
>     read :: IO (Either IOError String)
>     read = fmap Right getClipboard
> 
>     handler :: IOError -> IO (Either IOError String)
>     handler err = return $ Left err
> 
> instance Show (ClipboardReader IO) where
>   show _ = "<ClipboardReader IO>"



> newtype ClipboardWriter m = ClipboardWriter
>   { writeClipboardWith :: String -> m (Maybe IOError) }
> 
> clipboardWriterIO
>   :: ( LiftIO m )
>   => ClipboardWriter m
> clipboardWriterIO = ClipboardWriter $
>   \content -> liftIO $ catch (write content) handler
>   where
>     write :: String -> IO (Maybe IOError)
>     write content = setClipboard content >> return Nothing
> 
>     handler :: IOError -> IO (Maybe IOError)
>     handler err = return $ Just err
> 
> instance Show (ClipboardWriter IO) where
>   show _ = "<ClipboardWriter IO>"
