> {-# LANGUAGE FlexibleInstances #-}

> module Kreb.Effect.Data (
>     LiftIO(..)
> 
>   , FileReader(..)
>   , fileReaderIO
> 
>   , FileWriter(..)
>   , fileWriterIO
> 
>   , LogWriter(..)
>   , logWriterIO
>   , Severity(..)
> ) where
> 
> import System.IO
> import System.IO.Error
> import Data.Time.Clock.POSIX (getPOSIXTime)
> import Data.List (lines)
> import Control.Exception



> class (Monad m) => LiftIO m where
>   liftIO :: IO a -> m a
> 
> instance LiftIO IO where
>   liftIO = id



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
