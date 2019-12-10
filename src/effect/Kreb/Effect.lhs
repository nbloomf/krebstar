> {-# LANGUAGE FlexibleInstances #-}

> module Kreb.Effect (
>     FileReader(..)
>   , fileReaderIO
> 
>   , FileWriter(..)
>   , fileWriterIO
> ) where
> 
> import System.IO.Error
> import Control.Exception



> newtype FileReader m = FileReader
>   { readFileWith :: FilePath -> m (Either IOError String) }
> 
> fileReaderIO :: FileReader IO
> fileReaderIO = FileReader $
>   \path -> catch (read path) handle
>   where
>     read :: FilePath -> IO (Either IOError String)
>     read path = fmap Right $ readFile path
> 
>     handle :: IOError -> IO (Either IOError String)
>     handle err = return $ Left err
> 
> instance Show (FileReader IO) where
>   show _ = "<FileReader IO>"



> newtype FileWriter m = FileWriter
>   { writeFileWith :: FilePath -> String -> m (Maybe IOError) }
> 
> fileWriterIO :: FileWriter IO
> fileWriterIO = FileWriter $
>   \path content -> catch (write path content) handle
>   where
>     write :: FilePath -> String -> IO (Maybe IOError)
>     write path content = writeFile path content >> return Nothing
> 
>     handle :: IOError -> IO (Maybe IOError)
>     handle err = return $ Just err
> 
> instance Show (FileWriter IO) where
>   show _ = "<FileWriter IO>"
