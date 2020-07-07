> {-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, ScopedTypeVariables, RankNTypes, KindSignatures, FlexibleInstances, GADTs #-}
> 
> module Kreb.Effect.Region (
>     RGT()
>   , HandlesRGT
>   , Safe()
> ) where
> 
> import System.IO
> import System.IO.Error
> import Control.Exception
> import Data.IORef
> 
> import Kreb.Control

Region with resource type `res`

> newtype RGT s res m a
>   = RGT { unRGT :: EnvT (IORef res) m a }
> 
> deriving instance (Monad m) => Functor     (RGT s res m)
> deriving instance (Monad m) => Applicative (RGT s res m)
> deriving instance (Monad m) => Monad       (RGT s res m)



File handle resource with reference count

> type HandlesRGT s = RGT s [HandleR]
> 
> data HandleR = HandleR Handle (IORef Integer)

> closeHandleR :: HandleR -> IO ()
> closeHandleR (HandleR h refcount) = do
>   hPutStrLn stderr $ "Closing " ++ show h
>   rc <- readIORef refcount
>   writeIORef refcount (max 0 (rc - 1))
>   if rc > 1
>     then hPutStrLn stderr "  Aliased handle wasn't closed"
>     else hClose h

> newHandleR :: Handle -> IO HandleR
> newHandleR h = newIORef 1 >>= return . HandleR h





> -- internal only class
> class (Monad m) => RegionIO m where
>   brace   :: m a -> (a -> m b) -> (a -> m c) -> m c
>   capture :: (Exception e) => m a -> (e -> m a) -> m a
>   lyftIO  :: IO a -> m a

> class (RegionIO m) => SafeRegionIO m
> instance
>   ( RegionIO m, ResourceIO res
>   ) => SafeRegionIO (RGT s res m)



> instance RegionIO IO where
>   brace   = bracket
>   capture = catch
>   lyftIO  = id

> instance (RegionIO m) => RegionIO (EnvT r m) where
>   brace before after during = EnvT $ \r ->
>     brace (runEnvT r before) (runEnvT r . after) (runEnvT r . during)
>   capture m handle = EnvT $ \r ->
>     capture (runEnvT r m) (runEnvT r . handle)
>   lyftIO = lift . lyftIO

> instance (RegionIO m) => RegionIO (RGT s res m) where
>   brace before after during = RGT $
>     brace (unRGT before) (unRGT . after) (unRGT . during)
>   capture m handle = RGT $
>     capture (unRGT m) (unRGT . handle)
>   lyftIO = RGT . lyftIO



> -- resources which can be initialized and closed
> class ResourceIO t where
>   initR  :: IO t
>   closeR :: t -> IO ()

> instance ResourceIO [HandleR] where
>   initR  = return []
>   closeR = mapM_ closeHandleR

> withRegion
>   :: forall res m a
>    . ( ResourceIO res, RegionIO m )
>   => (forall s. RGT s res m a) -> m a
> withRegion m = brace before after during
>   where
>     before :: m (IORef res)
>     before = lyftIO (initR >>= newIORef)
> 
>     after :: IORef res -> m ()
>     after r = lyftIO (readIORef r >>= closeR)
> 
>     during :: IORef res -> m a
>     during r = runEnvT r (unRGT m)

> withHandlesRegion
>   :: (RegionIO m)
>   => (forall s. HandlesRGT s m a) -> m a
> withHandlesRegion = withRegion

-- Our (safe) handle is labeled with the monad where it was created

> newtype Safe (m :: * -> *) a = Safe a

> newSafeHandle
>   :: ( m ~ HandlesRGT s m1, RegionIO m1 )
>   => FilePath -> IOMode -> m (Either IOError (Safe m Handle))
> newSafeHandle name mode = RGT $ do
>   result <- lyftIO $ try $ openFile name mode
>   case result of
>     Left err ->
>       return $ Left err
>     Right h -> do
>       handles <- ask
>       hr <- lyftIO $ newHandleR h
>       lyftIO $ modifyIORef handles (hr:)
>       return $ Right $ Safe h



shGetLine (SHandle h) = lIO (hGetLine h)


newtype IORT s m v = IORT{ unIORT:: ReaderT (IORef [HandleR]) m v } 
    deriving (Functor, Applicative, Monad)

type SIO s v = IORT s IO
runSIO :: (forall s. SIO s v) -> IO v
liftSIO :: Monad m => IORT r m a -> IORT s (IORT r m) a

shGetLine :: (MonadRaise m1 m2, RMonadIO m2) => SHandle m1 -> m2 String
shPutStrLn :: (MonadRaise m1 m2, RMonadIO m2) => SHandle m1 -> String -> m2 ()
shIsEOF :: (MonadRaise m1 m2, RMonadIO m2) => SHandle m1 -> m2 Bool
shThrow :: RMonadIO m => Exception -> m a
shCatch :: RMonadIO m => m a -> (Exception -> m a) -> m a
shReport :: RMonadIO m => String -> m ()
sNewIORef :: RMonadIO m => a -> m (IORef a)
sReadIORef :: RMonadIO m => IORef a -> m a
sWriteIORef :: RMonadIO m => IORef a -> a -> m ()
shDup :: RMonadIO m => SHandle (IORT s (IORT r m)) -> IORT s (IORT r m) (SHandle (IORT r m))
