> module Kreb.Control.Monad.LiftIO (
>     LiftIO(..)
> ) where

> class ( Monad m ) => LiftIO m where
>   liftIO :: IO a -> m a
> 
> instance LiftIO IO where
>   liftIO = id
