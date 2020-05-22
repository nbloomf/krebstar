> module Kreb.Control.Monad.Flow where

> import Control.Monad

> concatM
>   :: ( Monad m )
>   => [a -> m a] -> (a -> m a)
> concatM = foldr (>=>) return
