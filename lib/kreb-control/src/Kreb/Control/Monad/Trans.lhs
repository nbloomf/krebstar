> {-# LANGUAGE QuantifiedConstraints #-}

> module Kreb.Control.Monad.Trans where

> import Kreb.Control.Monad





The signature of @local@ from @MonadReadOnly@

> type Local r m a = (r -> r) -> m a -> m a

Class representing monad transformers through which @local@ from @MonadReadOnly@ can be lifted.

> class (MonadTrans t) => LiftLocal t where
>   liftLocal
>     :: ( Monad m )
>     => (forall a. Local r    m  a)
>     -> (forall a. Local r (t m) a)
