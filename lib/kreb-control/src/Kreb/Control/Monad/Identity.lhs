> module Kreb.Control.Monad.Identity where

> import Kreb.Control.Monad

> newtype Identity a = Identity
>   { unIdentity :: a
>   } deriving (Eq, Ord, Show)



> instance Functor Identity where
>   fmap f = Identity . f . unIdentity
> 
> instance Applicative Identity where
>   pure = Identity
> 
>   f <*> x = Identity $
>     (unIdentity f) (unIdentity x)
> 
> instance Monad Identity where
>   return = pure
> 
>   (Identity x) >>= f = f x
