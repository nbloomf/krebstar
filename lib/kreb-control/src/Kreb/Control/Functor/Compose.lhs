> module Kreb.Control.Functor.Compose where



> newtype Compose f g a = Compose
>   { unCompose :: f (g a)
>   } deriving (Eq, Show)



> instance
>   ( Functor f, Functor g
>   ) => Functor (Compose f g)
>   where
>     fmap f (Compose x) = Compose (fmap (fmap f) x)

> instance
>   ( Applicative f, Applicative g
>   ) => Applicative (Compose f g)
>   where
>     pure x = Compose (pure (pure x))
>     Compose f <*> Compose x = Compose ((<*>) <$> f <*> x)
