> module Kreb.Control.LocalSt where



> newtype Local s a = Local
>   { unLocal :: s -> (s,a)
>   }
> 
> localSt
>   :: s -> Local s a -> s
> localSt s x =
>   fst $ unLocal x s
> 
> instance Functor (Local s) where
>   fmap f x = Local $ \s1 ->
>     let
>       (s2, a) = unLocal x s1
>     in (s2, f a)
> 
> instance Applicative (Local s) where
>   pure a = Local $ \s -> (s, a)
> 
>   f <*> x = Local $ \s1 ->
>     let
>       (s2, f') = unLocal f s1
>       (s3, x') = unLocal x s2
>     in (s3, f' x')
> 
> instance Monad (Local s) where
>   return = pure
> 
>   x >>= f = Local $ \s1 ->
>     let
>       (s2, x') = unLocal x s1
>     in unLocal (f x') s2

> readSt
>   :: (s -> t) -> Local s t
> readSt f = Local $ \s -> (s, f s)

> editSt
>   :: (s -> s) -> Local s ()
> editSt f = Local $ \s -> (f s, ())









