> module Kreb.Control.Tuple where



> pair :: a -> b -> (a,b)
> pair a b = (a,b)

> swap :: (a,b) -> (b,a)
> swap (a,b) = (b,a)

> uncurry3
>   :: (a1 -> a2 -> a3 -> b)
>   -> (a1,a2,a3) -> b
> uncurry3 f (a1,a2,a3) =
>   f a1 a2 a3

> uncurry4
>   :: (a1 -> a2 -> a3 -> a4 -> b)
>   -> (a1,a2,a3,a4) -> b
> uncurry4 f (a1,a2,a3,a4) =
>   f a1 a2 a3 a4

> uncurry5
>   :: (a1 -> a2 -> a3 -> a4 -> a5 -> b)
>   -> (a1,a2,a3,a4,a5) -> b
> uncurry5 f (a1,a2,a3,a4,a5) =
>   f a1 a2 a3 a4 a5

> uncurry6
>   :: (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> b)
>   -> (a1,a2,a3,a4,a5,a6) -> b
> uncurry6 f (a1,a2,a3,a4,a5,a6) =
>   f a1 a2 a3 a4 a5 a6
