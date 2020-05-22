> module Kreb.Optics.Base where

> import Kreb.Optics.Optics



> _1of1 :: Lens a1 a1
> _1of1 = lens
>   (\a1 -> a1)
>   (\_ a1 -> a1)

> _1of2 :: Lens a1 (a1,a2)
> _1of2 = lens
>   (\(a1,_) -> a1)
>   (\(_,a2) a1 -> (a1,a2))
> 
> _2of2 :: Lens a2 (a1,a2)
> _2of2 = lens
>   (\(_,a2) -> a2)
>   (\(a1,_) a2 -> (a1,a2))

> _1of3 :: Lens a1 (a1,a2,a3)
> _1of3 = lens
>   (\(a1,_,_) -> a1)
>   (\(_,a2,a3) a1 -> (a1,a2,a3))
> 
> _2of3 :: Lens a2 (a1,a2,a3)
> _2of3 = lens
>   (\(_,a2,_) -> a2)
>   (\(a1,_,a3) a2 -> (a1,a2,a3))
> 
> _3of3 :: Lens a3 (a1,a2,a3)
> _3of3 = lens
>   (\(_,_,a3) -> a3)
>   (\(a1,a2,_) a3 -> (a1,a2,a3))

> _Left :: Prism a (Either a b)
> _Left = prism
>   (\x -> case x of
>     Left  a -> Right a
>     Right _ -> Left x)
>   (\a -> Left a)
> 
> _Right :: Prism b (Either a b)
> _Right = prism
>   (\x -> case x of
>     Left  _ -> Left x
>     Right b -> Right b)
>   (\b -> Right b)
