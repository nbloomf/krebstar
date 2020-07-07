> module Kreb.Category.Data.IdC where

> import Kreb.Category.Class

> data Id (obj :: Obj) a where
>   Id :: ( obj a ) => a -> Id obj a
> 

> instance
>   ( forall x. ( obj1 x ) => obj2 (Id obj1 x)
>   ) => FunctorC
>     (obj1 :: Obj) (->) -- from
>     (obj2 :: Obj) (->) -- to
>     (Id obj1)
>   where
>     fmapC f (Id x) = Id (f x)

> instance
>   ( forall x. ( obj1 x ) => obj2 (Id obj1 x)
>   ) => FunctorC
>     (obj1 :: Obj) (MapOn obj1) -- from
>     (obj2 :: Obj) (MapOn obj2) -- to
>     (Id obj1)
>   where
>     fmapC (Map f) =
>       let g (Id x) = Id (f x)
>       in Map g
