> module Kreb.Category.Data.MaybeC where

> import Kreb.Category.Class

> data MaybeC (obj :: Obj) a where
>   NothingC :: MaybeC obj a
>   JustC    :: ( obj a ) => a -> MaybeC obj a
> 

> instance
>   ( forall x. ( obj1 x ) => obj2 (MaybeC obj1 x)
>   ) => FunctorC
>     (obj1 :: Obj) (->) -- from
>     (obj2 :: Obj) (->) -- to
>     (MaybeC obj1)
>   where
>     fmapC f x = case x of
>       NothingC -> NothingC
>       JustC a -> JustC (f a)

> instance
>   ( forall x. ( obj1 x ) => obj2 (MaybeC obj1 x)
>   ) => FunctorC
>     (obj1 :: Obj) (MapOn obj1) -- from
>     (obj2 :: Obj) (MapOn obj2) -- to
>     (MaybeC obj1)
>   where
>     fmapC (Map f) =
>       let
>         g x = case x of
>           NothingC -> NothingC
>           JustC a -> JustC (f a)
>       in Map g
