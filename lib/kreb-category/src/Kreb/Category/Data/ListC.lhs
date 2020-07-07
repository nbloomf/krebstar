> module Kreb.Category.Data.ListC where

> import Kreb.Category.Class


> data List (obj :: Obj) a where
>   Nil  :: List obj a
>   Cons :: ( obj a ) => a -> List obj a -> List obj a

> instance
>   ( forall x. ( obj1 x ) => obj2 (List obj1 x)
>   ) => FunctorC
>     (obj1 :: Obj) (->) -- from
>     (obj2 :: Obj) (->) -- to
>     (List obj1)
>   where
>     fmapC f x = case x of
>       Nil -> Nil
>       Cons a as -> Cons (f a) (fmapC @obj1 @(->) @obj2 @(->) f as)

> instance
>   ( forall x. ( obj1 x ) => obj2 (List obj1 x)
>   ) => FunctorC
>     (obj1 :: Obj) (MapOn obj1) -- from
>     (obj2 :: Obj) (MapOn obj2) -- to
>     (List obj1)
>   where
>     fmapC (Map f) =
>       let
>         g x = case x of
>           Nil -> Nil
>           Cons a as -> Cons (f a) (g as)
>       in Map g
