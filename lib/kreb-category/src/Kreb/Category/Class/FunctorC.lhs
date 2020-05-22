---
title: Functors
author: nbloomf
---

::: frontmatter

> module Kreb.Category.Class.FunctorC () where
> 
> import Kreb.Category.Class.CategoryC

:::





Laws:
- fmapC' unit == unit
- fmapC' (comp f g) == comp (fmapC' f) (fmapC' g)

> class
>   ( CategoryC obj1 mor1, CategoryC obj2 mor2
>   , forall x. ( obj1 x ) => obj2 (f x)
>   ) => FunctorC obj1 mor1 obj2 mor2 (f :: * -> *)
>   where
>     fmapC'
>       :: ( obj1 x, obj1 y )
>       => mor1 x y
>       -> mor2 (f x) (f y)





Examples:

> -- constrained Identity
> data Id (obj :: Obj) a where
>   Id :: ( obj a ) => a -> Id obj a
> 
> instance
>   ( forall x. ( obj1 x ) => obj2 (Id obj1 x)
>   ) => FunctorC
>     (obj1 :: Obj) (MapOn obj1) -- from
>     (obj2 :: Obj) (MapOn obj2) -- to
>     (Id obj1)
>   where
>     fmapC' (Map f) =
>       let g (Id x) = Id (f x)
>       in Map g

> -- constrained []
> data List (obj :: Obj) a where
>   Nil  :: List obj a
>   Cons :: ( obj a ) => a -> List obj a -> List obj a
> 
> instance
>   ( forall x. ( obj1 x ) => obj2 (List obj1 x)
>   ) => FunctorC
>     (obj1 :: Obj) (MapOn obj1) -- from
>     (obj2 :: Obj) (MapOn obj2) -- to
>     (List obj1)
>   where
>     fmapC' (Map f) =
>       let
>         g x = case x of
>           Nil -> Nil
>           Cons a as -> Cons (f a) (g as)
>       in Map g

> -- constrained Maybe
> data MaybeC (obj :: Obj) a where
>   NothingC :: MaybeC obj a
>   JustC    :: ( obj a ) => a -> MaybeC obj a
> 
> instance
>   ( forall x. ( obj1 x ) => obj2 (MaybeC obj1 x)
>   ) => FunctorC
>     (obj1 :: Obj) (MapOn obj1) -- from
>     (obj2 :: Obj) (MapOn obj2) -- to
>     (MaybeC obj1)
>   where
>     fmapC' (Map f) =
>       let
>         g x = case x of
>           NothingC -> NothingC
>           JustC a -> JustC (f a)
>       in Map g
