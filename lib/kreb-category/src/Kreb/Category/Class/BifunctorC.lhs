> {-# LANGUAGE OverloadedStrings #-}

> module Kreb.Category.Class.BifunctorC (
>     BifunctorC(..)
> ) where
> 
> import Kreb.Control
> 
> import Kreb.Category.Class.CategoryC



> class
>   ( CategoryC obj1 mor1, CategoryC obj2 mor2, CategoryC obj3 mor3
>   , forall x y. (obj1 x, obj2 y) => obj3 (f x y)
>   ) => BifunctorC obj1 mor1 obj2 mor2 obj3 mor3 (f :: * -> * -> *)
>   where
>     bimapC
>       :: ( obj1 x1, obj1 x2, obj2 y1, obj2 y2 )
>       => mor1 x1 x2 -> mor2 y1 y2
>       -> mor3 (f x1 y1) (f x2 y2)



Examples:







> data RightZero (obj1 :: Obj) (obj2 :: Obj) a b where
>   RightZero
>     :: ( obj1 a, obj2 b )
>     => b -> RightZero obj1 obj2 a b
> 
> instance
>   ( forall x y. (obj1 x, obj2 y) => obj3 (RightZero obj1 obj2 x y)
>   ) => BifunctorC
>     (obj1 :: Obj) (MapOn obj1)
>     (obj2 :: Obj) (MapOn obj2)
>     (obj3 :: Obj) (MapOn obj3)
>     (RightZero obj1 obj2)
>   where
>     bimapC (Map _) (Map g) =
>       let h (RightZero b) = RightZero (g b)
>       in Map h



> data LeftZero (obj1 :: Obj) (obj2 :: Obj) a b where
>   LeftZero
>     :: ( obj1 a, obj2 b )
>     => a -> LeftZero obj1 obj2 a b
> 
> instance
>   ( forall x y. (obj1 x, obj2 y) => obj3 (LeftZero obj1 obj2 x y)
>   ) => BifunctorC
>     (obj1 :: Obj) (MapOn obj1)
>     (obj2 :: Obj) (MapOn obj2)
>     (obj3 :: Obj) (MapOn obj3)
>     (LeftZero obj1 obj2)
>   where
>     bimapC (Map f) (Map _) =
>       let h (LeftZero a) = LeftZero (f a)
>       in Map h
