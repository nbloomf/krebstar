> module Kreb.Category.Class.ProfunctorC where

> import Kreb.Category.Class.CategoryC



> class
>   ( CategoryC obj1 mor1, CategoryC obj2 mor2, CategoryC obj3 mor3
>   , forall x y. (obj1 x, obj2 y) => obj3 (p x y)
>   ) => ProfunctorC obj1 mor1 obj2 mor2 obj3 mor3 (p :: * -> * -> *)
>   where
>     dimapC
>       :: ( obj1 x1, obj1 x2, obj2 y1, obj2 y2 )
>       => mor1 x2 x1 -> mor2 y1 y2
>       -> mor3 (p x1 y1) (p x2 y2)

Examples:

> instance
>   ( forall x y. (obj1 x, obj2 y) => obj3 (Map obj1 obj2 x y)
>   ) => ProfunctorC
>     (obj1 :: Obj) (MapOn obj1)
>     (obj2 :: Obj) (MapOn obj2)
>     (obj3 :: Obj) (MapOn obj3)
>     (Map obj1 obj2)
>   where
>     dimapC (Map f) (Map g) = Map $
>       \(Map h) -> Map (g . h . f)
