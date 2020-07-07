---
title: Bifunctors
author: nbloomf
---

::: frontmatter

> module Kreb.Category.Class.BifunctorC where
> 
> import Kreb.Category.Class.CategoryC

:::



Introduction
------------

Given categories $\mathcal{C}_1$ and $\mathcal{C}_2$, we can form a _product category_, $\mathcal{C}_1 \times \mathcal{C}_2$. Objects of this category are pairs $(A_1,A_2)$ of objects, with $A_1$ from $\mathcal{C}_1$ and $A_2$ from $\mathcal{C}_2$, and similarly morphisms are pairs of morphisms.

Now suppose we have a third category $\mathcal{D}$. A _bifunctor_ $B : \mathcal{C}_1 \times \mathcal{C}_2 \rightarrow \mathcal{D}$ associates objects to objects and morphisms such that $B(-,-)$ is functorial in "both slots". That is, holding objects $A_1$ and $A_2$ fixed, both $B(A_1,-)$ and $B(-,A_2)$ are functors. This is analogous to the bilinear maps we study in linear algebra.

As with functors, we can translate this concept to Haskell's type system. The object part of a bifunctor is a type constructor with kind `* -> * -> *`, and we can attach the morphism part using a type class.

> class
>   ( CategoryC obj1 mor1, CategoryC obj2 mor2, CategoryC obj3 mor3
>   , forall x y. (obj1 x, obj2 y) => obj3 (f x y)
>   ) => BifunctorC obj1 mor1 obj2 mor2 obj3 mor3 (f :: * -> * -> *)
>   where
>     bimapC
>       :: ( obj1 x1, obj1 x2, obj2 y1, obj2 y2 )
>       => mor1 x1 x2 -> mor2 y1 y2
>       -> mor3 (f x1 y1) (f x2 y2)



Examples
--------

The interesting bifunctors get their own modules, but we can describe some less interesting examples here: the right and left zero bifunctors ignore one of the factors.

> data RightZeroC (obj1 :: Obj) (obj2 :: Obj) a b where
>   RightZeroC
>     :: ( obj1 a, obj2 b )
>     => b -> RightZeroC obj1 obj2 a b
> 
> instance
>   ( forall x y. (obj1 x, obj2 y) => obj3 (RightZeroC obj1 obj2 x y)
>   ) => BifunctorC
>     (obj1 :: Obj) (MapOn obj1)
>     (obj2 :: Obj) (MapOn obj2)
>     (obj3 :: Obj) (MapOn obj3)
>     (RightZeroC obj1 obj2)
>   where
>     bimapC (Map _) (Map g) =
>       let h (RightZeroC b) = RightZeroC (g b)
>       in Map h
> 
> data LeftZeroC (obj1 :: Obj) (obj2 :: Obj) a b where
>   LeftZeroC
>     :: ( obj1 a, obj2 b )
>     => a -> LeftZeroC obj1 obj2 a b
> 
> instance
>   ( forall x y. (obj1 x, obj2 y) => obj3 (LeftZeroC obj1 obj2 x y)
>   ) => BifunctorC
>     (obj1 :: Obj) (MapOn obj1)
>     (obj2 :: Obj) (MapOn obj2)
>     (obj3 :: Obj) (MapOn obj3)
>     (LeftZeroC obj1 obj2)
>   where
>     bimapC (Map f) (Map _) =
>       let h (LeftZeroC a) = LeftZeroC (f a)
>       in Map h
