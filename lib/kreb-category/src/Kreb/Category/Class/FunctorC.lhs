---
title: Functors
author: nbloomf
---

::: frontmatter

> {-# LANGUAGE UndecidableInstances #-}
> 
> module Kreb.Category.Class.FunctorC where
> 
> import Kreb.Control
> 
> import Kreb.Category.Class.CategoryC

:::



Introduction
------------

Whenever we have a kind of structure, the transformations which preserve that structure are usually interesting. Categories are a kind of structure, and their transformations are called _functors_.

More precisely, given two categories $\mathcal{C}$ and $\mathcal{D}$, a _functor_ $F$ associates to every object $A$ of $\mathcal{C}$ an object $FA$ of $\mathcal{D}$, and to every morphism $\varphi$ of $\mathcal{C}$ a morphism $F\varphi$ of $\mathcal{D}$ such that the following properties hold.

- For every object $A$ of $\mathcal{C}$, we have $F\id_A = \id_{FA}$.
- If $\varphi : A \rightarrow B$ and $\psi : B \rightarrow C$ are morphisms in $\mathcal{C}$, then $F(\psi \circ \varphi) = F\psi \circ F\varphi$.

In categories derived from $\Hask$, objects are types. So the object component of a functor is a type constructor with kind `* -> *`. We can enforce the morphism component using a type class, like so:

> class
>   ( CategoryC obj1 mor1, CategoryC obj2 mor2
>   , forall x. ( obj1 x ) => obj2 (f x) -- f maps obj1 to obj2
>   ) => FunctorC obj1 mor1 obj2 mor2 (f :: * -> *)
>   where
>     fmapC
>       :: ( obj1 x, obj1 y )
>       => mor1 x y
>       -> mor2 (f x) (f y)

This is a generalization of the standard `Functor` class.



Examples
--------

> instance
>   ( forall x. (obj1 x) => obj2 (Identity x)
>   ) => FunctorC obj1 (->) obj2 (->) Identity
>   where
>     fmapC f (Identity x) = Identity (f x)
