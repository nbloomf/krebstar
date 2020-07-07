---
title: Traversable Functors
author: nbloomf
---

::: frontmatter

> module Kreb.Category.Class.TraversableC where
> 
> import Kreb.Category.Class.FunctorC

:::



Introduction
------------

A functor $F$ is called _traversable_ if it distributes with every applicative functor. Giving a full categorical definition requires talking about applicatives first, which I'm deferring for now. The primitives and laws used here come from [An Investigation of the Laws of Traversals](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.750.5714&rep=rep1&type=pdf) by Jaskelioff and Rypacek.

> class
>   ( Foldable t, FunctorC obj1 mor1 obj2 mor2 t
>   ) => TraversableC obj1 mor1 obj2 mor2 t
>   where
>     traverseC
>       :: ( Applicative f, obj1 a, obj1 b )
>       => (a -> f b) -> t a -> f (t b)
> 
>     sequenceAC
>       :: ( Applicative f, obj1 a, obj1 (f a) )
>       => t (f a) -> f (t a)
> 
>     consumeC
>       :: ( Applicative f, obj1 a, obj1 (f a) )
>       => (t a -> b) -> t (f a) -> f b

We define some special cases when interacting with monads instead of applicatives.

> mapMC
>   :: forall obj1 mor1 obj2 mor2 t m a b
>    . ( Monad m, TraversableC obj1 mor1 obj2 mor2 t
>      , obj1 a, obj1 b )
>   => (a -> m b) -> t a -> m (t b)
> mapMC = traverseC @obj1 @mor1 @obj2 @mor2 @t @m @a
> 
> sequenceC
>   :: forall obj1 mor1 obj2 mor2 t m a
>    . ( Monad m, TraversableC obj1 mor1 obj2 mor2 t
>      , obj1 a, obj1 (m a) )
>   => t (m a) -> m (t a)
> sequenceC = sequenceAC @obj1 @mor1 @obj2 @mor2 @t @m @a
