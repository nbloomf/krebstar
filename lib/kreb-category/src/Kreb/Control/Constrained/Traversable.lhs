> module Kreb.Control.Constrained.Traversable where

> import GHC.Exts (Constraint)

> import Kreb.Control.Constrained.Functor

> class
>   ( Foldable t, ConstrainedFunctor t
>   ) => ConstrainedTraversable (t :: * -> *)
>   where
>     traverseC
>       :: ( Applicative f, FunctorConstraint t a, FunctorConstraint t b )
>       => (a -> f b) -> t a -> f (t b)
> 
>     sequenceAC
>       :: ( Applicative f, FunctorConstraint t a, FunctorConstraint t (f a) )
>       => t (f a) -> f (t a)
> 
>     consumeC
>       :: ( Applicative f, FunctorConstraint t a, FunctorConstraint t (f a) )
>       => (t a -> b) -> t (f a) -> f b
>     consumeC f = fmap f . traverseC id

> mapMC
>   :: ( Monad m, ConstrainedTraversable t
>      , FunctorConstraint t a, FunctorConstraint t b )
>   => (a -> m b) -> t a -> m (t b)
> mapMC = traverseC

> sequenceC
>   :: ( Monad m, ConstrainedTraversable t
>      , FunctorConstraint t a, FunctorConstraint t (m a) )
>   => t (m a) -> m (t a)
> sequenceC = sequenceAC
