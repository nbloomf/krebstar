> {-# LANGUAGE QuantifiedConstraints #-}

> module Kreb.Control.Constrained.Functor where

> import GHC.Exts (Constraint)

> import Kreb.Control.Unconstrained
> import Kreb.Control.Monad.Identity

> class
>   ConstrainedFunctor (f :: * -> *)
>   where
>     type FunctorConstraint f :: * -> Constraint
> 
>     fmapC
>       :: ( FunctorConstraint f a, FunctorConstraint f b )
>       => (a -> b) -> (f a -> f b)

> -- TODO: explore this
> class
>   ( ConstrainedFunctor f
>   , forall a. FunctorConstraint f a ~ Unconstrained a
>   ) => MyFunctor f
>   where
>     fmap_
>       :: ( FunctorConstraint f a, FunctorConstraint f b )
>       => (a -> b) -> f a -> f b
>     fmap_ = fmapC
