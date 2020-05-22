> {-# LANGUAGE QuantifiedConstraints #-}
> {-# LANGUAGE FlexibleContexts #-}

> module Kreb.Struct.Class.Reverse where

> import Kreb.Struct.Class.Container
> import Kreb.Struct.Class.Subset
> import Kreb.Struct.Class.NonEmpty
> import Kreb.Struct.Class.Singleton
> import Kreb.Struct.Class.Cons



> class (Container t) => Reverse t where
>   reverse
>     :: ( ContainerConstraint t a )
>     => t a -> t a



> class
>   ( Reverse t
>   , forall a. (ContainerConstraint t a) => Semigroup (t a)
>   ) => ReverseSemigroup t



> class
>   ( Reverse t
>   , forall a. (ContainerConstraint t a) => Monoid (t a)
>   ) => ReverseMonoid t



> class (Reverse t, Subset t, Reverse (SupersetOf t)) => ReverseSubset t
> class (Reverse t, Singleton t) => ReverseSingleton t
> class (Reverse t, Cons t, Snoc t) => ReverseConsSnoc t
