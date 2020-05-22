> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE QuantifiedConstraints #-}

> module Kreb.Struct.Class.Ideal where

> import GHC.Exts (Constraint)

> import Kreb.Struct.Class.Container
> import Kreb.Struct.Class.Subset

> class
>   ( Subset t
>   , forall a. (ContainerConstraint t a) => Semigroup (t a)
>   , forall a. (ContainerConstraint (SupersetOf t) a) => Semigroup (SupersetOf t a)
>   ) => Subsemigroup t



> class (Subsemigroup t) => Ideal t where
>   (@>)
>     :: ( ContainerConstraint t a )
>     => t a -> SupersetOf t a -> t a
> 
>   (<@)
>     :: ( ContainerConstraint t a )
>     => SupersetOf t a -> t a -> t a

> (>@>)
>   :: ( Ideal t, ContainerConstraint t a )
>   => t a -> SupersetOf t a -> SupersetOf t a
> u >@> v = inject (u @> v)
> 
> (<@<)
>   :: ( Ideal t, ContainerConstraint t a )
>   => SupersetOf t a -> t a -> SupersetOf t a
> u <@< v = inject (u <@ v)
