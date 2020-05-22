> {-# LANGUAGE TypeFamilyDependencies #-}

> module Kreb.Struct.Class.Subset where

> import GHC.Exts (Constraint)

> import Kreb.Struct.Class.Container

> class (Container t) => Subset t where
>   type SupersetOf t = (u :: * -> *) | u -> t
> 
>   inject
>     :: ( ContainerConstraint t a )
>     => t a -> SupersetOf t a
> 
>   restrict
>     :: ( ContainerConstraint t a )
>     => SupersetOf t a -> Maybe (t a)
