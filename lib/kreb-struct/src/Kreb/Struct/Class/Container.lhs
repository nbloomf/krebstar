> module Kreb.Struct.Class.Container where

> import GHC.Exts (Constraint)

> import Kreb.Control.Unconstrained

> class Container (t :: * -> *) where
>   type ContainerConstraint t :: * -> Constraint

> instance Container [] where
>   type ContainerConstraint [] = Unconstrained
