> module Kreb.Struct.Class.NonEmpty where

> import Kreb.Struct.Class.Container
> import Kreb.Struct.Class.Subset

> class (Subset t) => NonEmpty (t :: * -> *) where
>   empty
>     :: ( ContainerConstraint t a )
>     => SupersetOf t a
> 
>   isEmpty
>     :: ( ContainerConstraint t a )
>     => SupersetOf t a -> Bool
>   isEmpty x = case restrict x of
>     Nothing -> True; Just _ -> False
