> {-# LANGUAGE FlexibleContexts #-}

> module Kreb.Struct.Class.Singleton where

> import Kreb.Control.Unconstrained

> import Kreb.Struct.Class.Container
> import Kreb.Struct.Class.Subset
> import Kreb.Struct.Class.NonEmpty

laws
- isSingleton . singleton == const True

> class (Container t) => Singleton (t :: * -> *) where
>   singleton
>     :: ( ContainerConstraint t a )
>     => a -> t a
> 
>   isSingleton
>     :: ( ContainerConstraint t a )
>     => t a -> Bool

> instance Singleton [] where
>   singleton
>     :: ( Unconstrained a )
>     => a -> [a]
>   singleton a = [a]
> 
>   isSingleton
>     :: ( Unconstrained a )
>     => [a] -> Bool
>   isSingleton xs = case xs of
>     [_] -> True
>     _ -> False

> class (Subset t, Singleton t, Singleton (SupersetOf t)) => SubsetSingleton (t :: * -> *)

> class (NonEmpty t, SubsetSingleton t) => NonEmptySingleton (t :: * -> *)
