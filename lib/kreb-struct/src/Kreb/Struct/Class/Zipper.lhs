> {-# LANGUAGE TypeFamilyDependencies #-}
> {-# LANGUAGE FlexibleContexts #-}

> module Kreb.Struct.Class.Zipper where

> import Kreb.Struct.Class.Container
> import Kreb.Struct.Class.NonEmpty

> class (NonEmpty (Zipped t)) => Zipper (t :: * -> *) where
>   type Zipped t = (u :: * -> *) | u -> t
> 
>   toZipper
>     :: ( ContainerConstraint t a )
>     => Zipped t a -> t a
> 
>   fromZipper
>     :: ( ContainerConstraint t a )
>     => t a -> Zipped t a
