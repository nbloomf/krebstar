> module Kreb.Struct.Class.Zipper.Linear where

> import Kreb.Struct.Class.Container

laws
- alterPointer (g . f) == alterPointer g . alterPointer f

> class (Container t) => LinearZipper t where
>   readPointer
>     :: ( ContainerConstraint t a )
>     => t a -> Maybe a
> 
>   alterPointer
>     :: ( ContainerConstraint t a )
>     => (a -> a) -> t a -> t a
> 
>   alterPointerM
>     :: ( ContainerConstraint t a, Monad m )
>     => (a -> m a) -> t a -> m (t a)
> 
>   isAtStart
>     :: ( ContainerConstraint t a )
>     => t a -> Bool
> 
>   isAtEnd
>     :: ( ContainerConstraint t a )
>     => t a -> Bool
> 
>   moveTowardStart
>     :: ( ContainerConstraint t a )
>     => t a -> t a
> 
>   moveTowardEnd
>     :: ( ContainerConstraint t a )
>     => t a -> t a
> 
>   moveToStart
>     :: ( ContainerConstraint t a )
>     => t a -> t a
> 
>   moveToEnd
>     :: ( ContainerConstraint t a )
>     => t a -> t a
