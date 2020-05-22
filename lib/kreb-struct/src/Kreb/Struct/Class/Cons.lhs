> {-# LANGUAGE QuantifiedConstraints #-}
> {-# LANGUAGE FlexibleContexts #-}

> module Kreb.Struct.Class.Cons where

> import GHC.Exts (Constraint)

> import Kreb.Struct.Class.Container
> import Kreb.Struct.Class.Subset
> import Kreb.Struct.Class.NonEmpty
> import Kreb.Struct.Class.Singleton

> import Kreb.Control.Unconstrained



> class
>   ( Container t
>   , forall a. (ContainerConstraint t a) => Semigroup (t a)
>   ) => Cons (t :: * -> *)
>   where
>     cons
>       :: ( ContainerConstraint t a )
>       => a -> t a -> t a
> 
>     uncons
>       :: ( ContainerConstraint t a )
>       => t a -> Maybe (a, t a)
> 
> readFirst
>   :: ( Cons t, ContainerConstraint t a )
>   => t a -> Maybe a
> readFirst = fmap fst . uncons

> instance Cons [] where
>   cons
>     :: ( Unconstrained a )
>     => a -> [a] -> [a]
>   cons a as = a:as
> 
>   uncons
>     :: ( Unconstrained a )
>     => [a] -> Maybe (a, [a])
>   uncons x = case x of
>     [] -> Nothing
>     a:as -> Just (a, as)

> class (Singleton t, Cons t) => SingletonCons t
> class (Subset t, Cons t, Cons (SupersetOf t)) => SubsetCons t



> class
>   ( Container t
>   , forall a. (ContainerConstraint t a) => Semigroup (t a)
>   ) => Snoc (t :: * -> *)
>   where
>     snoc
>       :: ( ContainerConstraint t a )
>       => a -> t a -> t a
> 
>     unsnoc
>       :: ( ContainerConstraint t a )
>       => t a -> Maybe (a, t a)
> 
> readLast
>   :: ( Snoc t, ContainerConstraint t a )
>   => t a -> Maybe a
> readLast = fmap fst . unsnoc

> class (Singleton t, Snoc t) => SingletonSnoc t
> class (Subset t, Snoc t, Snoc (SupersetOf t)) => SubsetSnoc t



> class
>   ( Cons t, NonEmpty t, Subset t, Cons (SupersetOf t)
>   ) => UnconsNonEmpty (t :: * -> *)
>   where
>     unconsNonEmpty
>       :: ( ContainerConstraint t a )
>       => t a -> (a, SupersetOf t a)
> 
> readFirstNonEmpty
>   :: ( UnconsNonEmpty t, ContainerConstraint t a )
>   => t a -> a
> readFirstNonEmpty = fst . unconsNonEmpty



> class
>   ( Snoc t, NonEmpty t, Subset t, Snoc (SupersetOf t)
>   ) => UnsnocNonEmpty (t :: * -> *)
>   where
>     unsnocNonEmpty
>       :: ( ContainerConstraint t a )
>       => t a -> (a, SupersetOf t a)
> 
> readLastNonEmpty
>   :: ( UnsnocNonEmpty t, ContainerConstraint t a )
>   => t a -> a
> readLastNonEmpty = fst . unsnocNonEmpty



> class (Cons t, Snoc t) => ConsSnoc t
