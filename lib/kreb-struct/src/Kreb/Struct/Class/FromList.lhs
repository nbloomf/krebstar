> {-# LANGUAGE QuantifiedConstraints #-}

> module Kreb.Struct.Class.FromList where

> import Control.Monad (join)

> import Kreb.Prop

> import Kreb.Struct.Class.Container
> import Kreb.Struct.Class.NonEmpty
> import Kreb.Struct.Class.Cons
> import Kreb.Struct.Class.Reverse

> class
>   ( Container t, Foldable t
>   , forall a. (ContainerConstraint t a) => Semigroup (t a)
>   ) => FromList t
>   where
>     fromList :: (ContainerConstraint t a) => [a] -> t a

> class
>   ( FromList t
>   , forall a. (ContainerConstraint t a) => Monoid (t a)
>   ) => FromListMonoid t

> class
>   ( FromList t
>   , Cons t, Snoc t, Reverse t
>   ) => FromListConsSnocReverse t

> arbFromList
>   :: ( FromList t, ContainerConstraint t a, Arb a )
>   => Sample (t a)
> arbFromList = genFromList arb
> 
> genFromList
>   :: ( FromList t, ContainerConstraint t a )
>   => Sample a -> Sample (t a)
> genFromList gen = do
>   NonNegative (k :: Int) <- arb
>   fmap fromList $ join $ vectOf <$> pure k <*> pure gen
