> module Kreb.Prop.Class.Positive (
>     Positive(..)
>   , NonNegative(..)
> ) where

> import qualified Kreb.Format as Fmt
> import           Kreb.Format (display, string, (<+>))

> import Kreb.Prop.Arb
> import Kreb.Prop.Class.FromZZ



> newtype Positive a = Positive
>   { fromPositive :: a
>   } deriving (Eq, Ord, Show)
> 
> instance (Fmt.Display a) => Fmt.Display (Positive a) where
>   display (Positive k) = string "Positive" <+> display k
> 
> instance (Ord a, FromZZ a) => FromZZ (Positive a) where
>   fromZZ = Positive . max (fromZZ 1) . fromZZ
> 
> instance
>   ( Eq a, Ord a, FromZZ a, Arb a
>   ) => Arb (Positive a)
>   where
>     arb = do
>       k <- arb
>       return $ Positive $ max (fromZZ 1) k
> 
> instance
>   ( Eq a, Ord a, FromZZ a, Prune a
>   ) => Prune (Positive a)
>   where
>     prune (Positive a) =
>       if a == fromZZ 1
>         then []
>         else map Positive
>           $ filter (\u -> u >= fromZZ 1)
>           $ prune a
> 
> instance
>   ( CoArb a
>   ) => CoArb (Positive a)
>   where
>     coarb (Positive a) = coarb a



> newtype NonNegative a = NonNegative
>   { fromNonNegative :: a
>   } deriving (Eq, Ord, Show)
> 
> instance (Fmt.Display a) => Fmt.Display (NonNegative a) where
>   display (NonNegative k) = string "NonNegative" <+> display k
> 
> instance (Ord a, FromZZ a) => FromZZ (NonNegative a) where
>   fromZZ = NonNegative . max (fromZZ 0) . fromZZ
> 
> instance
>   ( Eq a, Ord a, FromZZ a, Arb a
>   ) => Arb (NonNegative a)
>   where
>     arb = do
>       k <- arb
>       return $ NonNegative $ max (fromZZ 0) k
> 
> instance
>   ( Eq a, Ord a, FromZZ a, Prune a
>   ) => Prune (NonNegative a)
>   where
>     prune (NonNegative a) =
>       if a == fromZZ 0
>         then []
>         else map NonNegative
>           $ filter (\u -> u >= fromZZ 0)
>           $ prune a
> 
> instance (CoArb a) => CoArb (NonNegative a) where
>   coarb (NonNegative a) = coarb a
