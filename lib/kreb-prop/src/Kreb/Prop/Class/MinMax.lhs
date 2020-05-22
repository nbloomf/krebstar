> {-# LANGUAGE KindSignatures, DataKinds #-}

> module Kreb.Prop.Class.MinMax where

> import GHC.TypeLits
> import Data.Proxy

> import qualified Kreb.Format as Fmt
> import           Kreb.Format (string, (<+>), display, hsep)

> import Kreb.Prop.Arb
> import Kreb.Prop.Class.FromZZ


> newtype AtMost (n :: Nat) a = AtMost
>   { fromAtMost :: a
>   } deriving (Eq, Ord, Show)
> 
> instance (Fmt.Display a) => Fmt.Display (AtMost n a) where
>   display (AtMost k) = string "AtMost" <+> display k
> 
> instance
>   ( Ord a, FromZZ a, KnownNat n
>   ) => FromZZ (AtMost n a)
>   where
>     fromZZ k =
>       let n = natVal (Proxy :: Proxy n)
>       in AtMost $ min (fromZZ n) (fromZZ k)
> 
> instance
>   ( Eq a, Ord a, FromZZ a, Arb a, KnownNat n
>   ) => Arb (AtMost n a)
>   where
>     arb = do
>       let n = natVal (Proxy :: Proxy n)
>       k <- arb
>       return $ AtMost $ min (fromZZ n) k
> 
> instance
>   ( Eq a, Ord a, FromZZ a, Prune a, KnownNat n
>   ) => Prune (AtMost n a)
>   where
>     prune (AtMost a) =
>       let n = natVal (Proxy :: Proxy n)
>       in map AtMost
>         $ filter (\u -> u <= fromZZ n)
>         $ prune a
> 
> instance
>   ( CoArb a
>   ) => CoArb (AtMost n a)
>   where
>     coarb (AtMost a) = coarb a



> newtype Between (lo :: Nat) (hi :: Nat) a = Between
>   { fromBetween :: a
>   } deriving (Eq, Ord, Show)
> 
> instance
>   ( Fmt.Display a, KnownNat lo, KnownNat hi
>   ) => Fmt.Display (Between lo hi a)
>   where
>     display (Between k) =
>       let
>         lo = natVal (Proxy :: Proxy lo)
>         hi = natVal (Proxy :: Proxy hi)
>       in hsep
>         [ string "Between", display lo, display hi, display k ]
> 
> instance
>   ( Ord a, FromZZ a, KnownNat lo, KnownNat hi
>   ) => FromZZ (Between lo hi a)
>   where
>     fromZZ k =
>       let
>         lo = natVal (Proxy :: Proxy lo)
>         hi = natVal (Proxy :: Proxy hi)
>       in Between $ max (fromZZ lo) $ min (fromZZ hi) (fromZZ k)
> 
> instance
>   ( Eq a, Ord a, FromZZ a, Arb a, KnownNat lo, KnownNat hi
>   ) => Arb (Between lo hi a)
>   where
>     arb = do
>       let lo = natVal (Proxy :: Proxy lo)
>       let hi = natVal (Proxy :: Proxy hi)
>       k <- arb
>       return $ Between $ max (fromZZ lo) $ min (fromZZ hi) k
> 
> instance
>   ( Eq a, Ord a, FromZZ a, Prune a, KnownNat lo, KnownNat hi
>   ) => Prune (Between lo hi a)
>   where
>     prune (Between a) =
>       let
>         lo = natVal (Proxy :: Proxy lo)
>         hi = natVal (Proxy :: Proxy hi)
>       in map Between
>         $ filter (\u -> (fromZZ lo <= u) && (u <= fromZZ hi))
>         $ prune a
> 
> instance
>   ( CoArb a
>   ) => CoArb (Between lo hi a)
>   where
>     coarb (Between a) = coarb a
