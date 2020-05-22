> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE Rank2Types #-}
> {-# LANGUAGE KindSignatures #-}

> module Kreb.Arith.Class where

> import Data.Proxy
> import Kreb.Reflect.Nat



> class Canonical a where
>   canonize :: a -> a



> class (Ord d) => Dense d where
>   chooseBetween :: d -> d -> d
> 
> indexBetween
>   :: ( Dense d )
>   => d -> d -> [a] -> [(a,d)]
> indexBetween d1 d2 as = case as of
>   [] -> []
>   w:ws ->
>     let v = chooseBetween d1 d2
>     in (w,v) : indexBetween v d2 ws



> data Augmented a
>   = Augmented a
>   | Infimum
>   | Supremum
>   deriving (Eq, Show)

> instance (Ord a) => Ord (Augmented a) where
>   compare x y = case (x,y) of
>     (Infimum,     Infimum)     -> EQ
>     (Infimum,     _)           -> LT
>     (_,           Infimum)     -> GT
>     (Augmented u, Augmented v) -> compare u v
>     (Supremum,    Supremum)    -> EQ
>     (Supremum,    _)           -> GT
>     (_,           Supremum)    -> LT



> class IsBase d where
>   toBase   :: Proxy d -> Int
>   showBase :: Proxy d -> String
>   showBase = show . toBase
> 
> instance
>   ( ReflectNat d
>   ) => IsBase (Nat d)
>   where
>     toBase _ = reflectNat (Proxy :: Proxy d)
> 
> withBase
>   :: Int
>   -> (forall p. ( ReflectNat p ) => Proxy (Nat p) -> a)
>   -> a
> withBase k cont = reifyNat k $
>   \(Proxy :: Proxy u) -> cont (Proxy :: Proxy (Nat u))
> 
> class WitnessBase (f :: * -> *) where
>   witnessBase
>     :: ( IsBase d1, IsBase d2 )
>     => Proxy d2 -> f d1 -> f d2
