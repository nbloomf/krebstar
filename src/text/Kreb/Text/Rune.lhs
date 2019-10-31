> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE Rank2Types #-}

> module Kreb.Text.Rune (
>     ZZFrac()
>   , zzfrac
>   , IsBase(..)
>   , withBase

>   , complexity

>   , chooseBetween

> ) where

> import Data.Proxy

> import Kreb.Check
> import Kreb.Reflect



> data ZZFrac d
>   = ZZFrac Integer (NonNegative Integer)
>   deriving Eq

> class IsBase d where
>   toBase   :: Proxy d -> Int
>   showBase :: Proxy d -> String
> 
> instance
>   ( ReflectNat d
>   ) => IsBase (Nat d)
>   where
>     toBase _ = reflectNat (Proxy :: Proxy d)
>     showBase _ = show (undefined :: Nat d)
> 
> withBase
>   :: Int
>   -> (forall p. (IsBase p) => Proxy p -> a)
>   -> a
> withBase k cont = reifyNat k $
>   \(Proxy :: Proxy p) -> cont (Proxy :: Proxy (Nat p))

> instance
>   ( IsBase d
>   ) => Show (ZZFrac d)
>   where
>     show (ZZFrac m (NonNegative k)) =
>       let d = toBase (Proxy :: Proxy d)
>       in case k of
>         0 -> show m
>         1 -> concat [ show m, "/", show d ]
>         _ -> concat [ show m, "/", show d, "^", show k ]

> canonize
>   :: forall d
>    . ( IsBase d )
>   => ZZFrac d -> ZZFrac d
> canonize (ZZFrac m (NonNegative k)) =
>   let d = fromIntegral $ toBase (Proxy :: Proxy d)
>   in if m == 0
>     then ZZFrac 0 (NonNegative 0)
>     else let (m',k') = reducePower d (m,k)
>       in ZZFrac m' (NonNegative k')

> reducePower
>   :: Integer -> (Integer, Integer) -> (Integer, Integer)
> reducePower d (m,k) =
>   if (d <= 0) || (k < 0)
>     then error "reducePower: panic"
>     else if k == 0
>       then (m, 0)
>       else let (q,r) = quotRem m d
>         in if r /= 0
>           then (m, k)
>           else reducePower d (q,k-1)

> complexity
>   :: ZZFrac d -> Integer
> complexity (ZZFrac _ (NonNegative k)) = k

> zzfrac
>   :: ( IsBase d )
>   => Proxy d -> Integer -> Integer -> ZZFrac d
> zzfrac _ m k =
>   if k < 0
>     then error "zzfrac: exponent must be nonnegative"
>     else canonize $ ZZFrac m (NonNegative k)

::: doctest

> -- $
> -- >>> :{
> -- show $ zzfrac nat4 5 3
> -- :}
> -- "5/4^3"
> --
> -- >>> :{
> -- show $ zzfrac nat2 18 2
> -- :}
> -- "9/2"
> --
> -- >>> :{
> -- show $ zzfrac nat3 18 1
> -- :}
> -- "6"

:::




> instance (IsBase d) => Ord (ZZFrac d) where
>   compare u1 u2 =
>     let
>       d = fromIntegral $ toBase (Proxy :: Proxy d)
>       ZZFrac m1 (NonNegative k1) = u1
>       ZZFrac m2 (NonNegative k2) = u2
>     in compare (m1 * d^k2) (m2 * d^k1)

> chooseBetween
>   :: forall d
>    . ( IsBase d )
>   => ZZFrac d -> ZZFrac d -> ZZFrac d
> chooseBetween u1 u2 =
>   case compare u1 u2 of
>     EQ -> error "chooseBetween: panic"
>     GT -> chooseBetween u2 u1
>     LT ->
>       let
>         d = fromIntegral $ toBase (Proxy :: Proxy d)
>         ZZFrac m1 (NonNegative k1) = u1
>         ZZFrac m2 (NonNegative k2) = u2
>       in canonize $ case compare k1 k2 of
>         EQ -> if m2 - m1 > 1
>           then ZZFrac (m1 + 1) (NonNegative k1)
>           else ZZFrac (d*m1 + 1) (NonNegative (k1 + 1))
>         LT -> if m2 - d^(k2-k1)*m1 > 1
>           then ZZFrac (d^(k2-k1)*m1 + 1) (NonNegative k2)
>           else ZZFrac (d^(k2-k1+1)*m1 + 1) (NonNegative (k2 + 1))
>         GT -> if d^(k1-k2)*m2 - m1 > 1
>           then ZZFrac (m1 + 1) (NonNegative k1)
>           else ZZFrac (d*m1 + 1) (NonNegative (k1 + 1))



> data Rune d a
>   = Rune a (ZZFrac d)
>   deriving (Eq, Ord, Show)

> instance
>   ( IsBase d
>   ) => Num (ZZFrac d)
>   where
>     fromInteger m = ZZFrac m (NonNegative 0)
>     abs (ZZFrac m k) = ZZFrac (abs m) k
>     signum (ZZFrac m _) = ZZFrac (signum m) (NonNegative 0)
>     negate (ZZFrac m k) = ZZFrac (negate m) k
> 
>     u1 + u2 =
>       let
>         d = fromIntegral $ toBase (Proxy :: Proxy d)
>         ZZFrac m1 (NonNegative k1) = u1
>         ZZFrac m2 (NonNegative k2) = u2
>       in case compare k1 k2 of
>         EQ -> canonize $ ZZFrac (m1 + m2) (NonNegative k1)
>         LT -> ZZFrac ((d^(k2-k1)) * m1 + m2) (NonNegative k2)
>         GT -> ZZFrac (m1 + (d^(k1-k2)) * m2) (NonNegative k1)
> 
>     u1 * u2 =
>       let
>         ZZFrac m1 (NonNegative k1) = u1
>         ZZFrac m2 (NonNegative k2) = u2
>       in canonize $ ZZFrac (m1 * m2) (NonNegative $ k1 + k2)






> instance
>   ( IsBase d
>   ) => Arb (ZZFrac d)
>   where
>     arb = fmap canonize $ ZZFrac <$> arb <*> arb

> instance
>   ( IsBase d
>   ) => Prune (ZZFrac d)
>   where
>     prune (ZZFrac m k) =
>       [ canonize $ ZZFrac m' k | m' <- prune m ] ++
>       [ canonize $ ZZFrac m k' | k' <- prune k ]

> instance
>   ( IsBase d, Arb a
>   ) => Arb (Rune d a)
>   where
>     arb = Rune <$> arb <*> arb
> 
> instance
>   ( IsBase d, Prune a
>   ) => Prune (Rune d a)
>   where
>     prune (Rune a u) =
>       [ Rune a' u | a' <- prune a ] ++
>       [ Rune a u' | u' <- prune u ]



