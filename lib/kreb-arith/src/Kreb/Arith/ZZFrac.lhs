> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE InstanceSigs #-}
> {-# LANGUAGE Rank2Types #-}
> {-# LANGUAGE GADTs #-}

> module Kreb.Arith.ZZFrac (
>     ZZFrac(..)
>   , intBelow
>   , intAbove
> ) where

> import Data.Proxy

> import qualified Kreb.Format as Fmt
> import           Kreb.Format (string, char, (<+>), display)

> import Kreb.Prop
> import Kreb.Arith.Class

> -- m / d^k
> data ZZFrac d where
>   ZZFrac
>     :: (IsBase d)
>     => Integer -> Integer -> ZZFrac d

> instance Fmt.Display (ZZFrac d)
>   where
>     display (ZZFrac m k) =
>       let d :: Integer = fromIntegral $ toBase (Proxy :: Proxy d)
>       in case compare k 0 of
>         EQ -> display m
>         LT -> display (m * d^(abs k))
>         GT -> display m <+> char '/' <+> display d <+> char '^' <+> display k

> instance (IsBase d) => Canonical (ZZFrac d) where
>   canonize
>     :: forall d
>      . ( IsBase d )
>     => ZZFrac d -> ZZFrac d
>   canonize (ZZFrac m k) =
>     let d = fromIntegral $ toBase (Proxy :: Proxy d)
>     in if m == 0
>       then ZZFrac 0 0
>       else if k <= 0
>         then ZZFrac (m * d ^ (abs k)) 0
>         else let (m',k') = reducePower d (m,k)
>           in ZZFrac m' k'
>     where
>       reducePower
>         :: Integer -> (Integer, Integer) -> (Integer, Integer)
>       reducePower d (m,k) =
>         if (d <= 0) || (k < 0)
>           then error "reducePower: panic"
>           else if k == 0
>             then (m, 0)
>             else let (q,r) = quotRem m d
>               in if r /= 0
>                 then (m, k)
>                 else reducePower d (q,k-1)

> instance (IsBase d) => Eq (ZZFrac d) where
>   u1 == u2 =
>     let
>       ZZFrac m1 k1 = canonize u1
>       ZZFrac m2 k2 = canonize u2
>     in (m1, k1) == (m2, k2)

> instance (IsBase d) => Ord (ZZFrac d) where
>   compare u1 u2 =
>     let
>       d = fromIntegral $ toBase (Proxy :: Proxy d)
>       ZZFrac m1 k1 = canonize u1
>       ZZFrac m2 k2 = canonize u2
>     in compare (m1 * d^k2) (m2 * d^k1)

> instance (IsBase d) => Show (ZZFrac d) where
>   show u =
>     let
>       ZZFrac m k = canonize u
>       d = showBase (Proxy :: Proxy d)
>     in case k of
>       0 -> show m
>       1 -> concat [ show m, "/", d ]
>       _ -> concat [ show m, "/", d, "^", show k ]



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



> instance (IsBase d) => Dense (ZZFrac d) where
>   chooseBetween
>     :: forall d
>      . ( IsBase d )
>     => ZZFrac d -> ZZFrac d -> ZZFrac d
>   chooseBetween u1 u2 =
>     let
>       v1 = canonize u1
>       v2 = canonize u2
>     in case compare v1 v2 of
>       EQ -> error "chooseBetween: panic"
>       GT -> chooseBetween v2 v1
>       LT ->
>         let
>           d = fromIntegral $ toBase (Proxy :: Proxy d)
>           ZZFrac m1 k1 = v1
>           ZZFrac m2 k2 = v2
>         in canonize $ case compare k1 k2 of
>           EQ -> if m2 - m1 > 1
>             then ZZFrac (m1 + 1) k1
>             else ZZFrac (d*m1 + 1) (k1 + 1)
>           LT -> if m2 - d^(k2-k1)*m1 > 1
>             then ZZFrac (d^(k2-k1)*m1 + 1) k2
>             else ZZFrac (d^(k2-k1+1)*m1 + 1) (k2 + 1)
>           GT -> if d^(k1-k2)*m2 - m1 > 1
>             then ZZFrac (m1 + 1) k1
>             else ZZFrac (d*m1 + 1) (k1 + 1)



> instance (IsBase d) => Num (ZZFrac d) where
>   fromInteger m = ZZFrac m 0
>   abs (ZZFrac m k) = ZZFrac (abs m) k
>   signum (ZZFrac m _) = ZZFrac (signum m) 0
>   negate (ZZFrac m k) = ZZFrac (negate m) k
> 
>   u1 + u2 =
>     let
>       d = fromIntegral $ toBase (Proxy :: Proxy d)
>       ZZFrac m1 k1 = canonize u1
>       ZZFrac m2 k2 = canonize u2
>     in case compare k1 k2 of
>       EQ -> canonize $ ZZFrac (m1 + m2) k1
>       LT -> ZZFrac ((d^(k2-k1)) * m1 + m2) k2
>       GT -> ZZFrac (m1 + (d^(k1-k2)) * m2) k1
> 
>   u1 * u2 =
>     let
>       ZZFrac m1 k1 = canonize u1
>       ZZFrac m2 k2 = canonize u2
>     in canonize $ ZZFrac (m1 * m2) (k1 + k2)



> instance (IsBase d) => Arb (ZZFrac d) where
>   arb = fmap canonize $ ZZFrac <$> arb <*> arb

> instance (IsBase d) => Prune (ZZFrac d) where
>   prune (ZZFrac m k) =
>     [ canonize $ ZZFrac m' k | m' <- prune m ] ++
>     [ canonize $ ZZFrac m k' | k' <- prune k ]



> instance WitnessBase ZZFrac where
>   witnessBase
>     :: forall d1 d2
>      . ( IsBase d1, IsBase d2 )
>     => Proxy d2 -> ZZFrac d1 -> ZZFrac d2
>   witnessBase p (ZZFrac u v) =
>     if (toBase p) == (toBase (Proxy :: Proxy d1))
>       then ZZFrac u v
>       else error "panic: WitnessBase implementation for ZZFrac"



> intBelow
>   :: forall d
>    . ( IsBase d )
>   => ZZFrac d -> ZZFrac d
> intBelow (ZZFrac m k) =
>   let d = fromIntegral $ toBase (Proxy :: Proxy d)
>   in if k < 0
>     then ZZFrac (m * d^k - 1) 0
>     else let
>       d = fromIntegral $ toBase (Proxy :: Proxy d)
>       (q,r) = quotRem m (d ^ k)
>     in if r == 0
>       then ZZFrac (q-1) 0
>       else ZZFrac q 0

> intAbove
>   :: forall d
>    . ( IsBase d )
>   => ZZFrac d -> ZZFrac d
> intAbove (ZZFrac m k) =
>   let d = fromIntegral $ toBase (Proxy :: Proxy d)
>   in if k < 0
>     then ZZFrac (m * d^k + 1) 0
>     else let
>       d = fromIntegral $ toBase (Proxy :: Proxy d)
>       q = quot m (d ^ k)
>     in ZZFrac (q+1) 0


