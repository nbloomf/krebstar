> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE StandaloneDeriving #-}
> {-# LANGUAGE Rank2Types #-}

> module Kreb.Text.Rune (
>     ZZFrac()
>   , zzfrac
>   , IsBase(..)
>   , withBase

>   , complexity

>   , chooseBetween

>   , EventId(..)
>   , RuneId()

>   , Augmented(..)
>   , IsChar(..)

>   , Rune()
>   , getRuneValue
>   , getRuneId

>   , newRuneId

>   , updateRuneEventId
>   , updateRunesEventId

>   , makeRunes
>   , makeRunes2
>   , makeRunes3

>   , setEventId
> ) where

> import Data.Proxy

> import Kreb.Check
> import Kreb.Control
> import Kreb.Reflect


> class IsChar a where
>   toChar   :: a -> Char
>   fromChar :: Char -> a

> instance IsChar Char where
>   toChar   = id
>   fromChar = id

> instance (IsBase d, IsChar a) => IsChar (Rune d a) where
>   toChar = toChar . getRuneValue
>   fromChar a = newRuneId (EventId 0 "t") (Infimum, Supremum) (fromChar a)



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

> data EventId
>   = EventId Integer String
>   deriving (Eq, Ord, Show)

> data RuneId d
>   = RuneId (ZZFrac d) Char EventId
>   deriving Eq

> newRuneId
>   :: ( IsBase d, IsChar a )
>   => EventId
>   -> (Augmented (RuneId d), Augmented (RuneId d))
>   -> a -> Rune d a
> newRuneId eId (u,v) a = case (u,v) of
>   (Infimum, Supremum) ->
>     Rune a (RuneId 0 (toChar a) eId)
>   (Infimum, Augmented (RuneId y _ _)) ->
>     Rune a (RuneId (intBelow y) (toChar a) eId)
>   (Augmented (RuneId x _ _), Supremum) ->
>     Rune a (RuneId (intAbove x) (toChar a) eId)
>   (Augmented (RuneId x _ _), Augmented (RuneId y _ _)) ->
>     Rune a (RuneId (chooseBetween x y) (toChar a) eId)

> updateRuneEventId
>   :: forall a d
>    . ( IsBase d, IsChar a )
>   => EventId -> Rune d a -> Rune d a
> updateRuneEventId eId (Rune a (RuneId l c _)) =
>   Rune a (RuneId l c eId)

> updateRunesEventId
>   :: forall a d f
>    . ( IsBase d, IsChar a, Functor f )
>   => EventId -> f (Rune d a) -> f (Rune d a)
> updateRunesEventId eId = fmap (updateRuneEventId eId)

> makeRunes
>   :: forall a d
>    . ( IsBase d, IsChar a )
>   => EventId -> [a] -> [Rune d a]
> makeRunes eId =
>   zipWith (\i a -> Rune a (RuneId (fromInteger i) (toChar a) eId)) [0..]

> foo
>   :: (Integer -> a -> b)
>   -> Integer -> [a] -> ([b], Integer)
> foo f k as = foo' [] as k
>   where
>     foo' us vs m = case vs of
>       [] -> (reverse us, m)
>       w:ws -> foo' (f m w : us) ws (m+1)

> makeRunes2
>   :: forall a d
>    . ( IsBase d, IsChar a )
>   => EventId -> [a] -> a -> [a]
>   -> ([Rune d a], Rune d a, [Rune d a])
> makeRunes2 eId as x bs =
>   let
>     g i a = Rune a (RuneId (fromInteger i) (toChar a) eId)
>     (us,k) = foo g 0 as
>     w = g k x
>     (vs,_) = foo g (k+1) bs
>   in (us, w, vs)

> makeRunes3
>   :: forall a d
>    . ( IsBase d, IsChar a )
>   => EventId -> [a] -> a -> [a] -> a -> [a]
>   -> ([Rune d a], Rune d a, [Rune d a], Rune d a, [Rune d a])
> makeRunes3 eId as x bs y cs =
>   let
>     g i a = Rune a (RuneId (fromInteger i) (toChar a) eId)
>     (us,k) = foo g 0 as
>     w = g k x
>     (vs,l) = foo g (k+1) bs
>     z = g l y
>     (hs,_) = foo g (l+1) cs
>   in (us, w, vs, z, hs)

> deriving instance ( IsBase d ) => Ord (RuneId d)
> deriving instance ( IsBase d ) => Show (RuneId d)

> data Rune d a
>   = Rune a (RuneId d)
>   deriving (Eq, Ord, Show)

> setEventId
>   :: EventId -> Rune d a -> Rune d a
> setEventId eId (Rune a (RuneId x c _)) =
>   Rune a (RuneId x c eId)

> instance Functor (Rune d) where
>   fmap f (Rune x y) = Rune (f x) y

> getRuneValue
>   :: Rune d a -> a
> getRuneValue (Rune v _) = v
> 
> getRuneId
>   :: Rune d a -> RuneId d
> getRuneId (Rune _ m) = m

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

Finds the largest integer strictly less than.

> intBelow
>   :: forall d
>    . ( IsBase d )
>   => ZZFrac d -> ZZFrac d
> intBelow (ZZFrac m (NonNegative k)) =
>   let
>     d = fromIntegral $ toBase (Proxy :: Proxy d)
>     (q,r) = quotRem m (d ^ k)
>   in if r == 0
>     then ZZFrac (q-1) (NonNegative 0)
>     else ZZFrac q (NonNegative 0)

> intAbove
>   :: forall d
>    . ( IsBase d )
>   => ZZFrac d -> ZZFrac d
> intAbove (ZZFrac m (NonNegative k)) =
>   let
>     d = fromIntegral $ toBase (Proxy :: Proxy d)
>     q = quot m (d ^ k)
>   in ZZFrac (q+1) (NonNegative 0)

> isWhole
>   :: ZZFrac d -> Bool
> isWhole (ZZFrac _ (NonNegative k)) = k == 0






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

> instance Arb EventId where
>   arb = EventId <$> arb <*> arb
> 
> instance Prune EventId where
>   prune (EventId a b) =
>     [ EventId a' b | a' <- prune a ] ++
>     [ EventId a b' | b' <- prune b ]

> instance
>   ( IsBase d
>   ) => Arb (RuneId d)
>   where
>     arb = RuneId <$> arb <*> arb <*> arb

> instance
>   ( IsBase d
>   ) => Prune (RuneId d)
>   where
>     prune (RuneId u c v) =
>       [ RuneId u' c v | u' <- prune u ] ++
>       [ RuneId u c' v | c' <- prune c ] ++
>       [ RuneId u c v' | v' <- prune v ]

> instance
>   ( IsBase d, Arb a, IsChar a
>   ) => Arb (Rune d a)
>   where
>     arb = do
>       a <- arb
>       Rune <$> pure a <*> (RuneId <$> arb <*> pure (toChar a) <*> arb)
> 
> instance
>   ( IsBase d, Prune a, IsChar a
>   ) => Prune (Rune d a)
>   where
>     prune (Rune a (RuneId u _ v)) =
>       [ Rune a' (RuneId u (toChar a') v) | a' <- prune a ] ++
>       [ Rune a (RuneId u' (toChar a) v) | u' <- prune u ] ++
>       [ Rune a (RuneId u (toChar a) v') | v' <- prune v ]

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

