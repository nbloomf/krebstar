> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE StandaloneDeriving #-}
> {-# LANGUAGE Rank2Types #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE UndecidableInstances #-}

> module Kreb.Text.Rune (

>     complexity


>   , EventId(..)
>   , RuneId()

>   , Augmented(..)
>   , IsChar(..)

>   , Rune()
>   , getRuneValue
>   , getRuneId

>   , newRuneId
>   , newRunesId

>   , updateRuneEventId
>   , updateRunesEventId

>   , showDebugRune

>   , makeRunes
>   , makeRunesF
>   , makeRunes2
>   , makeRunes3

>   , setEventId

>   , witnessRune
> ) where

> import Data.Proxy

> import Kreb.Prop
> import Kreb.Control
> import Kreb.Reflect







> instance (IsBase d, IsChar a) => IsChar (Rune d a) where
>   toChar = toChar . getRuneValue
>   fromChar a = newRuneId (EventId 0 "t") (Infimum, Supremum) (fromChar a)



> numerator :: ZZFrac d -> Integer
> numerator (ZZFrac k _) = k





> complexity
>   :: ZZFrac d -> Integer
> complexity (ZZFrac _ (NonNegative k)) = k






> data RuneId d
>   = RuneId (ZZFrac d) Char EventId
>   deriving Eq

> showDebugRuneId
>   :: ( IsBase d )
>   => RuneId d -> String
> showDebugRuneId (RuneId f c eId) =
>   concat [ show f, show c, show eId ]

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

> newRunesId
>   :: ( IsBase d, IsChar a )
>   => EventId
>   -> (Augmented (RuneId d), Augmented (RuneId d))
>   -> [a] -> [Rune d a]
> newRunesId eId (u,v) as = case (u,v) of
>   (Infimum, Supremum) ->
>     zipWith (\a k -> Rune a (RuneId (fromIntegral k) (toChar a) eId)) as [0..]
>   (Infimum, Augmented (RuneId y _ _)) ->
>     let m = numerator $ intBelow y in
>     reverse $ zipWith (\a k -> Rune a (RuneId (fromIntegral k) (toChar a) eId)) as [m,(m - 1)..]
>   (Augmented (RuneId x _ _), Supremum) ->
>     let m = numerator $ intAbove x in
>     zipWith (\a k -> Rune a (RuneId (fromIntegral k) (toChar a) eId)) as [m..]
>   (Augmented (RuneId x _ _), Augmented (RuneId y _ _)) ->
>     map (\(a,k) -> Rune a (RuneId k (toChar a) eId)) (chooseBetweens x y as) 

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

> class ( Functor f ) => MakeRune f where
>   makeRuneF
>     :: forall a d
>      . ( IsBase d, IsChar a )
>     => EventId -> f a -> (EventId, f (Rune d a))


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

> deriving instance (IsBase d, Show a) => Show (Rune d a)

> showDebugRune
>   :: ( IsBase d, IsChar a )
>   => Rune d a -> String
> showDebugRune (Rune a rId) =
>   [toChar a] ++ " @ " ++ showDebugRuneId rId

> instance
>   ( IsChar a
>   ) => Eq (Rune d a)
>   where
>     (Rune a1 d1) == (Rune a2 d2) =
>       (toChar a1 == toChar a2) && (d1 == d2)
> 
> instance
>   ( IsChar a, IsBase d
>   ) => Ord (Rune d a)
>   where
>     compare (Rune a1 d1) (Rune a2 d2) =
>       compare (toChar a1, d1) (toChar a2, d2)



> instance Functor (Rune d) where
>   fmap f (Rune x y) = Rune (f x) y

> getRuneValue
>   :: Rune d a -> a
> getRuneValue (Rune v _) = v
> 
> getRuneId
>   :: Rune d a -> RuneId d
> getRuneId (Rune _ m) = m



Finds the largest integer strictly less than.



> isWhole
>   :: ZZFrac d -> Bool
> isWhole (ZZFrac _ (NonNegative k)) = k == 0





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

