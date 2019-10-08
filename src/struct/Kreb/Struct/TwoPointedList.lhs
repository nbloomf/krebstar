> {-# LANGUAGE
>     MultiParamTypeClasses
>   , FlexibleInstances
>   , ScopedTypeVariables
> #-}

List with two distinguished positions, the _point_ and the _mark_.

> module Kreb.Struct.TwoPointedList where


> import qualified Data.Foldable as Fold
> import Data.List (unwords)
> 
> import Kreb.Check ( Arb(..), Prune(..), CoArb(..), pickFrom5 )
> import Kreb.Struct.FingerTree


> data TwoPointedList m a
>   = Vacant
>   | PointOnly
>       (FingerTree m a, a, FingerTree m a)
>   | Coincide
>       (FingerTree m a, a, FingerTree m a)
>   | PointMark
>       (FingerTree m a, a, FingerTree m a, a, FingerTree m a)
>   | MarkPoint
>       (FingerTree m a, a, FingerTree m a, a, FingerTree m a)
>   deriving (Eq, Show)

> toList
>   :: ( Valued m a )
>   => TwoPointedList m a -> [a]
> toList w = case w of
>   Vacant -> []
>   PointOnly (as, x, bs) -> concat
>     [ Fold.toList as, [x], Fold.toList bs ]
>   Coincide (as, x, bs) -> concat
>     [ Fold.toList as, [x], Fold.toList bs ]
>   PointMark (as, x, bs, y, cs) -> concat
>     [ Fold.toList as, [x], Fold.toList bs, [y], Fold.toList cs ]
>   MarkPoint (as, x, bs, y, cs) -> concat
>     [ Fold.toList as, [x], Fold.toList bs, [y], Fold.toList cs ]

> instance
>   ( Arb a, Valued m a
>   ) => Arb (TwoPointedList m a)
>   where
>     arb = pickFrom5
>       ( pure Vacant
>       , PointOnly <$> arb
>       , Coincide <$> arb
>       , PointMark <$> arb
>       , MarkPoint <$> arb
>       )
> 
> instance
>   ( Prune a, Valued m a
>   ) => Prune (TwoPointedList m a)
>   where
>     prune w = case w of
>       Vacant -> []
>       PointOnly z -> concat
>         [ [ Vacant ]
>         , PointOnly <$> prune z
>         ]
>       Coincide z -> concat
>         [ [ Vacant, PointOnly z ]
>         , Coincide <$> prune z
>         ]
>       PointMark (as, x, bs, y, cs) -> concat
>         [ [ Vacant, Coincide (as, x, bs), Coincide (bs, y, cs) ]
>         , PointMark <$> prune (as, x, bs, y, cs)
>         ]
>       MarkPoint (as, x, bs, y, cs) -> concat
>         [ [ Vacant, Coincide (as, x, bs), Coincide (bs, y, cs) ]
>         , MarkPoint <$> prune (as, x, bs, y, cs)
>         ]

> empty
>   ::TwoPointedList m a
> empty = Vacant

> singleton
>   :: ( Valued m a )
>   => a -> TwoPointedList m a
> singleton a =
>   PointOnly (mempty, a, mempty)

> makeTwoPointedList
>   :: ( Valued m a )
>   => [a] -> TwoPointedList m a
> makeTwoPointedList xs =
>   case uncons $ fromListFT xs of
>     Nothing -> Vacant
>     Just (a, as) -> PointOnly (mempty, a, as)

> makePointOnly
>   :: ( Valued m a )
>   => [a] -> a -> [a] -> TwoPointedList m a
> makePointOnly as x bs =
>   PointOnly (fromListFT as, x, fromListFT bs)

> makeCoincide
>   :: ( Valued m a )
>   => [a] -> a -> [a] -> TwoPointedList m a
> makeCoincide as x bs =
>   Coincide (fromListFT as, x, fromListFT bs)

> makePointMark
>   :: ( Valued m a )
>   => [a] -> a -> [a] -> a -> [a] -> TwoPointedList m a
> makePointMark as x bs y cs = PointMark
>   ( fromListFT as, x, fromListFT bs, y, fromListFT cs )

> makeMarkPoint
>   :: ( Valued m a )
>   => [a] -> a -> [a] -> a -> [a] -> TwoPointedList m a
> makeMarkPoint as x bs y cs = PointMark
>   ( fromListFT as, x, fromListFT bs, y, fromListFT cs )

> fmapList
>   :: forall m1 m2 a1 a2
>    . ( Valued m1 a1, Valued m2 a2 )
>   => (a1 -> a2) -> TwoPointedList m1 a1 -> TwoPointedList m2 a2
> fmapList f w = case w of
>   Vacant -> Vacant
>   PointOnly (as, x, bs) ->
>     PointOnly (fmapFT f as, f x, fmapFT f bs)
>   Coincide (as, x, bs) ->
>     Coincide (fmapFT f as, f x, fmapFT f bs)
>   PointMark (as, x, bs, y, cs) ->
>     PointMark (fmapFT f as, f x, fmapFT f bs, f y, fmapFT f cs)
>   MarkPoint (as, x, bs, y, cs) ->
>     MarkPoint (fmapFT f as, f x, fmapFT f bs, f y, fmapFT f cs)

> fmapRegion
>   :: forall m a
>    . ( Valued m a )
>   => (a -> a) -> TwoPointedList m a -> TwoPointedList m a
> fmapRegion f w = case w of
>   Vacant -> Vacant
>   PointOnly (as, x, bs) ->
>     PointOnly (as, x, bs)
>   Coincide (as, x, bs) ->
>     Coincide (as, f x, bs)
>   PointMark (as, x, bs, y, cs) ->
>     PointMark (as, f x, fmapFT f bs, y, cs)
>   MarkPoint (as, x, bs, y, cs) ->
>     MarkPoint (as, f x, fmapFT f bs, y, cs)

> integrate
>   :: ( Valued m a )
>   => TwoPointedList m a -> FingerTree m a
> integrate w = case w of
>   Vacant ->
>     mempty
>   PointOnly (as, x, bs) ->
>     as <> (cons x bs)
>   Coincide (as, x, bs) ->
>     as <> (cons x bs)
>   PointMark (as, x, bs, y, cs) ->
>     as <> (cons x bs) <> (cons y cs)
>   MarkPoint (as, x, bs, y, cs) ->
>     as <> (cons x bs) <> (cons y cs)

> instance
>   ( Valued m a
>   ) => Valued m (TwoPointedList m a)
>   where
>     value w = case w of
>       Vacant -> mempty
>       PointOnly (as, x, bs) -> mconcat
>         [value as, value x, value bs]
>       Coincide (as, x, bs) -> mconcat
>         [value as, value x, value bs]
>       PointMark (as, x, bs, y, cs) -> mconcat
>         [value as, value x, value bs, value y, value cs]
>       MarkPoint (as, x, bs, y, cs) -> mconcat
>         [value as, value x, value bs, value y, value cs]

> valueAtPoint
>   :: ( Valued m a )
>   => TwoPointedList m a -> Maybe m
> valueAtPoint w = case w of
>   Vacant ->
>     Nothing
>   PointOnly (as, x, _) ->
>     Just (value as <> value x)
>   Coincide (as, x, _) ->
>     Just (value as <> value x)
>   PointMark (as, x, _, _, _) ->
>     Just (value as <> value x)
>   MarkPoint (as, x, bs, y, _) ->
>     Just (value as <> value x <> value bs <> value y)

> valueAtMark
>   :: ( Valued m a )
>   => TwoPointedList m a -> Maybe m
> valueAtMark w = case w of
>   Vacant ->
>     Nothing
>   PointOnly (as, x, _) ->
>     Just (value as <> value x)
>   Coincide (as, x, _) ->
>     Just (value as <> value x)
>   PointMark (as, x, bs, y, _) ->
>     Just (value as <> value x <> value bs <> value y)
>   MarkPoint (as, x, _, _, _) ->
>     Just (value as <> value x)

> remeasure
>   :: ( Valued m1 a, Valued m2 a )
>   => TwoPointedList m1 a -> TwoPointedList m2 a
> remeasure w = case w of
>   Vacant -> Vacant
>   PointOnly (as, x, bs) ->
>     PointOnly (remeasureFT as, x, remeasureFT bs)
>   Coincide (as, x, bs) ->
>     Coincide (remeasureFT as, x, remeasureFT bs)
>   PointMark (as, x, bs, y, cs) ->
>     PointMark (remeasureFT as, x, remeasureFT bs, y, remeasureFT cs)
>   MarkPoint (as, x, bs, y, cs) ->
>     MarkPoint (remeasureFT as, x, remeasureFT bs, y, remeasureFT cs)

> clearMark
>   :: ( Valued m a )
>   => TwoPointedList m a -> TwoPointedList m a
> clearMark w = case w of
>   Vacant ->
>     Vacant
>   PointOnly (as, x, bs) ->
>     PointOnly (as, x, bs)
>   Coincide (as, x, bs) ->
>     PointOnly (as, x, bs)
>   PointMark (as, x, bs, y, cs) ->
>     PointOnly (as, x, bs <> (cons y cs))
>   MarkPoint (as, x, bs, y, cs) ->
>     PointOnly (as <> (cons x bs), y, cs)

> resetMark
>   :: ( Valued m a )
>   => TwoPointedList m a -> TwoPointedList m a
> resetMark w = case w of
>   Vacant ->
>     Vacant
>   PointOnly (as, x, bs) ->
>     Coincide (as, x, bs)
>   Coincide (as, x, bs) ->
>     Coincide (as, x, bs)
>   PointMark (as, x, bs, y, cs) ->
>     Coincide (as, x, bs <> (cons y cs))
>   MarkPoint (as, x, bs, y, cs) ->
>     Coincide (as <> (cons x bs), y, cs)

> movePointLeft
>   :: ( Valued m a )
>   => TwoPointedList m a -> TwoPointedList m a
> movePointLeft w = case w of
>   Vacant -> Vacant
>   PointOnly (as, x, bs) -> case unsnoc as of
>     Nothing -> PointOnly (mempty, x, bs)
>     Just (a, as') -> PointOnly (as', a, cons x bs)
>   Coincide (as, x, bs) -> case unsnoc as of
>     Nothing -> Coincide (mempty, x, bs)
>     Just (a, as') -> PointMark (as', a, mempty, x, bs)
>   PointMark (as, x, bs, y, cs) -> case unsnoc as of
>     Nothing -> PointMark (mempty, x, bs, y, cs)
>     Just (a, as') -> PointMark (as', a, cons x bs, y, cs)
>   MarkPoint (as, x, bs, y, cs) -> case unsnoc bs of
>     Nothing -> Coincide (as, x, cons y cs)
>     Just (b, bs') -> MarkPoint (as, x, bs', b, cons y cs)

> movePointRight
>   :: ( Valued m a )
>   => TwoPointedList m a -> TwoPointedList m a
> movePointRight w = case w of
>   Vacant -> Vacant
>   PointOnly (as, x, bs) -> case uncons bs of
>     Nothing -> PointOnly (as, x, mempty)
>     Just (b, bs') -> PointOnly (snoc x as, b, bs')
>   Coincide (as, x, bs) -> case uncons bs of
>     Nothing -> Coincide (as, x, mempty)
>     Just (b, bs') -> MarkPoint (as, x, mempty, b, bs')
>   PointMark (as, x, bs, y, cs) -> case uncons bs of
>     Nothing -> Coincide (snoc x as, y, cs)
>     Just (b, bs') -> PointMark (snoc x as, b, bs', y, cs)
>   MarkPoint (as, x, bs, y, cs) -> case uncons cs of
>     Nothing -> MarkPoint (as, x, bs, y, mempty)
>     Just (c, cs') -> MarkPoint (as, x, snoc y bs, c, cs')

> moveMarkLeft
>   :: ( Valued m a )
>   => TwoPointedList m a -> TwoPointedList m a
> moveMarkLeft w = case w of
>   Vacant -> Vacant
>   PointOnly z -> PointOnly z
>   Coincide (as, x, bs) -> case unsnoc as of
>     Nothing -> Coincide (mempty, x, bs)
>     Just (a, as') -> MarkPoint (as', a, mempty, x, bs)
>   PointMark (as, x, bs, y, cs) -> case unsnoc bs of
>     Nothing -> Coincide (as, x, cons y cs)
>     Just (b, bs') -> PointMark (as, x, bs', b, cons y cs)
>   MarkPoint (as, x, bs, y, cs) -> case unsnoc as of
>     Nothing -> MarkPoint (mempty, x, bs, y, cs)
>     Just (a, as') -> MarkPoint (as', a, cons x bs, y, cs)

> moveMarkRight
>   :: ( Valued m a )
>   => TwoPointedList m a -> TwoPointedList m a
> moveMarkRight w = case w of
>   Vacant -> Vacant
>   PointOnly z -> PointOnly z
>   Coincide (as, x, bs) -> case uncons bs of
>     Nothing -> Coincide (as, x, mempty)
>     Just (b, bs') -> PointMark (as, x, mempty, b, bs')
>   PointMark (as, x, bs, y, cs) -> case uncons cs of
>     Nothing -> PointMark (as, x, bs, y, mempty)
>     Just (c, cs') -> PointMark (as, x, snoc y bs, c, cs')
>   MarkPoint (as, x, bs, y, cs) -> case uncons bs of
>     Nothing -> Coincide (snoc x as, y, cs)
>     Just (b, bs') -> MarkPoint (snoc x as, b, bs', y, cs)

> showInternal
>   :: ( Show m, Show a, Valued m a )
>   => TwoPointedList m a -> String
> showInternal w = case w of
>   Vacant -> "Vacant"
>   PointOnly (as, x, bs) -> concat
>     [ "PointOnly "
>     , "( ", showInternalFT as
>     , ", ", show x
>     , ", ", showInternalFT bs
>     , " )"
>     ]
>   Coincide (as, x, bs) -> concat
>     [ "Coincide "
>     , "( ", showInternalFT as
>     , ", ", show x
>     , ", ", showInternalFT bs
>     , " )"
>     ]
>   PointMark (as, x, bs, y, cs) -> concat
>     [ "PointMark "
>     , "( ", showInternalFT as
>     , ", ", show x
>     , ", ", showInternalFT bs
>     , ", ", show y
>     , ", ", showInternalFT cs
>     , " )"
>     ]
>   MarkPoint (as, x, bs, y, cs) -> concat
>     [ "MarkPoint "
>     , "( ", showInternalFT as
>     , ", ", show x
>     , ", ", showInternalFT bs
>     , ", ", show y
>     , ", ", showInternalFT cs
>     , " )"
>     ]

> validate
>   :: ( Eq m, Valued m a )
>   => TwoPointedList m a -> Bool
> validate w = case w of
>   Vacant -> True
>   PointOnly (as, _, bs) ->
>     (validateFT as) && (validateFT bs)
>   Coincide (as, _, bs) ->
>     (validateFT as) && (validateFT bs)
>   PointMark (as, _, bs, _, cs) ->
>     (validateFT as) && (validateFT bs) && (validateFT cs)
>   MarkPoint (as, _, bs, _, cs) ->
>     (validateFT as) && (validateFT bs) && (validateFT cs)

> toListDebug
>   :: ( Valued m a )
>   => TwoPointedList m a -> [(a, m)]
> toListDebug w = case w of
>   Vacant -> []
>   PointOnly (as, x, bs) ->
>     toListDebugFT (as <> (cons x bs))
>   Coincide (as, x, bs) ->
>     toListDebugFT (as <> (cons x bs))
>   PointMark (as, x, bs, y, cs) ->
>     toListDebugFT (as <> (cons x bs) <> (cons y cs))
>   MarkPoint (as, x, bs, y, cs) ->
>     toListDebugFT (as <> (cons x bs) <> (cons y cs))

> isEmpty
>   :: TwoPointedList m a -> Bool
> isEmpty w = case w of
>   Vacant -> True
>   _ -> False

> isPointAtEnd
>   :: ( Valued m a )
>   => TwoPointedList m a -> Bool
> isPointAtEnd w = case w of
>   Vacant -> False
>   PointOnly (_, _, bs) -> case unsnoc bs of
>     Nothing -> True
>     _ -> False
>   Coincide (_, _, bs) -> case unsnoc bs of
>     Nothing -> True
>     _ -> False
>   PointMark _ -> False
>   MarkPoint (_, _, _, _, cs) -> case unsnoc cs of
>     Nothing -> True
>     _ -> False

> split
>   :: ( Valued m a )
>   => (m -> Bool)       -- point predicate
>   -> Maybe (m -> Bool) -- mark predicate
>   -> TwoPointedList m a -> Maybe (TwoPointedList m a)
> split pointP q w =
>   let xs = integrate w
>   in case splitFT pointP xs of
>     Nothing -> Nothing
>     Just (as, x, bs) -> Just $ case q of
>       Nothing -> PointOnly (as, x, bs)
>       Just markP -> case splitFT markP as of
>         Just (us, y, vs) ->
>           MarkPoint (us, y, vs, x, bs)
>         Nothing -> case splitFT markP bs of
>           Just (us, y, vs) ->
>             PointMark (as, x, us, y, vs)
>           Nothing -> PointOnly (as, x, bs)

> readPoint
>   :: TwoPointedList m a -> Maybe a
> readPoint w = case w of
>   Vacant -> Nothing
>   PointOnly (_, x, _) -> Just x
>   Coincide (_, x, _) -> Just x
>   PointMark (_, x, _, _, _) -> Just x
>   MarkPoint (_, _, _, x, _) -> Just x

> movePointToEnd
>   :: ( Valued m a )
>   => TwoPointedList m a -> TwoPointedList m a
> movePointToEnd w = case w of
>   Vacant -> Vacant
>   PointOnly (as, x, bs) -> case unsnoc bs of
>     Nothing -> PointOnly (as, x, mempty)
>     Just (b, bs') -> PointOnly (snoc x as <> bs', b, mempty)
>   Coincide (as, x, bs) -> case unsnoc bs of
>     Nothing -> Coincide (as, x, mempty)
>     Just (b, bs') -> MarkPoint (as, x, bs', b, mempty)
>   PointMark (as, x, bs, y, cs) -> case unsnoc cs of
>     Nothing -> Coincide (snoc x as <> bs, y, mempty)
>     Just (c, cs') -> MarkPoint (snoc x as <> bs, y, cs', c, mempty)
>   MarkPoint (as, x, bs, y, cs) -> case unsnoc cs of
>     Nothing -> MarkPoint (as, x, bs, y, mempty)
>     Just (c, cs') -> MarkPoint (as, x, bs <> cons y cs', c, mempty)

> movePointToStart
>   :: ( Valued m a )
>   => TwoPointedList m a -> TwoPointedList m a
> movePointToStart w = case w of
>   Vacant -> Vacant
>   PointOnly (as, x, bs) -> case uncons as of
>     Nothing -> PointOnly (mempty, x, bs)
>     Just (a, as') -> PointOnly (mempty, a, snoc x as' <> bs)
>   Coincide (as, x, bs) -> case uncons as of
>     Nothing -> Coincide (mempty, x, bs)
>     Just (a, as') -> PointMark (mempty, a, as', x, bs)
>   PointMark (as, x, bs, y, cs) -> case uncons as of
>     Nothing -> PointMark (mempty, x, bs, y, cs)
>     Just (a, as') -> PointMark (mempty, a, snoc x as' <> bs, y, cs)
>   MarkPoint (as, x, bs, y, cs) -> case uncons as of
>     Nothing -> Coincide (mempty, x, bs <> cons y cs)
>     Just (a, as') -> PointMark (mempty, a, as', x, snoc y bs <> cs)

> insertPointLeft
>   :: ( Valued m a )
>   => a -> TwoPointedList m a -> TwoPointedList m a
> insertPointLeft u w = case w of
>   Vacant ->
>     PointOnly (mempty, u, mempty)
>   PointOnly (as, x, bs) ->
>     PointOnly (snoc u as, x, bs)
>   Coincide (as, x, bs) ->
>     Coincide (snoc u as, x, bs)
>   PointMark (as, x, bs, y, cs) ->
>     PointMark (snoc u as, x, bs, y, cs)
>   MarkPoint (as, x, bs, y, cs) ->
>     MarkPoint (as, x, snoc u bs, y, cs)

> deletePointLeft
>   :: ( Valued m a )
>   => TwoPointedList m a -> TwoPointedList m a
> deletePointLeft w = case w of
>   Vacant ->
>     Vacant
>   PointOnly (as, x, bs) -> case unsnoc as of
>     Nothing -> PointOnly (mempty, x, bs)
>     Just (a, as') -> PointOnly (as', x, bs)
>   Coincide (as, x, bs) -> case unsnoc as of
>     Nothing -> Coincide (mempty, x, bs)
>     Just (a, as') -> Coincide (as', x, bs)
>   PointMark (as, x, bs, y, cs) -> case unsnoc as of
>     Nothing -> PointMark (mempty, x, bs, y, cs)
>     Just (a, as') -> PointMark (as', x, bs, y, cs)
>   MarkPoint (as, x, bs, y, cs) -> case unsnoc bs of
>     Nothing -> Coincide (as, y, cs)
>     Just (b, bs') -> MarkPoint (as, x, bs', y, cs)








