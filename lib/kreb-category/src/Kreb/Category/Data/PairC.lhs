> {-# LANGUAGE OverloadedStrings #-}

> module Kreb.Category.Data.PairC (
>     PairC(..)
> ) where

> import           Kreb.Control
> import qualified Kreb.Format as Fmt
> import           Kreb.Format ((<+>), display)
> import           Kreb.Prop

> import Kreb.Category.Class

> data PairC (obj1 :: Obj) (obj2 :: Obj) a b where
>   PairC
>     :: ( obj1 a, obj2 b )
>     => a -> b -> PairC obj1 obj2 a b

> instance
>   ( Eq a, Eq b
>   ) => Eq (PairC obj1 obj2 a b)
>   where
>     (PairC a1 b1) == (PairC a2 b2) =
>       (a1 == a2) && (b1 == b2)

> instance
>   ( EqIn ctxa a, EqIn ctxb b
>   ) => EqIn (ctxa, ctxb) (PairC obj1 obj2 a b)
>   where
>     eqIn (ctxa, ctxb) (PairC a1 b1) (PairC a2 b2) =
>       (eqIn ctxa a1 a2) && (eqIn ctxb b1 b2)

> instance
>   ( Fmt.Display a, Fmt.Display b
>   ) => Fmt.Display (PairC obj1 obj2 a b)
>   where
>     display (PairC a b) =
>       "PairC" <+> display a <+> display b
> 
> instance
>   ( Arb a, obj1 a, Arb b, obj2 b
>   ) => Arb (PairC obj1 obj2 a b)
>   where
>     arb = PairC <$> arb <*> arb
> 
> instance
>   ( Prune a, obj1 a, Prune b, obj2 b
>   ) => Prune (PairC obj1 obj2 a b)
>   where
>     prune (PairC a b) =
>       [ PairC a' b | a' <- prune a ] ++
>       [ PairC a b' | b' <- prune b ]



Bifunctor Instances
-------------------

> instance
>   ( forall x y
>        . ( obj1 x, obj2 y )
>       => obj3 (PairC obj1 obj2 x y)
>   ) => BifunctorC
>     (obj1 :: Obj) (MapOn obj1)
>     (obj2 :: Obj) (MapOn obj2)
>     (obj3 :: Obj) (MapOn obj3)
>     (PairC obj1 obj2)
>   where
>     bimapC (Map f) (Map g) =
>       let h (PairC a b) = PairC (f a) (g b)
>       in Map h

> instance
>   ( Monad m
>   , forall x y
>        . ( AlgebraC obj m x, AlgebraC obj m y )
>       => AlgebraC obj m (PairC (AlgebraC obj m) (AlgebraC obj m) x y)
>   ) => BifunctorC
>     (AlgebraC obj m) (MapAlgOn obj m)
>     (AlgebraC obj m) (MapAlgOn obj m)
>     (AlgebraC obj m) (MapAlgOn obj m)
>     (PairC (AlgebraC obj m) (AlgebraC obj m))
>   where
>     bimapC (MapAlg f) (MapAlg g) =
>       MapAlg $ \(PairC a b) -> PairC (f a) (g b)

> instance
>   ( Monad m
>   , forall x y
>        . ( obj x, obj y )
>       => obj (PairC obj obj x y)
>   ) => BifunctorC
>     (obj :: Obj) (MapOn obj)
>     (obj :: Obj) (KleisliOn obj m)
>     (obj :: Obj) (KleisliOn obj m)
>     (PairC obj obj)
>   where
>     bimapC (Map f) (Kleisli g) =
>       Kleisli $ \(PairC a b) -> do
>         b' <- g b
>         return $ PairC (f a) b'

> instance
>   ( Monad m
>   , forall x y
>        . ( AlgebraC obj m x, obj y )
>       => obj (PairC (AlgebraC obj m) obj x y)
>   ) => BifunctorC
>     (AlgebraC obj m) (MapAlgOn obj m)
>     (obj :: Obj) (MapOn obj)
>     (obj :: Obj) (MapOn obj)
>     (PairC (AlgebraC obj m) obj)
>   where
>     bimapC (MapAlg f) (Map g) =
>       Map $ \(PairC x y) -> PairC (f x) (g y)



Monoidal Category Instances

> instance
>   ( forall x y. (obj x, obj y) => obj (PairC obj obj x y)
>   ) => MonoidalCategoryC (obj :: Obj) (MapOn obj) (PairC obj obj) ()
>   where
>     assocL = Map (\(PairC a (PairC b c)) -> PairC (PairC a b) c)
>     assocR = Map (\(PairC (PairC a b) c) -> PairC a (PairC b c))
>     unitL  = Map (\(PairC x ()) -> x)
>     unitL' = Map (\x -> PairC x ())
>     unitR  = Map (\(PairC () x) -> x)
>     unitR' = Map (\x -> PairC () x)

> instance
>   ( Monad m
>   , forall x y
>        . ( AlgebraC obj m x, AlgebraC obj m y)
>       => AlgebraC obj m (PairC (AlgebraC obj m) (AlgebraC obj m) x y)
>   ) => MonoidalCategoryC
>     (AlgebraC obj m) (MapAlgOn obj m)
>     (PairC (AlgebraC obj m) (AlgebraC obj m)) ()
>   where
>     assocL = MapAlg (\(PairC a (PairC b c)) -> PairC (PairC a b) c)
>     assocR = MapAlg (\(PairC (PairC a b) c) -> PairC a (PairC b c))
>     unitL  = MapAlg (\(PairC x ()) -> x)
>     unitL' = MapAlg (\x -> PairC x ())
>     unitR  = MapAlg (\(PairC () x) -> x)
>     unitR' = MapAlg (\x -> PairC () x)



Monoidal Action Instances

> instance
>   ( forall x y. (obj x, obj y) => obj (PairC obj obj x y)
>   ) => MonoidalActionC
>     (obj :: Obj) (MapOn obj) (PairC obj obj) ()
>     (obj :: Obj) (MapOn obj)
>     (PairC obj obj)
>   where
>     unitor  = Map $ \(PairC () x) -> x
>     unitor' = Map $ \x -> PairC () x
>     compor  = Map $ \(PairC p (PairC q x)) -> PairC (PairC p q) x
>     compor' = Map $ \(PairC (PairC p q) x) -> PairC p (PairC q x)

> instance
>   ( Monad m
>   , forall x y. (obj x, obj y) => obj (PairC obj obj x y)
>   ) => MonoidalActionC
>     (obj :: Obj) (MapOn obj) (PairC obj obj) ()
>     (obj :: Obj) (KleisliOn obj m)
>     (PairC obj obj)
>   where
>     unitor  = Kleisli $ \(PairC () x) -> return x
>     unitor' = Kleisli $ \x -> return (PairC () x)
>     compor  = Kleisli $ \(PairC p (PairC q x)) ->
>       return (PairC (PairC p q) x)
>     compor' = Kleisli $ \(PairC (PairC p q) x) ->
>       return (PairC p (PairC q x))

> instance
>   ( Monad m
>   , forall x y
>        . ( AlgebraC obj m x, AlgebraC obj m y )
>       => AlgebraC obj m (PairC (AlgebraC obj m) (AlgebraC obj m) x y)
>   , forall x y
>        . ( AlgebraC obj m x, obj y )
>       => obj (PairC (AlgebraC obj m) obj x y)
>   ) => MonoidalActionC
>     (AlgebraC obj m) (MapAlgOn obj m) (PairC (AlgebraC obj m) (AlgebraC obj m)) ()
>     (obj :: Obj) (MapOn obj)
>     (PairC (AlgebraC obj m) obj)
>   where
>     unitor  = Map $ \(PairC () x) -> x
>     unitor' = Map (\x -> PairC () x)
>     compor  = Map (\(PairC p (PairC q x)) -> PairC (PairC p q) x)
>     compor' = Map (\(PairC (PairC p q) x) -> PairC p (PairC q x))
