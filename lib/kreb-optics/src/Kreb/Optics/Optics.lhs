> module Kreb.Optics.Optics where

> import Prelude (($), Either(..), Monad(..))
> import Data.Void

> import Kreb.Category
> import Kreb.Optics.Class.Tambara



Optics for the action of the category on itself via the cartesian product.

Simple Lenses

> type LensC obj a s =
>   ProfOptic obj (MapOn obj) obj (MapOn obj) obj (MapOn obj)
>     (PairC obj obj) () (PairC obj obj) (PairC obj obj) a a s s
> 
> type Lens a s = LensC Hask a s
> 
> lensC
>   :: forall (obj :: Obj) a s
>    . ( obj a, obj s )
>   => (s -> a) -> (s -> a -> s) -> LensC obj a s
> lensC view update =
>   toProfOptic
>     @obj @(MapOn obj) @obj @(MapOn obj)
>     @obj @(MapOn obj) @(PairC obj obj) @()
>     @(PairC obj obj) @(PairC obj obj) $
>     Optic
>       (Map $ \s -> PairC s (view s))
>       (Map $ \(PairC s a) -> update s a)
> 
> lens
>   :: (s -> a) -> (s -> a -> s) -> Lens a s
> lens = lensC @Hask

Generic Lenses

> type LensC' obj a b s t =
>   ProfOptic obj (MapOn obj) obj (MapOn obj) obj (MapOn obj)
>     (PairC obj obj) () (PairC obj obj) (PairC obj obj) a b s t
> 
> type Lens' a b s t = LensC' Hask a b s t
> 
> lensC'
>   :: forall (obj :: Obj) a b s t
>    . ( obj a, obj s )
>   => (s -> a) -> (s -> b -> t) -> LensC' obj a b s t
> lensC' view update =
>   toProfOptic
>     @obj @(MapOn obj) @obj @(MapOn obj)
>     @obj @(MapOn obj) @(PairC obj obj) @()
>     @(PairC obj obj) @(PairC obj obj) $
>     Optic
>       (Map $ \s -> PairC s (view s))
>       (Map $ \(PairC s a) -> update s a)
> 
> lens'
>   :: (s -> a) -> (s -> b -> t) -> Lens' a b s t
> lens' = lensC' @Hask






Simple Prisms

> type PrismC obj a s =
>   ProfOptic obj (MapOn obj) obj (MapOn obj) obj (MapOn obj)
>     (ChoiceC obj obj) Void (ChoiceC obj obj) (ChoiceC obj obj) a a s s
> 
> type Prism a s = PrismC Hask a s
> 
> prismC
>   :: forall (obj :: Obj) a s
>    . (s -> Either s a) -> (a -> s) -> PrismC obj a s
> prismC downcast upcast =
>   toProfOptic
>     @obj @(MapOn obj) @obj @(MapOn obj)
>     @obj @(MapOn obj) @(ChoiceC obj obj) @Void
>     @(ChoiceC obj obj) @(ChoiceC obj obj) $
>     Optic
>       (Map $ \s -> case downcast s of
>         Left s' -> Choice1 s'; Right a -> Choice2 a)
>       (Map $ \x -> case x of
>         Choice1 s -> s; Choice2 a -> upcast a)
> 
> prism
>   :: (s -> Either s a) -> (a -> s) -> Prism a s
> prism = prismC @Hask

Generic Prisms

> type PrismC' obj a b s t =
>   ProfOptic obj (MapOn obj) obj (MapOn obj) obj (MapOn obj)
>     (ChoiceC obj obj) Void (ChoiceC obj obj) (ChoiceC obj obj) a b s t
> 
> type Prism' a b s t = PrismC' Hask a b s t
> 
> prismC'
>   :: forall (obj :: Obj) a b s t
>    . (s -> Either t a) -> (b -> t) -> PrismC' obj a b s t
> prismC' downcast upcast =
>   toProfOptic
>     @obj @(MapOn obj) @obj @(MapOn obj)
>     @obj @(MapOn obj) @(ChoiceC obj obj) @Void
>     @(ChoiceC obj obj) @(ChoiceC obj obj) $
>     Optic
>       (Map $ \s -> case downcast s of
>         Left t -> Choice1 t; Right a -> Choice2 a)
>       (Map $ \x -> case x of
>         Choice1 t -> t; Choice2 b -> upcast b)
> 
> prism'
>   :: (s -> Either t a) -> (b -> t) -> Prism' a b s t
> prism' = prismC' @Hask





Simple Algebraic Lenses

> type AlgLensC obj m a s = ( Monad m ) =>
>   ProfOptic obj (MapOn obj) obj (MapOn obj) (AlgebraC obj m) (MapAlgOn obj m)
>     (PairC (AlgebraC obj m) (AlgebraC obj m)) ()
>     (PairC (AlgebraC obj m) obj) (PairC (AlgebraC obj m) obj) a a s s
> 
> type AlgLens m a s = AlgLensC Hask m a s
> 
> algLensC
>   :: forall (obj :: Obj) m a s
>    . ( Monad m )
>   => (s -> a) -> (m s -> a -> s) -> AlgLensC obj m a s
> algLensC view updateM =
>   toProfOptic
>     @obj @(MapOn obj) @obj @(MapOn obj)
>     @(AlgebraC obj m) @(MapAlgOn obj m) @(PairC (AlgebraC obj m) (AlgebraC obj m)) @()
>     @(PairC (AlgebraC obj m) obj) @(PairC (AlgebraC obj m) obj) $
>     Optic
>       (Map $ \a -> PairC (return @m a) (view a))
>       (Map $ \(PairC ms a) -> updateM ms a)
> 
> algLens
>   :: ( Monad m )
>   => (s -> a) -> (m s -> a -> s) -> AlgLens m a s
> algLens = algLensC @Hask

Generic AlgebraCic Lenses

> type AlgLensC' obj m a b s t = ( Monad m ) =>
>   ProfOptic obj (MapOn obj) obj (MapOn obj) (AlgebraC obj m) (MapAlgOn obj m)
>     (PairC (AlgebraC obj m) (AlgebraC obj m)) ()
>     (PairC (AlgebraC obj m) obj) (PairC (AlgebraC obj m) obj) a b s t
> 
> type AlgLens' m a b s t = AlgLensC' Hask m a b s t
> 
> algLensC'
>   :: forall (obj :: Obj) m a b s t
>    . ( Monad m )
>   => (s -> a) -> (m s -> b -> t) -> AlgLensC' obj m a b s t
> algLensC' view updateM =
>   toProfOptic
>     @obj @(MapOn obj) @obj @(MapOn obj)
>     @(AlgebraC obj m) @(MapAlgOn obj m) @(PairC (AlgebraC obj m) (AlgebraC obj m)) @()
>     @(PairC (AlgebraC obj m) obj) @(PairC (AlgebraC obj m) obj) $
>     Optic
>       (Map $ \a -> PairC (return @m a) (view a))
>       (Map $ \(PairC ms b) -> updateM ms b)
> 
> algLens'
>   :: ( Monad m )
>   => (s -> a) -> (m s -> b -> t) -> AlgLens' m a b s t
> algLens' = algLensC' @Hask









-- | Kaleidoscopes are optics for the action by evaluation of applicative
-- functors.
type Kaleidoscope a s = ProfOptic Any (->) Any (->) Applicative Nat Compose Identity App App a a s s
mkKaleidoscope :: (([a] -> a) -> ([s] -> s)) -> Kaleidoscope a s
mkKaleidoscope f =
  ex2prof @Any @(->) @Any @(->) @Applicative @Nat @Compose @Identity @App @App
  $ Optic (App . flip More (Done id)) (uncurry (flip f) . noFun . getApp)
    where
      noFun :: FunList s a b -> ([s] , ([a] -> b))
      noFun (Done b) = ([] , const b)
      noFun (More s l) = (\(u , f) -> (s:u , \(h:t) -> f t h)) $ noFun l

-- | Monadic lenses are mixed optics for the cartesian product in the base
-- category and in the Kleisli category.
type MonadicLens m a b s t = (Monad m) => ProfOptic Any (->) Any (Kleisli m) Any (->) (,) () (,) (,) a b s t
mkMonadicLens :: forall m a b s t . (Monad m) => (s -> a) -> (s -> b -> m t) -> MonadicLens m a b s t
mkMonadicLens v u =
  ex2prof @Any @(->) @Any @(Kleisli m) @Any @(->) @(,) @() @(,) @(,)
  $ Optic (\a -> (a , v a)) (Kleisli (uncurry u))

-- | Traversals as optics for the action of traversable functors.
type Traversal a s = ProfOptic Any (->) Any (->) Traversable Nat Compose Identity App App a a s s
mkTraversal :: forall a b s t . (s -> ([a], [a] -> s)) -> Traversal a s
mkTraversal ext = mkTraversal2 (\s -> Split (fst (ext s)) s) (\(Split la s) -> snd (ext s) la)

data Split s a = Split [a] s
instance Functor (Split s) where
  fmap f (Split l s) = Split (fmap f l) s
instance Foldable (Split s) where
  foldr f z (Split l s) = foldr f z l
instance Traversable (Split s) where
  traverse f (Split l s) = fmap (flip Split s) (traverse f l)


mkTraversal2 :: forall a s f . Traversable f => (s -> f a) -> (f a -> s) -> Traversal a s
mkTraversal2 l r =
  ex2prof @Any @(->) @Any @(->) @Traversable @Nat @Compose @Identity @App @App
  $ Optic (App . l) (r . getApp)
