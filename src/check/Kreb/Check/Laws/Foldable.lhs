> {-# LANGUAGE
>     ScopedTypeVariables
>   , RankNTypes
> #-}

> module Kreb.Check.Laws.Foldable where
> 
> import Data.Proxy
> import Data.Foldable
> 
> import Test.Tasty
> 
> import Kreb.Check.Arb
> import Kreb.Check.Tasty
> import Kreb.Check.Alg
> import Kreb.Check.Check
> import Kreb.Check.Fun

> test_Foldable_laws
>   :: forall a b m t
>    . ( Eq m, Monoid m, Eq b, Show b, MakeTo b, CoArb b, Arb b, Prune b
>      , Show m, MakeTo a, CoArb a, Arb m, Prune a, Prune m, Eq a, Show a
>      , Foldable t, Show (t a), Arb (t a), Prune (t a), Show (t m)
>      , Arb (t m), Prune (t m) )
>   => Proxy t -> Proxy a -> Proxy b -> Proxy m
>   -> TestTree
> test_Foldable_laws _ _ _ _ =
>   test_Foldable_laws_with
>     (fold :: t m -> m)
>     (foldMap :: forall u n. (Monoid n) => (u -> n) -> t u -> n)
>     (foldr :: (a -> b -> b) -> b -> t a -> b)

For types where we can't give a bona fide class instance, but still want to check properties

> test_Foldable_laws_with
>   :: forall a b m t
>    . ( Eq m, Monoid m, Eq b, Show b, MakeTo b, CoArb b, Arb b, Prune b
>      , Show m, MakeTo a, CoArb a, Arb m, Prune a, Prune m, Eq a, Show a
>      , Show (t a), Arb (t a), Prune (t a), Show (t m), Arb (t m), Prune (t m) )
>   => (t m -> m)
>   -> (forall u n. (Monoid n) => (u -> n) -> t u -> n)
>   -> ((a -> b -> b) -> b -> t a -> b)
>   -> TestTree
> test_Foldable_laws_with fold foldMap foldr =
>   testGroup "Foldable laws"
>     [ testKreb "fold == foldMap id" $
>         \(x :: t m) ->
>           check_prop_foldable_foldMap_id fold foldMap x
> 
>     , testKreb "foldr f z t = appEndo (foldMap (Endo . f) t) z" $
>         \(f :: Fun (a,b) b) z (t :: t a) ->
>           check_prop_foldable_foldr_foldMap foldr foldMap (apFun2 f) z t
>     ]

> test_FoldableFunctor_laws
>   :: forall a b m t
>    . ( Eq m, Show b, MakeTo b, CoArb b, Prune b, Arb b, Monoid m, Show m, MakeTo a, CoArb a, Arb m, Prune a, Prune m, Eq a, Show a, Foldable t, Functor t, Show (t a), Arb (t a), Prune (t a), Show (t m), Arb (t m), Prune (t m) )
>   => Proxy t -> Proxy a -> Proxy b -> Proxy m
>   -> TestTree
> test_FoldableFunctor_laws _ _ _ _ =
>   test_FoldableFunctor_laws_with
>     (fmap :: (a -> b) -> t a -> t b)
>     (fmap :: (a -> m) -> t a -> t m)
>     (fold :: t m -> m)
>     (foldMap :: forall u n. (Monoid n) => (u -> n) -> t u -> n)

> test_FoldableFunctor_laws_with
>   :: forall a b m t
>    . ( Eq m, Show b, MakeTo b, CoArb b, Prune b, Arb b, Monoid m, Show m, MakeTo a, CoArb a, Arb m, Prune a, Prune m, Eq a, Show a, Show (t a), Arb (t a), Prune (t a), Show (t m), Arb (t m), Prune (t m) )
>   => ((a -> b) -> t a -> t b)
>   -> ((a -> m) -> t a -> t m)
>   -> (t m -> m)
>   -> (forall u n. (Monoid n) => (u -> n) -> t u -> n)
>   -> TestTree
> test_FoldableFunctor_laws_with fmap1 fmap2 fold foldMap =
>   testGroup "Foldable laws"
>     [ testKreb "foldMap f == fold . fmap f" $
>         \(f :: Fun a m) (x :: t a) ->
>           check_prop_foldable_fold_fmap
>             foldMap fold fmap2 (apFun f) x
> 
>     , testKreb "foldMap f . fmap g == foldMap (f . g)" $
>         \(g :: Fun b m) (f :: Fun a b) (x :: t a) ->
>           check_prop_foldable_foldMap_fmap
>             foldMap fmap1 foldMap (apFun g) (apFun f) x
>     ]
