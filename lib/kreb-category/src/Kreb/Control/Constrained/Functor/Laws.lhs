> module Kreb.Control.Constrained.Functor.Laws where
> 
> import Data.Proxy
> import Data.Foldable
> 
> import Test.Tasty
> 
> import qualified Kreb.Format as Fmt
> import Kreb.Prop

> import Kreb.Control.Constrained.Functor

> test_ConstrainedFunctor_laws
>   :: forall a b c t
>    . ( ConstrainedFunctor t
>      , Fmt.Display a, Arb a, Prune a, CoArb a, MakeTo a
>      , Fmt.Display b, Arb b, Prune b, CoArb b, MakeTo b
>      , Fmt.Display c, Arb c, Prune c
>      , Eq (t a), Fmt.Display (t a), Arb (t a), Prune (t a)
>      , Eq (t c)
>      , FunctorConstraint t a, FunctorConstraint t b, FunctorConstraint t c )
>   => Proxy t -> Proxy a -> Proxy b -> Proxy c
>   -> TestTree
> test_ConstrainedFunctor_laws pt pa pb pc =
>   testGroup "ConstrainedFunctor laws"
>     [ krebProp "identity" $
>         (check_prop_fmap_id fmapC :: t a -> Check)
> 
>     , krebProp "composite" $
>         \(g :: Fun b c) (f :: Fun a b) x ->
>           check_prop_fmap_comp
>             (fmapC :: (b -> c) -> t b -> t c)
>             fmapC fmapC (apFun g) (apFun f) x
>     ]

> test_FoldableConstrainedFunctor_laws
>   :: forall a b m t
>    . ( ConstrainedFunctor t, Foldable t
>      , Eq a, Fmt.Display a, Arb a, Prune a, CoArb a, MakeTo a
>      , Fmt.Display b, Arb b, Prune b, CoArb b, MakeTo b
>      , Eq m, Fmt.Display m, Arb m, Prune m, Monoid m
>      , Fmt.Display (t a), Arb (t a), Prune (t a)
>      , Fmt.Display (t m), Arb (t m), Prune (t m)
>      , FunctorConstraint t a, FunctorConstraint t b, FunctorConstraint t m )
>   => Proxy t -> Proxy a -> Proxy b -> Proxy m
>   -> TestTree
> test_FoldableConstrainedFunctor_laws _ _ _ _ =
>   testGroup "Foldable laws"
>     [ krebProp "foldMap f == fold . fmap f" $
>         \(f :: Fun a m) (x :: t a) ->
>           check_prop_foldable_fold_fmap
>             foldMap fold fmapC (apFun f) x
> 
>     , krebProp "foldMap f . fmap g == foldMap (f . g)" $
>         \(g :: Fun b m) (f :: Fun a b) (x :: t a) ->
>           check_prop_foldable_foldMap_fmap
>             foldMap fmapC foldMap (apFun g) (apFun f) x
>     ]
