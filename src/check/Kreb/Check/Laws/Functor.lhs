> {-# LANGUAGE
>     ScopedTypeVariables
>   , RankNTypes
> #-}

> module Kreb.Check.Laws.Functor where
> 
> import Data.Proxy
> 
> import Test.Tasty
> 
> import Kreb.Check.Arb
> import Kreb.Check.Tasty
> import Kreb.Check.Alg
> import Kreb.Check.Check
> import Kreb.Check.Fun

> test_Functor_laws
>   :: forall a b c t
>    . ( Eq (t a), Eq (t c)
>      , MakeTo a, MakeTo b, CoArb b, CoArb a
>      , Show b, Arb c, Arb b, Arb (t a), Prune b
>      , Prune c, Prune (t a), Show c, Show (t a)
>      , Arb a, Show a, Prune a, Functor t )
>   => Proxy t -> Proxy a -> Proxy b -> Proxy c
>   -> TestTree
> test_Functor_laws pt pa pb pc =
>   test_Functor_laws_with pt pa pb pc
>     (fmap :: forall u v. (u -> v) -> t u -> t v)

> test_Functor_laws_with
>   :: forall a b c t
>    . ( Eq (t a), Eq (t c)
>      , MakeTo a, MakeTo b, CoArb b, CoArb a
>      , Show b, Arb c, Arb b, Arb (t a), Prune b
>      , Prune c, Prune (t a), Show c, Show (t a)
>      , Arb a, Show a, Prune a )
>   => Proxy t -> Proxy a -> Proxy b -> Proxy c
>   -> (forall u v. (u -> v) -> t u -> t v)
>   -> TestTree
> test_Functor_laws_with _ _ _ _ map =
>   testGroup "Functor laws"
>     [ testKreb "identity" $
>         (check_prop_fmap_id map :: t a -> Check)
>     , testKreb "composite" $
>         \(g :: Fun b c) (f :: Fun a b) x ->
>           check_prop_fmap_comp
>             map map map (apFun g) (apFun f) x
>     ]
