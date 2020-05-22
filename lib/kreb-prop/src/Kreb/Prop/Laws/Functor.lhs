> {-# LANGUAGE ScopedTypeVariables, RankNTypes #-}

> module Kreb.Prop.Laws.Functor where
> 
> import Data.Proxy
> 
> import Test.Tasty
> 
> import qualified Kreb.Format as Fmt
> 
> import Kreb.Prop.Arb
> import Kreb.Prop.Tasty
> import Kreb.Prop.Alg
> import Kreb.Prop.Check
> import Kreb.Prop.Fun

> test_Functor_laws
>   :: forall a b c t
>    . ( Eq (t a), Eq (t c)
>      , MakeTo a, MakeTo b, CoArb b, CoArb a
>      , Fmt.Display b, Arb c, Arb b, Arb (t a), Prune b
>      , Prune c, Prune (t a), Fmt.Display c, Fmt.Display (t a)
>      , Arb a, Fmt.Display a, Prune a, Functor t )
>   => Proxy t -> Proxy a -> Proxy b -> Proxy c
>   -> TestTree
> test_Functor_laws pt pa pb pc =
>   test_Functor_laws_with pt pa pb pc
>     (fmap :: forall u v. (u -> v) -> t u -> t v)

> test_Functor_laws_with
>   :: forall a b c t
>    . ( Eq (t a), Eq (t c)
>      , MakeTo a, MakeTo b, CoArb b, CoArb a
>      , Fmt.Display b, Arb c, Arb b, Arb (t a), Prune b
>      , Prune c, Prune (t a), Fmt.Display c, Fmt.Display (t a)
>      , Arb a, Fmt.Display a, Prune a )
>   => Proxy t -> Proxy a -> Proxy b -> Proxy c
>   -> (forall u v. (u -> v) -> t u -> t v)
>   -> TestTree
> test_Functor_laws_with _ _ _ _ map =
>   testGroup "Functor laws"
>     [ krebProp "identity" $
>         (check_prop_fmap_id map :: t a -> Check)
>     , krebProp "composite" $
>         \(g :: Fun b c) (f :: Fun a b) x ->
>           check_prop_fmap_comp
>             map map map (apFun g) (apFun f) x
>     ]
