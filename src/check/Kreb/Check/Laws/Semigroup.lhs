> {-# LANGUAGE
>     ScopedTypeVariables
> #-}

> module Kreb.Check.Laws.Semigroup where
> 
> import Data.Proxy
> import Data.Semigroup
> 
> import Test.Tasty
> 
> import Kreb.Check.Arb
> import Kreb.Check.Tasty
> import Kreb.Check.Alg



> test_Semigroup_laws
>   :: forall a
>    . ( Eq a, Arb a, Show a, Prune a, Semigroup a )
>   => Proxy a -> TestTree
> test_Semigroup_laws _ =
>   testGroup "Semigroup laws"
>     [ let op = (<>) :: a -> a -> a in
>       testKreb "associativity" $
>         check_prop_associative op
>     ]