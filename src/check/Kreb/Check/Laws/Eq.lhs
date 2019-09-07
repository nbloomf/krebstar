> {-# LANGUAGE
>     ScopedTypeVariables
> #-}

> module Kreb.Check.Laws.Eq where
> 
> import Data.Proxy
> 
> import Test.Tasty
> 
> import Kreb.Check.Arb
> import Kreb.Check.Tasty
> import Kreb.Check.Alg



> test_Eq_laws
>   :: forall a
>    . ( Eq a, Arb a, Show a, Prune a )
>   => Proxy a -> TestTree
> test_Eq_laws _ =
>   let eq = (==) :: a -> a -> Bool in
>   testGroup "Eq laws"
>     [ testKreb "reflexivity" $
>         check_prop_reflexive eq
>     , testKreb "symmetry" $
>         check_prop_symmetric eq
>     , testKreb "transitivity" $
>         check_prop_transitive eq
>     ]