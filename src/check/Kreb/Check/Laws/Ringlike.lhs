> {-# LANGUAGE
>     ScopedTypeVariables
> #-}

> module Kreb.Check.Laws.Ringlike where
> 
> import Data.Proxy
> import Data.Semigroup
> 
> import Test.Tasty
> 
> import Kreb.Check.Arb
> import Kreb.Check.Tasty
> import Kreb.Check.Alg



> test_Left_Dioid_laws
>   :: forall a
>    . ( Eq a, Arb a, Show a, Prune a )
>   => a -> (a -> a -> a) -> (a -> a -> a)
>   -> TestTree
> test_Left_Dioid_laws zero plus times =
>   testGroup "Left Dioid laws"
>     [ testKreb "left identity (+)" $
>         check_prop_left_neutral zero plus
>     , testKreb "right identity (+)" $
>         check_prop_right_neutral zero plus
>     , testKreb "associativity (+)" $
>         check_prop_associative plus
>     , testKreb "idempotent (+)" $
>         check_prop_idempotent_op plus
>     , testKreb "associativity (*)" $
>         check_prop_associative times
>     , testKreb "right distributivity (*/+)" $
>         check_prop_right_distributive times plus
>     , testKreb "left zero (0/+)" $
>         check_prop_left_absorbing zero times
>     ]
