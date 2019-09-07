> {-# LANGUAGE
>     ScopedTypeVariables
> #-}

> module Kreb.Check.Laws.Monoid where
> 
> import Data.Proxy
> import Data.Monoid
> 
> import Test.Tasty
> 
> import Kreb.Check.Arb
> import Kreb.Check.Tasty
> import Kreb.Check.Alg



> test_Monoid_laws
>   :: forall a
>    . ( Eq a, Arb a, Show a, Prune a, Monoid a )
>   => Proxy a -> TestTree
> test_Monoid_laws _ =
>   let
>     em = mempty :: a
>     op = mappend :: a -> a -> a
>   in testGroup "Monoid laws"
>     [ testKreb "associativity" $
>         check_prop_associative op
>     , testKreb "left identity" $
>         check_prop_left_neutral em op
>     , testKreb "right identity" $
>         check_prop_right_neutral em op
>     ]

> test_Monoid_homomorphism
>   :: forall a b
>    . ( Eq b, Arb a, Show a, Prune a, Monoid a, Monoid b )
>   => (a -> b) -> TestTree
> test_Monoid_homomorphism f =
>   let
>     em1 = const mempty :: a -> a
>     em2 = const mempty :: b -> b
>     op1 = mappend :: a -> a -> a
>     op2 = mappend :: b -> b -> b
>   in testGroup "Monoid homomorphism"
>     [ testKreb "preserve identity" $
>         check_prop_function_respect_op1 em1 em2 f
>     , testKreb "preserve products" $
>         check_prop_function_respect_op2 op1 op2 f
>     ]
