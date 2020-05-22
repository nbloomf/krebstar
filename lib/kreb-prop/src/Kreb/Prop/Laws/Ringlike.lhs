> {-# LANGUAGE
>     ScopedTypeVariables
> #-}

> module Kreb.Prop.Laws.Ringlike where
> 
> import Data.Proxy
> import Data.Semigroup
> 
> import Test.Tasty
> 
> import qualified Kreb.Format as Fmt
> 
> import Kreb.Prop.Arb
> import Kreb.Prop.Tasty
> import Kreb.Prop.Alg



> test_Left_Dioid_laws
>   :: forall a
>    . ( Eq a, Arb a, Fmt.Display a, Prune a )
>   => a -> (a -> a -> a) -> (a -> a -> a)
>   -> TestTree
> test_Left_Dioid_laws zero plus times =
>   testGroup "Left Dioid laws"
>     [ krebProp "left identity (+)" $
>         check_prop_left_neutral zero plus
>     , krebProp "right identity (+)" $
>         check_prop_right_neutral zero plus
>     , krebProp "associativity (+)" $
>         check_prop_associative plus
>     , krebProp "idempotent (+)" $
>         check_prop_idempotent_op plus
>     , krebProp "associativity (*)" $
>         check_prop_associative times
>     , krebProp "right distributivity (*/+)" $
>         check_prop_right_distributive times plus
>     , krebProp "left zero (0/+)" $
>         check_prop_left_absorbing zero times
>     ]
