> {-# LANGUAGE
>     ScopedTypeVariables
> #-}

> module Kreb.Prop.Laws.Semigroup where
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



> test_Semigroup_laws
>   :: forall a
>    . ( Eq a, Arb a, Fmt.Display a, Prune a, Semigroup a )
>   => Proxy a -> TestTree
> test_Semigroup_laws _ =
>   testGroup "Semigroup laws"
>     [ let op = (<>) :: a -> a -> a in
>       krebProp "associativity" $
>         check_prop_associative op
>     ]
