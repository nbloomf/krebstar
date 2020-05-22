> module Kreb.Prop.Laws.Eq where
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



> test_Eq_laws
>   :: forall a
>    . ( Eq a, Arb a, Fmt.Display a, Prune a )
>   => Proxy a -> TestTree
> test_Eq_laws _ =
>   let eq = (==) :: a -> a -> Bool in
>   testGroup "Eq laws"
>     [ krebProp "reflexivity" $
>         check_prop_reflexive eq
>     , krebProp "symmetry" $
>         check_prop_symmetric eq
>     , krebProp "transitivity" $
>         check_prop_transitive eq
>     ]
