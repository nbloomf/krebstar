> module Kreb.Prop.Laws.Monoid where
> 
> import Data.Proxy
> import Data.Monoid
> 
> import Test.Tasty
> 
> import qualified Kreb.Format as Fmt
> 
> import Kreb.Prop.Arb
> import Kreb.Prop.Tasty
> import Kreb.Prop.Alg



> test_Monoid_laws
>   :: forall a
>    . ( Eq a, Arb a, Fmt.Display a, Prune a, Monoid a )
>   => Proxy a -> TestTree
> test_Monoid_laws _ =
>   let
>     em = mempty :: a
>     op = mappend :: a -> a -> a
>   in testGroup "Monoid laws"
>     [ krebProp "associativity" $
>         check_prop_associative op
>     , krebProp "left identity" $
>         check_prop_left_neutral em op
>     , krebProp "right identity" $
>         check_prop_right_neutral em op
>     ]

> test_Idempotent_Monoid_laws
>   :: forall a
>    . ( Eq a, Arb a, Fmt.Display a, Prune a, Monoid a )
>   => a -> (a -> a -> a) -> TestTree
> test_Idempotent_Monoid_laws em op =
>   testGroup "Monoid laws"
>     [ krebProp "associativity" $
>         check_prop_associative op
>     , krebProp "left identity" $
>         check_prop_left_neutral em op
>     , krebProp "right identity" $
>         check_prop_right_neutral em op
>     , krebProp "idempotence" $
>         check_prop_idempotent_op op
>     ]

> test_Monoid_left_action_laws
>   :: forall a b
>    . ( Monoid a, Fmt.Display a, Fmt.Display b, Eq b, Arb a, Arb b, Prune a, Prune b )
>   => (a -> b -> b) -> TestTree
> test_Monoid_left_action_laws act =
>   let
>     em = mempty :: a
>     op = mappend :: a -> a -> a
>   in testGroup "left Monoid action laws"
>     [ krebProp "identity" $
>         check_prop_left_act_identity act em
>     , krebProp "product" $
>         check_prop_left_act_product act op
>     ]

> test_Monoid_right_action_laws
>   :: forall a b
>    . ( Monoid a, Fmt.Display a, Fmt.Display b, Eq b, Arb a, Arb b, Prune a, Prune b )
>   => (b -> a -> b) -> TestTree
> test_Monoid_right_action_laws act =
>   let
>     em = mempty :: a
>     op = mappend :: a -> a -> a
>   in testGroup "right Monoid action laws"
>     [ krebProp "identity" $
>         check_prop_right_act_identity act em
>     , krebProp "product" $
>         check_prop_right_act_product act op
>     ]

> test_Monoid_homomorphism
>   :: forall a b
>    . ( Eq b, Arb a, Fmt.Display a, Prune a, Monoid a, Monoid b )
>   => (a -> b) -> TestTree
> test_Monoid_homomorphism f =
>   let
>     em1 = const mempty :: a -> a
>     em2 = const mempty :: b -> b
>     op1 = mappend :: a -> a -> a
>     op2 = mappend :: b -> b -> b
>   in testGroup "Monoid homomorphism"
>     [ krebProp "preserve identity" $
>         check_prop_function_respect_op1 em1 em2 f
>     , krebProp "preserve products" $
>         check_prop_function_respect_op2 op1 op2 f
>     ]

> test_Monoid_antihomomorphism
>   :: forall a b
>    . ( Eq b, Arb a, Fmt.Display a, Prune a, Monoid a, Monoid b )
>   => (a -> b) -> TestTree
> test_Monoid_antihomomorphism f =
>   let
>     em1 = const mempty :: a -> a
>     em2 = const mempty :: b -> b
>     op1 = mappend :: a -> a -> a
>     op2 = mappend :: b -> b -> b
>   in testGroup "Monoid antihomomorphism"
>     [ krebProp "preserve identity" $
>         check_prop_function_respect_op1 em1 em2 f
>     , krebProp "reverse products" $
>         check_prop_function_antirespect_op2 op1 op2 f
>     ]
