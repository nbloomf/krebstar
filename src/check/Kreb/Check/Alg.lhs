> module Kreb.Check.Alg where

> import Kreb.Check.Arb
> import Kreb.Check.Check
> import Kreb.Check.Tests



> check_prop_fixed_point
>   :: ( Eq a, Arb a, Show a )
>   => a -> (a -> a) -> Check
> check_prop_fixed_point a f =
>   if a == f a
>     then accept
>     else reject "not a fixed point"
> 
> check_prop_involutive
>   :: ( Eq a, Arb a, Show a )
>   => (a -> a) -> a -> Check
> check_prop_involutive f x =
>   if x == f (f x)
>     then accept
>     else reject "not involutive"
> 
> check_prop_idempotent
>   :: ( Eq a, Arb a, Show a )
>   => (a -> a) -> a -> Check
> check_prop_idempotent f x =
>   if f x == f (f x)
>     then accept
>     else reject "not idempotent"



> check_prop_left_neutral
>   :: ( Eq a, Arb a, Show a )
>   => e -> (e -> a -> a) -> a -> Check
> check_prop_left_neutral e f x =
>   if x == f e x
>     then accept
>     else reject "not left neutral"
> 
> check_prop_right_neutral
>   :: ( Eq a, Arb a, Show a )
>   => e -> (a -> e -> a) -> a -> Check
> check_prop_right_neutral e f x =
>   if x == f x e
>     then accept
>     else reject "not right neutral"
> 
> check_prop_left_absorbing
>   :: ( Eq z, Arb a, Show a )
>   => z -> (z -> a -> z) -> a -> Check
> check_prop_left_absorbing z f x =
>   if z == f z x
>     then accept
>     else reject "not left absorbing"
> 
> check_prop_right_absorbing
>   :: ( Eq z, Arb a, Show a )
>   => z -> (a -> z -> z) -> a -> Check
> check_prop_right_absorbing z f x =
>   if z == f x z
>     then accept
>     else reject "not right absorbing"
> 
> check_prop_commutative
>   :: ( Eq a, Arb a, Show a )
>   => (a -> a -> a) -> a -> a -> Check
> check_prop_commutative f x y =
>   if (f x y) == (f y x)
>     then accept
>     else reject "noncommutative"
> 
> check_prop_associative
>   :: ( Eq a, Arb a, Show a )
>   => (a -> a -> a) -> a -> a -> a -> Check
> check_prop_associative f x y z =
>   if (f (f x y) z) == (f x (f y z))
>     then accept
>     else reject "nonassociative"



> check_prop_reflexive
>   :: ( Arb a, Show a )
>   => (a -> a -> Bool) -> a -> Check
> check_prop_reflexive p x =
>   if p x x
>     then accept
>     else reject "not reflexive"
> 
> check_prop_symmetric
>   :: ( Arb a, Show a )
>   => (a -> a -> Bool) -> a -> a -> Check
> check_prop_symmetric p x y =
>   if (p x y) == (p y x)
>     then accept
>     else reject "not symmetric"
> 
> check_prop_transitive
>   :: ( Arb a, Show a )
>   => (a -> a -> Bool) -> a -> a -> a -> Check
> check_prop_transitive p x y z =
>   if (p x y) && (p y z)
>     then if p x z
>       then accept
>       else reject "not transitive"
>     else accept

> check_prop_function_inverse
>   :: ( Eq a, Arb a, Show a )
>   => (b -> a) -> (a -> b) -> a -> Check
> check_prop_function_inverse g f x =
>   if x == g (f x)
>     then accept
>     else reject "not inverse"

> check_prop_function_respect_op1
>   :: ( Eq b, Arb a, Show a )
>   => (a -> a) -> (b -> b)
>   -> (a -> b) -> a -> Check
> check_prop_function_respect_op1 op1 op2 f x =
>   if f (op1 x) == op2 (f x)
>     then accept
>     else reject "not respectful"

> check_prop_function_respect_op2
>   :: ( Eq b, Arb a, Show a )
>   => (a -> a -> a) -> (b -> b -> b)
>   -> (a -> b) -> a -> a -> Check
> check_prop_function_respect_op2 op1 op2 f x y =
>   if f (op1 x y) == op2 (f x) (f y)
>     then accept
>     else reject "not respectful"
