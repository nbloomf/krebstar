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
