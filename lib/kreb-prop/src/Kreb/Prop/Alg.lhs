> {-# LANGUAGE RankNTypes #-}

> module Kreb.Prop.Alg where

> import Data.Monoid

> import qualified Kreb.Format as Fmt
> import           Kreb.Format (reflow)

> import Kreb.Prop.Arb
> import Kreb.Prop.Check
> import Kreb.Prop.Tests



> -- f a == a
> check_prop_fixed_point
>   :: ( Eq a, Arb a, Fmt.Display a )
>   => a -> (a -> a) -> Check
> check_prop_fixed_point a f =
>   if a == f a
>     then accept
>     else reject $ reflow "not a fixed point"
> 
> -- f (f x) == x
> check_prop_involutive
>   :: ( Eq a, Arb a, Fmt.Display a )
>   => (a -> a) -> a -> Check
> check_prop_involutive f x =
>   if x == f (f x)
>     then accept
>     else reject $ reflow "not involutive"
> 
> -- f (f x) == f x
> check_prop_idempotent
>   :: ( Eq a, Arb a, Fmt.Display a )
>   => (a -> a) -> a -> Check
> check_prop_idempotent f x =
>   if f x == f (f x)
>     then accept
>     else reject $ reflow "not idempotent"



> -- f e x == x
> check_prop_left_neutral
>   :: ( Eq a )
>   => e -> (e -> a -> a) -> a -> Check
> check_prop_left_neutral e f x =
>   if x == f e x
>     then accept
>     else reject $ reflow "not left neutral"
> 
> -- f x e == x
> check_prop_right_neutral
>   :: ( Eq a )
>   => e -> (a -> e -> a) -> a -> Check
> check_prop_right_neutral e f x =
>   if x == f x e
>     then accept
>     else reject $ reflow "not right neutral"
> 
> -- if f e1 x == x then if f e2 x == x then e1 == e2
> check_prop_unique_left_neutral_for
>   :: ( Eq a, Eq e, Fmt.Display e )
>   => e -> (e -> a -> a) -> a -> e -> Check
> check_prop_unique_left_neutral_for e1 f x e2 =
>   if x == f e1 x
>     then if x == f e2 x
>       then claimEqual e1 e2
>       else accept
>     else reject $ reflow "claimed left neutral is not"
> 
> -- if f x e1 == x then if f x e2 == x then e1 == e2
> check_prop_unique_right_neutral_for
>   :: ( Eq a, Eq e, Fmt.Display e )
>   => e -> (a -> e -> a) -> a -> e -> Check
> check_prop_unique_right_neutral_for e1 f x e2 =
>   if x == f x e1
>     then if x == f x e2
>       then claimEqual e1 e2
>       else accept
>     else reject $ reflow "claimed right neutral is not"
> 
> -- f z x == z
> check_prop_left_absorbing
>   :: ( Eq z )
>   => z -> (z -> a -> z) -> a -> Check
> check_prop_left_absorbing z f x =
>   if z == f z x
>     then accept
>     else reject $ reflow "not left absorbing"
> 
> -- f x z == z
> check_prop_right_absorbing
>   :: ( Eq z )
>   => z -> (a -> z -> z) -> a -> Check
> check_prop_right_absorbing z f x =
>   if z == f x z
>     then accept
>     else reject $ reflow "not right absorbing"
> 
> -- f x y == f y x
> check_prop_commutative
>   :: ( Eq a )
>   => (a -> a -> a) -> a -> a -> Check
> check_prop_commutative f x y =
>   if (f x y) == (f y x)
>     then accept
>     else reject $ reflow "noncommutative"
> 
> -- x == op x x
> check_prop_idempotent_op
>   :: ( Eq a, Fmt.Display a )
>   => (a -> a -> a) -> a -> Check
> check_prop_idempotent_op op x =
>   claimEqual x (op x x)
> 
> -- f x (f y z) == f (f x y) z
> check_prop_associative
>   :: ( Eq a )
>   => (a -> a -> a) -> a -> a -> a -> Check
> check_prop_associative f x y z =
>   if (f (f x y) z) == (f x (f y z))
>     then accept
>     else reject $ reflow "nonassociative"

> -- op1 (op2 a b) c == op2 (op1 a c) (op1 b c)
> check_prop_right_distributive
>   :: ( Eq a, Fmt.Display a )
>   => (a -> a -> a) -> (a -> a -> a)
>   -> a -> a -> a -> Check
> check_prop_right_distributive op1 op2 x y z =
>   claimEqual
>     (op1 (op2 x y) z)
>     (op2 (op1 x z) (op1 y z))
> 
> -- op1 x (op2 y z) == op2 (op1 x y) (op2 x z)
> check_prop_left_distributive
>   :: ( Eq a, Fmt.Display a )
>   => (a -> a -> a) -> (a -> a -> a)
>   -> a -> a -> a -> Check
> check_prop_left_distributive op1 op2 x y z =
>   claimEqual
>     (op1 x (op2 y z))
>     (op2 (op1 x y) (op1 x z))

> -- f x x
> check_prop_reflexive
>   :: (a -> a -> Bool) -> a -> Check
> check_prop_reflexive p x =
>   if p x x
>     then accept
>     else reject $ reflow "not reflexive"
> 
> -- f x y == f y x
> check_prop_symmetric
>   :: (a -> a -> Bool) -> a -> a -> Check
> check_prop_symmetric p x y =
>   if (p x y) == (p y x)
>     then accept
>     else reject $ reflow "not symmetric"
> 
> -- if (f x y) && (f y z) then f x z
> check_prop_transitive
>   :: (a -> a -> Bool) -> a -> a -> a -> Check
> check_prop_transitive p x y z =
>   if (p x y) && (p y z)
>     then if p x z
>       then accept
>       else reject $ reflow "not transitive"
>     else accept

> -- g (f x) == x
> check_prop_function_inverse
>   :: ( Eq a )
>   => (b -> a) -> (a -> b) -> a -> Check
> check_prop_function_inverse g f x =
>   if x == g (f x)
>     then accept
>     else reject $ reflow "not inverse"

> -- f (g x) == h (f x)
> check_prop_function_respect_op1
>   :: ( Eq b )
>   => (a -> a) -> (b -> b)
>   -> (a -> b) -> a -> Check
> check_prop_function_respect_op1 op1 op2 f x =
>   if f (op1 x) == op2 (f x)
>     then accept
>     else reject $ reflow "not respectful"

> -- f (g x y) == h (f x) (f y)
> check_prop_function_respect_op2
>   :: ( Eq b )
>   => (a -> a -> a) -> (b -> b -> b)
>   -> (a -> b) -> a -> a -> Check
> check_prop_function_respect_op2 op1 op2 f x y =
>   if f (op1 x y) == op2 (f x) (f y)
>     then accept
>     else reject $ reflow "not respectful"

> -- f (g x y) == h (f y) (f x)
> check_prop_function_antirespect_op2
>   :: ( Eq b )
>   => (a -> a -> a) -> (b -> b -> b)
>   -> (a -> b) -> a -> a -> Check
> check_prop_function_antirespect_op2 op1 op2 f x y =
>   if f (op1 x y) == op2 (f y) (f x)
>     then accept
>     else reject $ reflow "not antirespectful"



> check_prop_left_act_identity
>   :: ( Eq b, Fmt.Display b )
>   => (a -> b -> b) -> a
>   -> b -> Check
> check_prop_left_act_identity act e x =
>   claimEqual x (act e x)
> 
> check_prop_left_act_product
>   :: ( Eq b, Fmt.Display b )
>   => (a -> b -> b) -> (a -> a -> a)
>   -> a -> a -> b -> Check
> check_prop_left_act_product act op u v x =
>   claimEqual (act (op u v) x) (act u (act v x))
> 
> check_prop_right_act_identity
>   :: ( Eq b, Fmt.Display b )
>   => (b -> a -> b) -> a
>   -> b -> Check
> check_prop_right_act_identity act e x =
>   claimEqual x (act x e)
> 
> check_prop_right_act_product
>   :: ( Eq b, Fmt.Display b )
>   => (b -> a -> b) -> (a -> a -> a)
>   -> a -> a -> b -> Check
> check_prop_right_act_product act op u v x =
>   claimEqual (act x (op u v)) (act (act x u) v)



> -- act a (op x y) == op (act a x) y
> check_prop_left_affine
>   :: ( Eq b )
>   => (a -> b -> b) -> (b -> b -> b)
>   -> a -> b -> b -> Check
> check_prop_left_affine act op a x y =
>   if act a (op x y) == op (act a x) y
>     then accept
>     else reject $ reflow "not left affine"
> 
> -- act (op x y) a == op x (act y a)
> check_prop_right_affine
>   :: ( Eq b, Fmt.Display b )
>   => (b -> a -> b) -> (b -> b -> b)
>   -> a -> b -> b -> Check
> check_prop_right_affine act op a x y =
>   claimEqual (act (op x y) a) (op x (act y a))

> -- f (act1 a x) == act2 a (f x)
> check_prop_left_affine_func
>   :: ( Eq c, Fmt.Display c )
>   => (a -> b -> b) -> (a -> c -> c) -> (b -> c)
>   -> a -> b -> Check
> check_prop_left_affine_func act1 act2 f a x =
>   claimEqual (f (act1 a x)) (act2 a (f x))
> 
> -- f (act1 x a) == act2 (f x) a
> check_prop_right_affine_func
>   :: ( Eq c, Fmt.Display c )
>   => (b -> a -> b) -> (c -> a -> c) -> (b -> c)
>   -> a -> b -> Check
> check_prop_right_affine_func act1 act2 f a x =
>   claimEqual (f (act1 x a)) (act2 (f x) a)
> 
> -- f (act1 a x) == act2 (f x) a
> check_prop_left_right_affine_func
>   :: ( Eq c, Fmt.Display c )
>   => (a -> b -> b) -> (c -> a -> c) -> (b -> c)
>   -> a -> b -> Check
> check_prop_left_right_affine_func act1 act2 f a x =
>   claimEqual (f (act1 a x)) (act2 (f x) a)
> 
> -- f (act1 x a) == act2 a (f x)
> check_prop_right_left_affine_func
>   :: ( Eq c, Fmt.Display c )
>   => (b -> a -> b) -> (a -> c -> c) -> (b -> c)
>   -> a -> b -> Check
> check_prop_right_left_affine_func act1 act2 f a x =
>   claimEqual (f (act1 x a)) (act2 a (f x))



> check_prop_fmap_id
>   :: ( Eq (t a) )
>   => ((a -> a) -> t a -> t a)
>   -> t a -> Check
> check_prop_fmap_id map x =
>   if x == map id x
>     then accept
>     else reject $ reflow "fmap id"
> 
> check_prop_fmap_comp
>   :: ( Eq (t c) )
>   => ((b -> c) -> t b -> t c)
>   -> ((a -> b) -> t a -> t b)
>   -> ((a -> c) -> t a -> t c)
>   -> (b -> c) -> (a -> b) -> t a -> Check
> check_prop_fmap_comp map2 map1 map g f x =
>   if map (g . f) x == map2 g (map1 f x)
>     then accept
>     else reject $ reflow "fmap comp"



Foldable Laws

> -- fold == foldMap id
> check_prop_foldable_foldMap_id
>   :: ( Eq m, Fmt.Display m, Monoid m )
>   => (t m -> m) -> ((m -> m) -> t m -> m)
>   -> t m -> Check
> check_prop_foldable_foldMap_id fold foldMap x =
>   claimEqual (fold x) (foldMap id x)
> 
> -- foldr f z t = appEndo (foldMap (Endo . f) t ) z
> check_prop_foldable_foldr_foldMap
>   :: ( Eq b, Fmt.Display b )
>   => ((a -> b -> b) -> b -> t -> b)
>   -> ((a -> Endo b) -> t -> Endo b)
>   -> (a -> b -> b) -> b -> t -> Check
> check_prop_foldable_foldr_foldMap foldr foldMap f z t =
>   claimEqual (foldr f z t) (appEndo (foldMap (Endo . f) t) z)

Foldable Functor Laws

> -- foldMap f == fold . fmap f
> check_prop_foldable_fold_fmap
>   :: ( Eq m, Fmt.Display m, Monoid m )
>   => ((a -> m) -> t a -> m) -> (t m -> m) -> ((a -> m) -> t a -> t m)
>   -> (a -> m) -> t a -> Check
> check_prop_foldable_fold_fmap foldMap fold fmap f x =
>   claimEqual (foldMap f x) (fold $ fmap f x)
> 
> -- foldMap f . fmap g == foldMap (f . g)
> check_prop_foldable_foldMap_fmap
>   :: ( Eq m, Fmt.Display m, Monoid m )
>   => ((b -> m) -> t b -> m) -> ((a -> b) -> t a -> t b) -> ((a -> m) -> t a -> m)
>   -> (b -> m) -> (a -> b) -> t a -> Check
> check_prop_foldable_foldMap_fmap foldMap1 fmap foldMap2 g f x =
>   claimEqual (foldMap1 g $ fmap f x) (foldMap2 (g . f) x)
