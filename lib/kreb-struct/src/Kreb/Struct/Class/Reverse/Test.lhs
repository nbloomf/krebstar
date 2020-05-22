> {-# LANGUAGE FlexibleContexts #-}

> module Kreb.Struct.Class.Reverse.Test where

> import Prelude hiding (reverse)
> import Data.Proxy

> import Test.Tasty

> import qualified Kreb.Format as Fmt
> import Kreb.Prop

> import Kreb.Struct.Class.Container
> import Kreb.Struct.Class.Reverse
> import Kreb.Struct.Class.Subset
> import Kreb.Struct.Class.Singleton
> import Kreb.Struct.Class.Cons

> test_Reverse_laws
>   :: forall a t
>    . ( Reverse t, ContainerConstraint t a
>      , Eq (t a), Fmt.Display (t a), Arb (t a), Prune (t a) )
>   => String -> Proxy (t :: * -> *) -> Proxy (a :: *) -> TestTree
> test_Reverse_laws label _ _ =
>   let title = "Reverse (" ++ label ++ ")"
>   in testGroup title
>     [ krebProp
>         "reverse . reverse == id" $
>         \(x :: t a) ->
>           claimEqual x (reverse (reverse x))
>     ]

> test_ReverseSemigroup_laws
>   :: forall a t
>    . ( ReverseSemigroup t, ContainerConstraint t a
>      , Eq (t a), Fmt.Display (t a), Arb (t a), Prune (t a) )
>   => String -> Proxy (t :: * -> *) -> Proxy (a :: *) -> TestTree
> test_ReverseSemigroup_laws label _ _ =
>   let title = "ReverseSemigroup (" ++ label ++ ")"
>   in testGroup title
>     [ krebProp
>         "reverse (x <> y) == (reverse y) <> (reverse x)" $
>         \(x :: t a) (y :: t a) ->
>           claimEqual
>             (reverse (x <> y))
>             ((reverse y) <> (reverse x))
>     ]

> test_ReverseMonoid_laws
>   :: forall a t
>    . ( ReverseMonoid t, ContainerConstraint t a, Eq (t a), Fmt.Display (t a) )
>   => String -> Proxy (t :: * -> *) -> Proxy (a :: *) -> TestTree
> test_ReverseMonoid_laws label _ _ =
>   let title = "ReverseMonoid (" ++ label ++ ")"
>   in testGroup title
>     [ krebProp
>         "reverse mempty == mempty" $
>         claimEqual (reverse mempty) (mempty :: t a)
>     ]

> test_ReverseSubset_laws
>   :: forall a t
>    . ( ReverseSubset t, ContainerConstraint t a, ContainerConstraint (SupersetOf t) a
>      , Eq (t a), Fmt.Display (t a), Arb (t a), Prune (t a)
>      , Eq (SupersetOf t a), Fmt.Display (SupersetOf t a), Arb (SupersetOf t a), Prune (SupersetOf t a) )
>   => String -> Proxy (t :: * -> *) -> Proxy (a :: *) -> TestTree
> test_ReverseSubset_laws label _ _ =
>   let title = "ReverseSubset (" ++ label ++ ")"
>   in testGroup title
>     [ krebProp
>         "reverse (inject x) == inject (reverse x)" $
>         \(x :: t a) ->
>           claimEqual (reverse (inject x)) (inject (reverse x))
> 
>     , krebProp
>         "restrict (reverse x) == fmap reverse (restrict x)" $
>         \(x :: SupersetOf t a) ->
>           claimEqual (restrict (reverse x)) (fmap reverse (restrict x))
>     ]

> test_ReverseSingleton_laws
>   :: forall a t
>    . ( ReverseSingleton t, ContainerConstraint t a
>      , Eq a, Fmt.Display a, Arb a, Prune a
>      , Eq (t a), Fmt.Display (t a), Arb (t a), Prune (t a) )
>   => String -> Proxy (t :: * -> *) -> Proxy (a :: *) -> TestTree
> test_ReverseSingleton_laws label _ _ =
>   let title = "ReverseSingleton (" ++ label ++ ")"
>   in testGroup title
>     [ krebProp
>         "reverse (singleton a) == singleton a" $
>         \(a :: a) ->
>           claimEqual
>             (reverse (singleton a))
>             (singleton a :: t a)
>     ]

> test_ReverseConsSnoc_laws
>   :: forall a t
>    . ( ReverseConsSnoc t, ContainerConstraint t a
>      , Eq a, Fmt.Display a, Arb a, Prune a
>      , Eq (t a), Fmt.Display (t a), Arb (t a), Prune (t a) )
>   => String -> Proxy (t :: * -> *) -> Proxy (a :: *) -> TestTree
> test_ReverseConsSnoc_laws label _ _ =
>   let title = "ReverseConsSnoc (" ++ label ++ ")"
>   in testGroup title
>     [ krebProp
>         "reverse (cons a x) == snoc a (reverse x)" $
>         \(a :: a) (x :: t a) ->
>           claimEqual
>             (reverse (cons a x))
>             (snoc a (reverse x))
> 
>     , krebProp
>         "reverse (snoc a x) == cons a (reverse x)" $
>         \(a :: a) (x :: t a) ->
>           claimEqual
>             (reverse (snoc a x))
>             (cons a (reverse x))
>     ]
