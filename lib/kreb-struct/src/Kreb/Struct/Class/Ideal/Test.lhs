> {-# LANGUAGE FlexibleContexts #-}

> module Kreb.Struct.Class.Ideal.Test where

> import Data.Proxy

> import Test.Tasty

> import qualified Kreb.Format as Fmt
> import Kreb.Prop

> import Kreb.Struct.Class.Container
> import Kreb.Struct.Class.Subset
> import Kreb.Struct.Class.Ideal

> test_Subsemigroup_laws
>   :: forall a t
>    . ( Subsemigroup t, ContainerConstraint t a, ContainerConstraint (SupersetOf t) a
>      , Eq (t a), Fmt.Display (t a), Arb (t a), Prune (t a)
>      , Eq (SupersetOf t a), Fmt.Display (SupersetOf t a), Semigroup (SupersetOf t a) )
>   => String -> Proxy (t :: * -> *) -> Proxy (a :: *) -> TestTree
> test_Subsemigroup_laws label _ _ =
>   let title = "Subsemigroup (" ++ label ++ ")"
>   in testGroup title
>     [ krebProp
>         "inject (u <> v) == (inject u) <> (inject v)" $
>         \(u :: t a) (v :: t a) ->
>           claimEqual
>             (inject (u <> v))
>             ((inject u) <> (inject v))
>     ]

> test_Ideal_laws
>   :: forall a t
>    . ( Ideal t, ContainerConstraint t a, ContainerConstraint (SupersetOf t) a
>      , Eq (t a), Fmt.Display (t a), Arb (t a), Prune (t a)
>      , Eq (SupersetOf t a), Fmt.Display (SupersetOf t a), Arb (SupersetOf t a), Prune (SupersetOf t a), Semigroup (SupersetOf t a) )
>   => String -> Proxy (t :: * -> *) -> Proxy (a :: *) -> TestTree
> test_Ideal_laws label _ _ =
>   let title = "Ideal (" ++ label ++ ")"
>   in testGroup title
>     [ krebProp
>         "inject (u @> v) == (inject u) <> v" $
>         \(u :: t a) (v :: SupersetOf t a) ->
>           claimEqual
>             (inject (u @> v))
>             ((inject u) <> v)
> 
>     , krebProp
>         "inject (u <@ v) == u <> (inject v)" $
>         \(u :: SupersetOf t a) (v :: t a) ->
>           claimEqual
>             (inject (u <@ v))
>             (u <> (inject v))
>     ]
