> {-# LANGUAGE FlexibleContexts #-}

> module Kreb.Struct.Class.Subset.Test where

> import Data.Proxy

> import Test.Tasty

> import qualified Kreb.Format as Fmt
> import Kreb.Prop

> import Kreb.Struct.Class.Container
> import Kreb.Struct.Class.Subset

> test_Subset_laws
>   :: forall t a
>    . ( Subset t, ContainerConstraint t a
>      , Eq (t a), Fmt.Display (t a), Arb (t a), Prune (t a)
>      , Eq (SupersetOf t a), Fmt.Display (SupersetOf t a), Arb (SupersetOf t a), Prune (SupersetOf t a) )
>   => String -> Proxy (t :: * -> *) -> Proxy (a :: *) -> TestTree
> test_Subset_laws label _ _ =
>   let title = "Subset (" ++ label ++ ")"
>   in testGroup title
>     [ krebProp
>         "restrict . inject == Just" $
>         \(x :: t a) ->
>           claimEqual (restrict (inject x)) (Just x)
> 
>     , krebProp
>         "if restrict x == Just a then inject a == x" $
>         \(x :: SupersetOf t a) ->
>           case restrict x of
>             Nothing -> accept
>             Just a -> claimEqual x (inject a)
>     ]
