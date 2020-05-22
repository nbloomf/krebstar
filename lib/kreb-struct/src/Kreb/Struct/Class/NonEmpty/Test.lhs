> {-# LANGUAGE FlexibleContexts #-}

> module Kreb.Struct.Class.NonEmpty.Test where

> import Data.Proxy

> import Test.Tasty

> import qualified Kreb.Format as Fmt
> import Kreb.Prop

> import Kreb.Struct.Class.Container
> import Kreb.Struct.Class.Subset
> import Kreb.Struct.Class.NonEmpty

> test_NonEmpty_laws
>   :: forall t a
>    . ( NonEmpty t, ContainerConstraint t a
>      , Eq (t a), Fmt.Display (t a), Arb (t a), Prune (t a)
>      , Eq (SupersetOf t a), Fmt.Display (SupersetOf t a), Arb (SupersetOf t a), Prune (SupersetOf t a) )
>   => String -> Proxy (t :: * -> *) -> Proxy (a :: *) -> TestTree
> test_NonEmpty_laws label _ _ =
>   let title = "NonEmpty (" ++ label ++ ")"
>   in testGroup title
>     [ krebProp
>         "isEmpty empty == True" $
>         claimTrue (isEmpty (empty :: SupersetOf t a))
> 
>     , krebProp
>         "(restrict x == Nothing) == (isEmpty x)" $
>         \(x :: SupersetOf t a) ->
>           claimEqual (restrict x == Nothing) (isEmpty x)
> 
>     , krebProp
>         "isEmpty (inject x) == False" $
>         \(x :: t a) ->
>           claimFalse (isEmpty (inject x))
>     ]
