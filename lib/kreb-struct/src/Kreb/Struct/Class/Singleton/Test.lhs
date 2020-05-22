> {-# LANGUAGE FlexibleContexts #-}

> module Kreb.Struct.Class.Singleton.Test where

> import Data.Proxy

> import Test.Tasty

> import qualified Kreb.Format as Fmt
> import Kreb.Prop

> import Kreb.Struct.Class.Container
> import Kreb.Struct.Class.Singleton
> import Kreb.Struct.Class.Subset
> import Kreb.Struct.Class.NonEmpty

> test_Singleton_laws
>   :: forall t a
>    . ( Singleton t, ContainerConstraint t a
>      , Eq a, Fmt.Display a, Arb a, Prune a )
>   => String -> Proxy (t :: * -> *) -> Proxy (a :: *) -> TestTree
> test_Singleton_laws label _ _ =
>   let title = "Singleton (" ++ label ++ ")"
>   in testGroup title
>     [ krebProp
>         "isSingleton (singleton a) == True" $
>         \(a :: a) ->
>           claimTrue (isSingleton (singleton a :: t a))
>     ]

> test_SubsetSingleton_laws
>   :: forall t a
>    . ( SubsetSingleton t, ContainerConstraint t a, ContainerConstraint (SupersetOf t) a
>    , Eq a, Fmt.Display a, Arb a, Prune a
>    , Eq (t a), Fmt.Display (t a)
>    , Eq (SupersetOf t a), Fmt.Display (SupersetOf t a) )
>   => String -> Proxy (t :: * -> *) -> Proxy (a :: *) -> TestTree
> test_SubsetSingleton_laws label _ _ =
>   let title = "SubsetSingleton (" ++ label ++ ")"
>   in testGroup title
>     [ krebProp
>         "inject (singleton a) == singleton a" $
>         \(a :: a) ->
>           claimEqual
>             (inject (singleton a :: t a))
>             (singleton a)
> 
>     , krebProp
>         "restrict (singleton a) == Just (singleton a)" $
>         \(a :: a) ->
>           claimEqual
>             (restrict (singleton a :: SupersetOf t a))
>             (Just (singleton a))
>     ]

> test_NonEmptySingleton_laws
>   :: forall t a
>    . ( NonEmptySingleton t, ContainerConstraint t a, ContainerConstraint (SupersetOf t) a
>      , Eq a, Fmt.Display a, Arb a, Prune a )
>   => String -> Proxy (t :: * -> *) -> Proxy (a :: *) -> TestTree
> test_NonEmptySingleton_laws label _ _ =
>   let title = "NonEmptySingleton (" ++ label ++ ")"
>   in testGroup title
>     [ krebProp
>         "isSingleton empty == False" $
>         claimFalse (isSingleton (empty :: SupersetOf t a))
> 
>     , krebProp
>         "isEmpty (singleton a) == False" $
>         \(a :: a) ->
>           claimFalse (isEmpty (singleton a :: SupersetOf t a))
>     ]
