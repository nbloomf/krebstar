> {-# LANGUAGE FlexibleContexts #-}

> module Kreb.Struct.Class.Zipper.Test where

> import Kreb.Prop

> import Kreb.Struct.Class.Zipper

> import Data.Proxy

> import Test.Tasty

> import qualified Kreb.Format as Fmt
> import           Kreb.Prop

> import Kreb.Struct.Class.Container
> import Kreb.Struct.Class.Zipper

> test_Zipper_laws
>   :: forall t a
>    . ( Zipper t, ContainerConstraint t a
>      , Eq (Zipped t a), Fmt.Display (Zipped t a), Arb (Zipped t a), Prune (Zipped t a) )
>   => String -> Proxy (t :: * -> *) -> Proxy (a :: *) -> TestTree
> test_Zipper_laws label _ _ =
>   let title = "Zipper (" ++ label ++ ")"
>   in testGroup title
>     [ krebProp
>         "fromZipper . toZipper == id" $
>         \(x :: Zipped t a) ->
>           claimEqual x (fromZipper (toZipper x))
>     ]
