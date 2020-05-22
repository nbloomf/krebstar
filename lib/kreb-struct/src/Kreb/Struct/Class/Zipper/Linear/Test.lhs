> {-# LANGUAGE FlexibleContexts #-}

> module Kreb.Struct.Class.Zipper.Linear.Test where

> import Data.Proxy

> import Test.Tasty

> import qualified Kreb.Format as Fmt
> import           Kreb.Format (string)
> import           Kreb.Prop

> import Kreb.Struct.Class.Container
> import Kreb.Struct.Class.Zipper
> import Kreb.Struct.Class.Zipper.Linear

> test_LinearZipper_laws
>   :: forall t a
>    . ( LinearZipper t, ContainerConstraint t a
>      , Eq (t a), Fmt.Display (t a), Arb (t a), Prune (t a)
>      )
>   => String -> Proxy (t :: * -> *) -> Proxy (a :: *) -> TestTree
> test_LinearZipper_laws label _ _ =
>   let title = "LinearZipper (" ++ label ++ ")"
>   in testGroup title
>     [ krebProp
>         "moveTowardStart . moveTowardEnd . moveTowardStart == moveTowardStart" $
>         \(x :: t a) ->
>           claimEqual
>             (moveTowardStart $ moveTowardEnd $ moveTowardStart x)
>             (moveTowardStart x)
> 
>     , krebProp
>         "moveTowardEnd . moveTowardStart . moveTowardEnd == moveTowardEnd" $
>         \(x :: t a) ->
>           claimEqual
>             (moveTowardEnd $ moveTowardStart $ moveTowardEnd x)
>             (moveTowardEnd x)
> 
>     , krebProp
>         "moveToStart . moveToEnd . moveToStart == moveToStart" $
>         \(x :: t a) ->
>           claimEqual
>             (moveToStart $ moveToEnd $ moveToStart x)
>             (moveToStart x)
> 
>     , krebProp
>         "moveToEnd . moveToStart . moveToEnd == moveToEnd" $
>         \(x :: t a) ->
>           claimEqual
>             (moveToEnd $ moveToStart $ moveToEnd x)
>             (moveToEnd x)
> 
>     , krebProp
>         "moveToStart . moveToStart == moveToStart" $
>         \(x :: t a) ->
>           claimEqual
>             (moveToStart $ moveToStart x)
>             (moveToStart x)
> 
>     , krebProp
>         "moveToStart . moveTowardStart == moveToStart" $
>         \(x :: t a) ->
>           claimEqual
>             (moveToStart $ moveTowardStart x)
>             (moveToStart x)
> 
>     , krebProp
>         "moveToStart . moveToEnd == moveToStart" $
>         \(x :: t a) ->
>           claimEqual
>             (moveToStart $ moveToEnd x)
>             (moveToStart x)
> 
>     , krebProp
>         "moveToStart . moveTowardEnd == moveToStart" $
>         \(x :: t a) ->
>           claimEqual
>             (moveToStart $ moveTowardEnd x)
>             (moveToStart x)
> 
>     , krebProp
>         "moveToEnd . moveToEnd == moveToEnd" $
>         \(x :: t a) ->
>           claimEqual
>             (moveToEnd $ moveToEnd x)
>             (moveToEnd x)
> 
>     , krebProp
>         "moveToEnd . moveTowardEnd == moveToEnd" $
>         \(x :: t a) ->
>           claimEqual
>             (moveToEnd $ moveTowardEnd x)
>             (moveToEnd x)
> 
>     , krebProp
>         "moveToEnd . moveToStart == moveToEnd" $
>         \(x :: t a) ->
>           claimEqual
>             (moveToEnd $ moveToStart x)
>             (moveToEnd x)
> 
>     , krebProp
>         "moveToEnd . moveTowardStart == moveToEnd" $
>         \(x :: t a) ->
>           claimEqual
>             (moveToEnd $ moveTowardStart x)
>             (moveToEnd x)
> 
>     , krebProp
>         "if isAtStart x == False then moveTowardEnd $ moveTowardStart x == x" $
>         \(x :: t a) ->
>           provisio
>             [( "isAtStart x == False", isAtStart x == False )] $
>             claimEqual x (moveTowardEnd $ moveTowardStart x)
> 
>     , krebProp
>         "if isAtEnd x == False then moveTowardStart $ moveTowardEnd x == x" $
>         \(x :: t a) ->
>           provisio
>             [( "isAtEnd x == False", isAtEnd x == False )] $
>             claimEqual x (moveTowardStart $ moveTowardEnd x)
>     ]


 >     , krebProp
 >         "if isEmpty x == False then isAtStart . moveToStart == const True" $
 >         \(x :: t a) ->
 >           provisio
 >             [( "isEmpty x == False", isEmpty x == False )] $
 >             claimTrue (isAtStart $ moveToStart x)
 > 
 >     , krebProp
 >         "if isEmpty x == False then isAtEnd . moveToEnd == const True" $
 >         \(x :: t a) ->
 >           provisio
 >             [( "isEmpty x == False", isEmpty x == False )] $
 >             claimTrue (isAtEnd $ moveToEnd x)

