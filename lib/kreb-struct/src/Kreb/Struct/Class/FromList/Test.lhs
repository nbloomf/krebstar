> module Kreb.Struct.Class.FromList.Test where

> import Prelude hiding (reverse)
> import qualified Prelude as Prelude (reverse)

> import Data.Proxy
> import Data.Foldable

> import Test.Tasty

> import qualified Kreb.Format as Fmt
> import Kreb.Prop

> import Kreb.Struct.Class.Container
> import Kreb.Struct.Class.FromList
> import Kreb.Struct.Class.Cons
> import Kreb.Struct.Class.Reverse

> test_FromList_laws
>   :: forall t a
>    . ( FromList t, ContainerConstraint t a
>      , Eq a, Fmt.Display a
>      , Eq (t a), Fmt.Display (t a), Arb (t a), Prune (t a) )
>   => String -> Proxy (t :: * -> *) -> Proxy (a :: *) -> TestTree
> test_FromList_laws label _ _ =
>   let title = "FromList (" ++ label ++ ")"
>   in testGroup title
>     [ krebProp
>         "fromList . toList == id" $
>         \(x :: t a) ->
>           claimEqual x (fromList (toList x))
> 
>     , krebProp
>         "toList (x <> y) == toList x ++ toList y" $
>         \(x :: t a) (y :: t a) ->
>           claimEqual
>             (toList (x <> y))
>             ((toList x) ++ (toList y))
>     ]

> test_FromListMonoid_laws
>   :: forall t a
>    . ( FromListMonoid t, ContainerConstraint t a
>      , Eq a, Fmt.Display a, Arb a, Prune a
>      , Eq (t a), Fmt.Display (t a) )
>   => String -> Proxy (t :: * -> *) -> Proxy (a :: *) -> TestTree
> test_FromListMonoid_laws label _ _ =
>   let title = "FromListMonoid (" ++ label ++ ")"
>   in testGroup title
>     [ krebProp
>         "toList . fromList == id" $
>         \(x :: [a]) ->
>           claimEqual x (toList (fromList x :: t a))
> 
>     , krebProp
>         "fromList (x ++ y) == fromList x <> fromList y" $
>         \(x :: [a]) (y :: [a]) ->
>           claimEqual
>             (fromList (x ++ y) :: t a)
>             ((fromList x) <> (fromList y))
> 
>     , krebProp
>         "fromList mempty == mempty" $
>         claimEqual (mempty :: t a) (fromList mempty)
> 
>     , krebProp
>         "toList mempty == mempty" $
>         claimEqual (mempty) (toList (mempty :: t a))
>     ]

> test_FromListConsSnocReverse_laws
>   :: forall t a
>    . ( FromListConsSnocReverse t, ContainerConstraint t a
>      , Eq a, Fmt.Display a, Arb a, Prune a
>      , Eq (t a), Fmt.Display (t a), Arb (t a), Prune (t a) )
>   => String -> Proxy (t :: * -> *) -> Proxy (a :: *) -> TestTree
> test_FromListConsSnocReverse_laws label _ _ =
>   let title = "FromListConsSnocReverse (" ++ label ++ ")"
>   in testGroup title
>     [ krebProp
>         "toList (cons a x) == a : toList x" $
>         \(a :: a) (x :: t a) ->
>           claimEqual
>             (toList (cons a x))
>             (a : (toList x))
> 
>     , krebProp
>         "toList (snoc a x) == (toList x) ++ [a]" $
>         \(a :: a) (x :: t a) ->
>           claimEqual
>             (toList (snoc a x))
>             ((toList x) ++ [a])
> 
>     , krebProp
>         "fromList (a : as) == cons a (fromList as)" $
>         \(a :: a) (as :: [a]) ->
>           claimEqual
>             (fromList (a : as) :: t a)
>             (cons a (fromList as))
> 
>     , krebProp
>         "fromList (as ++ [a]) == snoc a (fromList as)" $
>         \(a :: a) (as :: [a]) ->
>           claimEqual
>             (fromList (as ++ [a]) :: t a)
>             (snoc a (fromList as))
> 
>     , krebProp
>         "toList (reverse x) == reverse (toList x)" $
>         \(x :: t a) ->
>           claimEqual
>             (toList (reverse x))
>             (Prelude.reverse (toList x))
> 
>     , krebProp
>         "fromList (reverse x) == reverse (fromList x)" $
>         \(x :: [a]) ->
>           claimEqual
>             (fromList (Prelude.reverse x))
>             (reverse (fromList x :: t a))
>     ]
