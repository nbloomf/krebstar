> {-# LANGUAGE FlexibleContexts #-}

> module Kreb.Struct.Class.Cons.Test where

> import Data.Proxy

> import Test.Tasty

> import qualified Kreb.Format as Fmt
> import Kreb.Prop

> import Kreb.Struct.Class.Container
> import Kreb.Struct.Class.Cons
> import Kreb.Struct.Class.Subset
> import Kreb.Struct.Class.NonEmpty
> import Kreb.Struct.Class.Singleton

> test_Cons_laws
>   :: forall t a
>    . ( Cons t, ContainerConstraint t a
>      , Eq a, Fmt.Display a, Arb a, Prune a
>      , Eq (t a), Fmt.Display (t a), Arb (t a), Prune (t a) )
>   => String -> Proxy (t :: * -> *) -> Proxy (a :: *) -> TestTree
> test_Cons_laws label _ _ =
>   let title = "Cons (" ++ label ++ ")"
>   in testGroup title
>     [ krebProp
>         "cons a (x <> y) == (cons a x) <> y" $
>         \(a :: a) (x :: t a) (y :: t a) ->
>           claimEqual (cons a (x <> y)) ((cons a x) <> y)
> 
>     , krebProp
>         "uncons (cons a x) == Just (a, x)" $
>         \(a :: a) (x :: t a) ->
>           claimEqual (uncons (cons a x)) (Just (a, x))
> 
>     , krebProp
>         "if uncons w == Just (a, x) then cons a x == w" $
>         \(w :: t a) ->
>           case uncons w of
>             Nothing -> accept
>             Just (a, x) -> claimEqual w (cons a x)
>     ]

> test_SingletonCons_laws
>   :: forall t a
>    . ( SingletonCons t, ContainerConstraint t a
>      , Eq a, Fmt.Display a, Arb a, Prune a
>      , Eq (t a), Fmt.Display (t a), Arb (t a), Prune (t a) )
>   => String -> Proxy (t :: * -> *) -> Proxy (a :: *) -> TestTree
> test_SingletonCons_laws label _ _ =
>   let title = "SingletonCons (" ++ label ++ ")"
>   in testGroup title
>     [ krebProp
>         "cons a x == (singleton a) <> x" $
>         \(a :: a) (x :: t a) ->
>           claimEqual (cons a x) ((singleton a) <> x)
>     ]

> test_SubsetCons_laws
>   :: forall t a
>    . ( SubsetCons t, ContainerConstraint t a, ContainerConstraint (SupersetOf t) a
>      , Eq a, Fmt.Display a, Arb a, Prune a
>      , Eq (t a), Fmt.Display (t a), Arb (t a), Prune (t a)
>      , Eq (SupersetOf t a), Fmt.Display (SupersetOf t a) )
>   => String -> Proxy (t :: * -> *) -> Proxy (a :: *) -> TestTree
> test_SubsetCons_laws label _ _ =
>   let title = "SubsetCons (" ++ label ++ ")"
>   in testGroup title
>     [ krebProp
>         "inject (cons a x) == cons a (inject x)" $
>         \(a :: a) (x :: t a) ->
>           claimEqual (inject (cons a x)) (cons a (inject x))
> 
>     , krebProp
>         "restrict (cons a (inject x)) == Just (cons a x)" $
>         \(a :: a) (x :: t a) ->
>           claimEqual (restrict (cons a (inject x))) (Just (cons a x))
>     ]

> test_UnconsNonEmpty_laws
>   :: forall t a
>    . ( UnconsNonEmpty t, ContainerConstraint t a, ContainerConstraint (SupersetOf t) a
>      , Eq a, Fmt.Display a, Arb a, Prune a
>      , Eq (t a), Fmt.Display (t a), Arb (t a), Prune (t a)
>      , Eq (SupersetOf t a), Fmt.Display (SupersetOf t a), Arb (SupersetOf t a), Prune (SupersetOf t a) )
>   => String -> Proxy (t :: * -> *) -> Proxy (a :: *) -> TestTree
> test_UnconsNonEmpty_laws label _ _ =
>   let title = "UnconsNonEmpty (" ++ label ++ ")"
>   in testGroup title
>     [ krebProp
>         "inject x = cons a as where (a, as) == unconsNonEmpty x" $
>         \(x :: t a) ->
>           let (a, as) = unconsNonEmpty x
>           in claimEqual
>             (inject x)
>             (cons a as)
> 
>     , krebProp
>         "fmap unconsNonEmpty (restrict (cons a x)) == Just (a, x)" $
>         \(a :: a) (x :: SupersetOf t a) ->
>           claimEqual
>             (fmap unconsNonEmpty (restrict (cons a x)))
>             (Just (a, x))
> 
>     , krebProp
>         "isEmpty x == (Nothing == uncons x)" $
>         \(x :: SupersetOf t a) ->
>           claimEqual (isEmpty x) (Nothing == uncons x)
> 
>     , krebProp
>         "if uncons w == Just (a, x) then unconsNonEmpty w == (a, inject x)" $
>         \(w :: t a) ->
>           case uncons w of
>             Nothing -> accept
>             Just (a, x) -> claimEqual (unconsNonEmpty w) (a, inject x)
>     ]

> test_Snoc_laws
>   :: forall t a
>    . ( Snoc t, ContainerConstraint t a
>      , Eq a, Fmt.Display a, Arb a, Prune a
>      , Eq (t a), Fmt.Display (t a), Arb (t a), Prune (t a) )
>   => String -> Proxy (t :: * -> *) -> Proxy (a :: *) -> TestTree
> test_Snoc_laws label _ _ =
>   let title = "Snoc (" ++ label ++ ")"
>   in testGroup title
>     [ krebProp
>         "snoc a (x <> y) == x <> (snoc a y)" $
>         \(a :: a) (x :: t a) (y :: t a) ->
>           claimEqual (snoc a (x <> y)) (x <> (snoc a y))
> 
>     , krebProp
>         "uncons (snoc a x) == Just (a, x)" $
>         \(a :: a) (x :: t a) ->
>           claimEqual (unsnoc (snoc a x)) (Just (a, x))
> 
>     , krebProp
>         "if unsnoc w == Just (a, x) then snoc a x == w" $
>         \(w :: t a) ->
>           case unsnoc w of
>             Nothing -> accept
>             Just (a, x) -> claimEqual w (snoc a x)
>     ]

> test_SingletonSnoc_laws
>   :: forall t a
>    . ( SingletonSnoc t, ContainerConstraint t a
>      , Eq a, Fmt.Display a, Arb a, Prune a
>      , Eq (t a), Fmt.Display (t a), Arb (t a), Prune (t a) )
>   => String -> Proxy (t :: * -> *) -> Proxy (a :: *) -> TestTree
> test_SingletonSnoc_laws label _ _ =
>   let title = "SingletonSnoc (" ++ label ++ ")"
>   in testGroup title
>     [ krebProp
>         "snoc a x == x <> (singleton a)" $
>         \(a :: a) (x :: t a) ->
>           claimEqual (snoc a x) (x <> (singleton a))
>     ]

> test_SubsetSnoc_laws
>   :: forall t a
>    . ( SubsetSnoc t, ContainerConstraint t a, ContainerConstraint (SupersetOf t) a
>      , Eq a, Fmt.Display a, Arb a, Prune a
>      , Eq (t a), Fmt.Display (t a), Arb (t a), Prune (t a)
>      , Eq (SupersetOf t a), Fmt.Display (SupersetOf t a) )
>   => String -> Proxy (t :: * -> *) -> Proxy (a :: *) -> TestTree
> test_SubsetSnoc_laws label _ _ =
>   let title = "SubsetSnoc (" ++ label ++ ")"
>   in testGroup title
>     [ krebProp
>         "inject (snoc a x) == snoc a (inject x)" $
>         \(a :: a) (x :: t a) ->
>           claimEqual (inject (snoc a x)) (snoc a (inject x))
> 
>     , krebProp
>         "restrict (snoc a (inject x)) == Just (snoc a x)" $
>         \(a :: a) (x :: t a) ->
>           claimEqual (restrict (snoc a (inject x))) (Just (snoc a x))
>     ]

> test_UnsnocNonEmpty_laws
>   :: forall t a
>    . ( UnsnocNonEmpty t, ContainerConstraint t a, ContainerConstraint (SupersetOf t) a
>      , Eq a, Fmt.Display a, Arb a, Prune a
>      , Eq (t a), Fmt.Display (t a), Arb (t a), Prune (t a)
>      , Eq (SupersetOf t a), Fmt.Display (SupersetOf t a), Arb (SupersetOf t a), Prune (SupersetOf t a) )
>   => String -> Proxy (t :: * -> *) -> Proxy (a :: *) -> TestTree
> test_UnsnocNonEmpty_laws label _ _ =
>   let title = "UnsnocNonEmpty (" ++ label ++ ")"
>   in testGroup title
>     [ krebProp
>         "inject x = snoc a as where (a, as) == unsnocNonEmpty x" $
>         \(x :: t a) ->
>           let (a, as) = unsnocNonEmpty x
>           in claimEqual
>             (inject x)
>             (snoc a as)
> 
>     , krebProp
>         "fmap unsnocNonEmpty (restrict (snoc a x)) == Just (a, x)" $
>         \(a :: a) (x :: SupersetOf t a) ->
>           claimEqual
>             (fmap unsnocNonEmpty (restrict (snoc a x)))
>             (Just (a, x))
> 
>     , krebProp
>         "isEmpty x == (Nothing == unsnoc x)" $
>         \(x :: SupersetOf t a) ->
>           claimEqual (isEmpty x) (Nothing == unsnoc x)
> 
>     , krebProp
>         "if unsnoc w == Just (a, x) then unsnocNonEmpty w == (a, inject x)" $
>         \(w :: t a) ->
>           case unsnoc w of
>             Nothing -> accept
>             Just (a, x) -> claimEqual (unsnocNonEmpty w) (a, inject x)
>     ]

> test_ConsSnoc_laws
>   :: forall t a
>    . ( ConsSnoc t, ContainerConstraint t a
>      , Eq a, Fmt.Display a, Arb a, Prune a
>      , Eq (t a), Fmt.Display (t a), Arb (t a), Prune (t a) )
>   => String -> Proxy (t :: * -> *) -> Proxy (a :: *) -> TestTree
> test_ConsSnoc_laws label _ _ =
>   let title = "ConsSnoc (" ++ label ++ ")"
>   in testGroup title
>     [ krebProp
>         "cons u (snoc v x) == snoc v (cons u x)" $
>         \(u :: a) (v :: a) (x :: t a) ->
>           claimEqual
>             (cons u (snoc v x))
>             (snoc v (cons u x))
>     ]
