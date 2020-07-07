> {-# LANGUAGE FlexibleContexts #-}

> module Kreb.Struct.Class.Container.Test where

> import Prelude hiding (reverse)
> import qualified Prelude as Prelude (reverse)
> import Data.Proxy
> import Data.Foldable

> import Test.Tasty

> import qualified Kreb.Format as Fmt
> import Kreb.Prop

> import Kreb.Struct.Class.Container

> test_Subset_laws
>   :: forall t a
>    . ( Subset t, ElementOf t a
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



> test_NonEmpty_laws
>   :: forall t a
>    . ( NonEmpty t, ElementOf t a
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



> test_Singleton_laws
>   :: forall t a
>    . ( Singleton t, ElementOf t a
>      , Eq a, Fmt.Display a, Arb a, Prune a
>      , Eq (t a), Fmt.Display (t a), Arb (t a), Prune (t a) )
>   => String -> Proxy (t :: * -> *) -> Proxy (a :: *) -> TestTree
> test_Singleton_laws label _ _ =
>   let title = "Singleton (" ++ label ++ ")"
>   in testGroup title
>     [ krebProp
>         "isSingleton (singleton a) == True" $
>         \(a :: a) ->
>           claimTrue (isSingleton (singleton a :: t a))
> 
>     , krebProp
>         "fromSingleton (singleton a) == Just a" $
>         \(a :: a) ->
>           claimEqual
>             (fromSingleton (singleton a :: t a))
>             (Just a)
> 
>     , krebProp
>         "(fromSingleton x == Just a) == (x == singleton a)" $
>         \(a :: a) (x :: t a) ->
>           claimEqual
>             (fromSingleton x == Just a)
>             (x == singleton a)
>     ]

> test_SubsetSingleton_laws
>   :: forall t a
>    . ( SubsetSingleton t, ElementOf t a, ElementOf (SupersetOf t) a
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
>    . ( NonEmptySingleton t, ElementOf t a, ElementOf (SupersetOf t) a
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



> test_Subsemigroup_laws
>   :: forall a t
>    . ( Subsemigroup t, ElementOf t a, ElementOf (SupersetOf t) a
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
>    . ( Ideal t, ElementOf t a, ElementOf (SupersetOf t) a
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
> 
>     , krebProp
>         "(u <> v) @> w == u <> (v @> w)" $
>         \(u :: t a) (v :: t a) (w :: SupersetOf t a) ->
>           claimEqual
>             ((u <> v) @> w)
>             (u <> (v @> w))
> 
>     , krebProp
>         "u <@ (v <> w) == (u <@ v) <> w" $
>         \(u :: SupersetOf t a) (v :: t a) (w :: t a) ->
>           claimEqual
>             (u <@ (v <> w))
>             ((u <@ v) <> w)
> 
>     , krebProp
>         "u <@ (v @> w) == (u <@ v) @> w" $
>         \(u :: SupersetOf t a) (v :: t a) (w :: SupersetOf t a) ->
>           claimEqual
>             (u <@ (v @> w))
>             ((u <@ v) @> w)
> 
>     , krebProp
>         "u <> (v <@ w) == (u @> v) <> w" $
>         \(u :: t a) (v :: SupersetOf t a) (w :: t a) ->
>           claimEqual
>             (u <> (v <@ w))
>             ((u @> v) <> w)
>     ]

> test_Cons_laws
>   :: forall t a
>    . ( Cons t, ElementOf t a
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
>         "(uncons w == Just (a, x)) == (cons a x == w)" $
>         \(a :: a) (x :: t a) (w :: t a) ->
>           claimEqual
>             (uncons w == Just (a, x))
>             (cons a x == w)
>     ]

> test_SingletonCons_laws
>   :: forall t a
>    . ( SingletonCons t, ElementOf t a
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
>    . ( SubsetCons t, ElementOf t a, ElementOf (SupersetOf t) a
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
>    . ( UnconsNonEmpty t, ElementOf t a, ElementOf (SupersetOf t) a
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
>    . ( Snoc t, ElementOf t a
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
>         "unsnoc (snoc a x) == Just (a, x)" $
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
>    . ( SingletonSnoc t, ElementOf t a
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
>    . ( SubsetSnoc t, ElementOf t a, ElementOf (SupersetOf t) a
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
>    . ( UnsnocNonEmpty t, ElementOf t a, ElementOf (SupersetOf t) a
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
>    . ( ConsSnoc t, ElementOf t a
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

> test_Reverse_laws
>   :: forall a t
>    . ( Reverse t, ElementOf t a
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
>    . ( ReverseSemigroup t, ElementOf t a
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
>    . ( ReverseMonoid t, ElementOf t a, Eq (t a), Fmt.Display (t a) )
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
>    . ( ReverseSubset t, ElementOf t a, ElementOf (SupersetOf t) a
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
>    . ( ReverseSingleton t, ElementOf t a
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
>    . ( ReverseConsSnoc t, ElementOf t a
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


> test_FromList_laws
>   :: forall t a
>    . ( FromList t, ElementOf t a
>      , Eq a, Fmt.Display a
>      , Eq (t a), Fmt.Display (t a), Arb (t a), Prune (t a) )
>   => String -> Proxy (t :: * -> *) -> Proxy (a :: *) -> TestTree
> test_FromList_laws label _ _ =
>   let title = "FromList (" ++ label ++ ")"
>   in testGroup title
>     [ krebProp
>         "fromListMaybe . toList == Just" $
>         \(x :: t a) ->
>           claimEqual (Just x) (fromListMaybe (toList x))
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
>    . ( FromListMonoid t, ElementOf t a
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
>    . ( FromListConsSnocReverse t, ElementOf t a
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
>             (cons a (toList x))
> 
>     , krebProp
>         "toList (snoc a x) == (toList x) ++ [a]" $
>         \(a :: a) (x :: t a) ->
>           claimEqual
>             (toList (snoc a x))
>             (snoc a (toList x))
> 
>     , krebProp
>         "fromListMaybe (a : as) == fmap $ cons a (fromListMaybe as)" $
>         \(a :: a) (as :: [a]) ->
>           claimEqual
>             (fromListMaybe (cons a as) :: Maybe (t a))
>             (fmap (cons a) (fromListMaybe as))
> 
>     , krebProp
>         "fromList (as ++ [a]) == snoc a (fromList as)" $
>         \(a :: a) (as :: [a]) ->
>           claimEqual
>             (fromListMaybe (snoc a as) :: Maybe (t a))
>             (fmap (snoc a) (fromListMaybe as))
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
>             (fromListMaybe (Prelude.reverse x))
>             (fmap reverse (fromListMaybe x :: Maybe (t a)))
>     ]

> {-

> test_Zipper_laws
>   :: forall t a
>    . ( Zipper t, ElementOf t a
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





> test_LinearZipper_laws
>   :: forall t a
>    . ( LinearZipper t, ElementOf t a
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

> -}
