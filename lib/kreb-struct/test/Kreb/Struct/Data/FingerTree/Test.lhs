---
title: Finger Trees -- Tests
---



> {-# LANGUAGE
>    FlexibleContexts, OverloadedStrings
> #-}
> 
> module Kreb.Struct.Data.FingerTree.Test (
>     test_FingerTree
> ) where
> 
> import Data.Proxy
> import Data.Foldable
> 
> import Test.Tasty
> 
> import qualified Kreb.Format as Fmt
> import           Kreb.Prop
> 
> import Kreb.Struct.Class
> import Kreb.Struct.Data.FingerTree





> test_FingerTree :: TestTree
> test_FingerTree =
>   testGroup "FingerTree"
>     [ testGroup "FingerTree Properties"
>       [ test_FingerTree_properties "Trivial Char"
>           (Proxy :: Proxy (Trivial Char))
>       , test_FingerTree_properties "Counted Char"
>           (Proxy :: Proxy (Counted Char))
>       , test_FingerTree_properties "(Counted Bool, Counted Bool)"
>           (Proxy :: Proxy (Counted Bool, Counted Bool))
>       ]
> 
>     , testGroup "Class Laws"
>       [ testGroup "Eq"
>         [ test_Eq_laws (Proxy :: Proxy (FingerTree (Counted Char)))
>         , test_Eq_laws (Proxy :: Proxy (FingerTree (Self [Bool])))
> 
>         , test_Eq_laws (Proxy :: Proxy (NonEmptyFingerTree (Counted Char)))
>         , test_Eq_laws (Proxy :: Proxy (NonEmptyFingerTree (Self [Bool])))
>         ]
> 
>       , testGroup "Semigroup"
>         [ test_Semigroup_laws (Proxy :: Proxy (FingerTree (Counted Char)))
>         , test_Semigroup_laws (Proxy :: Proxy (FingerTree (Self [Bool])))
> 
>         , test_Semigroup_laws (Proxy :: Proxy (NonEmptyFingerTree (Counted Char)))
>         , test_Semigroup_laws (Proxy :: Proxy (NonEmptyFingerTree (Self [Bool])))
>         ]
> 
>       , testGroup "Subsemigroup"
>         [ test_Subsemigroup_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Trivial Char))
>         , test_Subsemigroup_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Counted Char))
>         ]
> 
>       , testGroup "Ideal"
>         [ test_Ideal_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Trivial Char))
>         , test_Ideal_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Counted Char))
>         ]
> 
>       , testGroup "Monoid"
>         [ test_Monoid_laws (Proxy :: Proxy (FingerTree (Counted Char)))
>         , test_Monoid_laws (Proxy :: Proxy (FingerTree (Self [Bool])))
>         ]
> 
>       , testGroup "Subset"
>         [ test_Subset_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Trivial Char))
>         , test_Subset_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Counted Char))
>         ]
> 
>       , testGroup "NonEmpty"
>         [ test_NonEmpty_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Trivial Char))
>         , test_NonEmpty_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Counted Char))
>         ]
>       ]
> 
>       , testGroup "Singleton"
>         [ test_Singleton_laws "FingerTree" (Proxy :: Proxy FingerTree) (Proxy :: Proxy (Counted Char))
>         , test_Singleton_laws "FingerTree" (Proxy :: Proxy FingerTree) (Proxy :: Proxy (Self [Bool]))
> 
>         , test_Singleton_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Trivial Char))
>         , test_Singleton_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Counted Char))
>         , test_SubsetSingleton_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Trivial Char))
>         , test_SubsetSingleton_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Counted Char))
>         , test_NonEmptySingleton_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Trivial Char))
>         , test_NonEmptySingleton_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Counted Char))
>         ]
> 
>       , testGroup "Cons and Snoc"
>         [ test_Cons_laws "FingerTree" (Proxy :: Proxy FingerTree) (Proxy :: Proxy (Counted Char))
>         , test_Cons_laws "FingerTree" (Proxy :: Proxy FingerTree) (Proxy :: Proxy (Self [Bool]))
>         , test_SingletonCons_laws "FingerTree" (Proxy :: Proxy FingerTree) (Proxy :: Proxy (Counted Char))
>         , test_SingletonCons_laws "FingerTree" (Proxy :: Proxy FingerTree) (Proxy :: Proxy (Self [Bool]))
>         , test_Snoc_laws "FingerTree" (Proxy :: Proxy FingerTree) (Proxy :: Proxy (Counted Char))
>         , test_Snoc_laws "FingerTree" (Proxy :: Proxy FingerTree) (Proxy :: Proxy (Self [Bool]))
>         , test_SingletonSnoc_laws "FingerTree" (Proxy :: Proxy FingerTree) (Proxy :: Proxy (Counted Char))
>         , test_SingletonSnoc_laws "FingerTree" (Proxy :: Proxy FingerTree) (Proxy :: Proxy (Self [Bool]))
>         , test_ConsSnoc_laws "FingerTree" (Proxy :: Proxy FingerTree) (Proxy :: Proxy (Counted Char))
>         , test_ConsSnoc_laws "FingerTree" (Proxy :: Proxy FingerTree) (Proxy :: Proxy (Self [Bool]))
> 
>         , test_Cons_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Counted Char))
>         , test_Cons_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Self [Bool]))
>         , test_SingletonCons_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Counted Char))
>         , test_SingletonCons_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Self [Bool]))
>         , test_SubsetCons_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Counted Char))
>         , test_SubsetCons_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Self [Bool]))
>         , test_Snoc_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Counted Char))
>         , test_Snoc_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Self [Bool]))
>         , test_SingletonSnoc_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Counted Char))
>         , test_SingletonSnoc_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Self [Bool]))
>         , test_SubsetSnoc_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Counted Char))
>         , test_SubsetSnoc_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Self [Bool]))
>         , test_UnconsNonEmpty_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Counted Char))
>         , test_UnconsNonEmpty_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Self [Bool]))
>         , test_UnsnocNonEmpty_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Counted Char))
>         , test_UnsnocNonEmpty_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Self [Bool]))
>         , test_ConsSnoc_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Counted Char))
>         , test_ConsSnoc_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Self [Bool]))
>         ]
> 
>       , testGroup "Reverse"
>         [ test_Reverse_laws "FingerTree" (Proxy :: Proxy FingerTree) (Proxy :: Proxy (Counted Char))
>         , test_Reverse_laws "FingerTree" (Proxy :: Proxy FingerTree) (Proxy :: Proxy (Self [Bool]))
>         , test_ReverseSemigroup_laws "FingerTree" (Proxy :: Proxy FingerTree) (Proxy :: Proxy (Counted Char))
>         , test_ReverseSemigroup_laws "FingerTree" (Proxy :: Proxy FingerTree) (Proxy :: Proxy (Self [Bool]))
>         , test_ReverseMonoid_laws "FingerTree" (Proxy :: Proxy FingerTree) (Proxy :: Proxy (Counted Char))
>         , test_ReverseMonoid_laws "FingerTree" (Proxy :: Proxy FingerTree) (Proxy :: Proxy (Self [Bool]))
>         , test_ReverseSingleton_laws "FingerTree" (Proxy :: Proxy FingerTree) (Proxy :: Proxy (Counted Char))
>         , test_ReverseSingleton_laws "FingerTree" (Proxy :: Proxy FingerTree) (Proxy :: Proxy (Self [Bool]))
>         , test_ReverseConsSnoc_laws "FingerTree" (Proxy :: Proxy FingerTree) (Proxy :: Proxy (Counted Char))
>         , test_ReverseConsSnoc_laws "FingerTree" (Proxy :: Proxy FingerTree) (Proxy :: Proxy (Self [Bool]))
> 
>         , test_Reverse_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Counted Char))
>         , test_Reverse_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Self [Bool]))
>         , test_ReverseSemigroup_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Counted Char))
>         , test_ReverseSemigroup_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Self [Bool]))
>         , test_ReverseSubset_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Counted Char))
>         , test_ReverseSubset_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Self [Bool]))
>         , test_ReverseSingleton_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Counted Char))
>         , test_ReverseSingleton_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Self [Bool]))
>         , test_ReverseConsSnoc_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Counted Char))
>         , test_ReverseConsSnoc_laws "NonEmptyFingerTree" (Proxy :: Proxy NonEmptyFingerTree) (Proxy :: Proxy (Self [Bool]))
>         ]
> 
>       , testGroup "FromList"
>         [ test_FromList_laws "FingerTree" (Proxy :: Proxy FingerTree) (Proxy :: Proxy (Counted Char))
>         , test_FromList_laws "FingerTree" (Proxy :: Proxy FingerTree) (Proxy :: Proxy (Self [Bool]))
>         , test_FromListMonoid_laws "FingerTree" (Proxy :: Proxy FingerTree) (Proxy :: Proxy (Counted Char))
>         , test_FromListMonoid_laws "FingerTree" (Proxy :: Proxy FingerTree) (Proxy :: Proxy (Self [Bool]))
>         , test_FromListConsSnocReverse_laws "FingerTree" (Proxy :: Proxy FingerTree) (Proxy :: Proxy (Counted Char))
>         , test_FromListConsSnocReverse_laws "FingerTree" (Proxy :: Proxy FingerTree) (Proxy :: Proxy (Self [Bool]))
> 
>         , test_FromList_laws "NonEmptyFingerTree" (Proxy :: Proxy FingerTree) (Proxy :: Proxy (Counted Char))
>         , test_FromList_laws "NonEmptyFingerTree" (Proxy :: Proxy FingerTree) (Proxy :: Proxy (Self [Bool]))
>         , test_FromListMonoid_laws "NonEmptyFingerTree" (Proxy :: Proxy FingerTree) (Proxy :: Proxy (Counted Char))
>         , test_FromListMonoid_laws "NonEmptyFingerTree" (Proxy :: Proxy FingerTree) (Proxy :: Proxy (Self [Bool]))
>         , test_FromListConsSnocReverse_laws "NonEmptyFingerTree" (Proxy :: Proxy FingerTree) (Proxy :: Proxy (Counted Char))
>         , test_FromListConsSnocReverse_laws "NonEmptyFingerTree" (Proxy :: Proxy FingerTree) (Proxy :: Proxy (Self [Bool]))
>         ]
> 
>    {-   , testGroup "ConstrainedFunctor"
>         [ test_ConstrainedFunctor_laws (Proxy :: Proxy FingerTree)
>             (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool]))
>         , test_ConstrainedFunctor_laws (Proxy :: Proxy NonEmptyFingerTree)
>             (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool]))
>         ]
> 
>       , testGroup "Foldable"
>         [ test_Foldable_laws (Proxy :: Proxy FingerTree)
>             (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool]))
> 
>         , test_FoldableConstrainedFunctor_laws (Proxy :: Proxy FingerTree)
>             (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool]))
> 
>         , test_Foldable_laws (Proxy :: Proxy NonEmptyFingerTree)
>             (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool]))
> 
>         , test_FoldableConstrainedFunctor_laws (Proxy :: Proxy NonEmptyFingerTree)
>             (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool]))
>         ]
> 
>       , testGroup "ConstrainedTraversable"
>         [ test_ConstrainedTraversable_laws (Proxy :: Proxy FingerTree)
>             (Proxy :: Proxy Trivial) (Proxy :: Proxy Trivial)
>             (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool]))
> 
>         , test_ConstrainedTraversable_laws (Proxy :: Proxy FingerTree)
>             (Proxy :: Proxy Counted) (Proxy :: Proxy Counted)
>             (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool]))
> 
>         , test_ConstrainedTraversable_laws (Proxy :: Proxy NonEmptyFingerTree)
>             (Proxy :: Proxy Trivial) (Proxy :: Proxy Trivial)
>             (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool]))
> 
>         , test_ConstrainedTraversable_laws (Proxy :: Proxy NonEmptyFingerTree)
>             (Proxy :: Proxy Counted) (Proxy :: Proxy Counted)
>             (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool]))
>         ] -}
>     ]





> test_FingerTree_properties
>   :: forall m a
>    . ( Eq a, Fmt.Display a, Arb a, Prune a, MakeTo a, CoArb a, Valued a
>      , Fmt.Display (Value a), MakeTo (Value a), CoArb (Value a), Prune (Value a) )
>   => String -> Proxy a -> TestTree
> test_FingerTree_properties label _ =
>   let title = "FingerTree (" ++ label ++ ")"
>   in testGroup title
>   {-  [ krebProp
>         "validate a == True" $
>         \(a :: FingerTree a) ->
>           claimTrue (validate a) -}
> 
>     [ krebProp
>         "Just a == readFirst (singleton a)" $
>         \(a :: a) ->
>           let x = singleton a :: FingerTree a
>           in claimEqual (Just a) (readFirst x)
> 
>     , krebProp
>         "Just a == readLast (singleton a)" $
>         \(a :: a) ->
>           let x = singleton a :: FingerTree a
>           in claimEqual (Just a) (readLast x)
> 
>     , krebProp
>         "value as == foldMap value (toList as)" $
>         \(as :: FingerTree a) ->
>           claimEqual
>             (value as)
>             (foldMap (value :: a -> Value a) (toList as))
> 
> 
>     , krebProp
>         "left splitting property: concat" $
>         \(p :: Fun (Value a) Bool) (as :: FingerTree a) ->
>           claimAny
>             [ claimTrue (apFun p mempty)
>             , claimFalse (apFun p (value as))
>             , case splitL (apFun p) as of
>                 NotFound -> reject "expected successful left split"
>                 Found us x vs -> claimEqual as (us <> cons x vs)
>             ]
> 
>     , krebProp
>         "left splitting property: prefix" $
>         \(p :: Fun (Value a) Bool) (as :: FingerTree a) ->
>           claimAny
>             [ claimTrue (apFun p mempty)
>             , claimFalse (apFun p (value as))
>             , case splitL (apFun p) as of
>                 NotFound -> reject "expected successful left split"
>                 Found us _ _ -> claimFalse (apFun p (value us))
>             ]
> 
>     , krebProp
>         "left splitting property: found" $
>         \(p :: Fun (Value a) Bool) (as :: FingerTree a) ->
>           claimAny
>             [ claimTrue (apFun p mempty)
>             , claimFalse (apFun p (value as))
>             , case splitL (apFun p) as of
>                 NotFound -> reject "expected successful left split"
>                 Found us x _ -> claimTrue (apFun p (value us <> value x))
>             ]
> 
>     , krebProp
>         "left splitting property: exhaustive" $
>         \(p :: Fun (Value a) Bool) (as :: FingerTree a) ->
>           case splitL (apFun p) as of
>             Found _ _ _ -> accept
>             NotFound ->
>               let
>                 prefixes :: [a] -> [[a]]
>                 prefixes [] = [[]]
>                 prefixes (x:xs) = [] : map (x:) (prefixes xs)
> 
>                 splits :: [a] -> [([a], a)]
>                 splits xs = zip (prefixes xs) xs
>               in claimAll
>                 [ claimAny
>                   [ claimTrue (apFun p (mconcat $ map value ps))
>                   , claimFalse (apFun p (value x))
>                   ]
>                 | (ps, x) <- splits $ toList as
>                 ]
> 
>     , krebProp
>         "right splitting property: concat" $
>         \(p :: Fun (Value a) Bool) (as :: FingerTree a) ->
>           claimAny
>             [ claimTrue (apFun p mempty)
>             , claimFalse (apFun p (value as))
>             , case splitR (apFun p) as of
>                 NotFound -> reject "expected successful right split"
>                 Found us x vs -> claimEqual as (us <> cons x vs)
>             ]
> 
>     , krebProp
>         "right splitting property: prefix" $
>         \(p :: Fun (Value a) Bool) (as :: FingerTree a) ->
>           claimAny
>             [ claimTrue (apFun p mempty)
>             , claimFalse (apFun p (value as))
>             , case splitR (apFun p) as of
>                 NotFound -> reject "expected successful right split"
>                 Found _ _ vs -> claimFalse (apFun p (value vs))
>             ]
> 
>     , krebProp
>         "right splitting property: found" $
>         \(p :: Fun (Value a) Bool) (as :: FingerTree a) ->
>           claimAny
>             [ claimTrue (apFun p mempty)
>             , claimFalse (apFun p (value as))
>             , case splitR (apFun p) as of
>                 NotFound -> reject "expected successful right split"
>                 Found _ x vs -> claimTrue (apFun p (value x <> value vs))
>             ]
> 
>     , krebProp
>         "right splitting property: exhaustive" $
>         \(p :: Fun (Value a) Bool) (as :: FingerTree a) ->
>           case splitR (apFun p) as of
>             Found _ _ _ -> accept
>             NotFound ->
>               let
>                 suffixes :: [a] -> [[a]]
>                 suffixes [] = [[]]
>                 suffixes (x:xs) = (x:xs) : (suffixes xs)
> 
>                 splits :: [a] -> [([a], a)]
>                 splits xs = zip (tail $ suffixes xs) xs
>               in claimAll
>                 [ claimAny
>                   [ claimTrue (apFun p (mconcat $ map value ps))
>                   , claimFalse (apFun p (value x))
>                   ]
>                 | (ps, x) <- splits $ toList as
>                 ]
> 
>     , krebProp
>         "(isEmpty as) || (readFirst (as <> bs) == readFirst as)" $
>         \(as :: FingerTree a) (bs :: FingerTree a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (readFirst as)
>                 (readFirst (as <> bs))
>             ]
> 
>     , krebProp
>         "(isEmpty bs) || (readLast (as <> bs) == readLast bs)" $
>         \(as :: FingerTree a) (bs :: FingerTree a) ->
>           claimAny
>             [ claimTrue (isEmpty bs)
>             , claimEqual
>                 (readLast bs)
>                 (readLast (as <> bs))
>             ]
> 
>     , krebProp
>         "readFirst (mempty <> bs) == readFirst bs" $
>         \(bs :: FingerTree a) ->
>           claimEqual
>             (readFirst bs)
>             (readFirst (mempty <> bs))
> 
>     , krebProp
>         "readLast (as <> mempty) == readLast as" $
>         \(as :: FingerTree a) ->
>           claimEqual
>             (readLast as)
>             (readLast (as <> mempty))
>     ]

> {-

>     , krebProp
>         "inflateWith id (fmapC (:[]) as) == as" $
>         \(as :: FingerTree a) ->
>           claimEqual
>             (as)
>             (inflateWith id (fmapC (:[]) as :: FingerTree m [a]))
> 
>     , krebProp
>         "inflateWith id (singleton xs) == fromList xs" $
>         \(xs :: [a]) ->
>           let as = singleton xs :: FingerTree m [a]
>           in claimEqual
>             (inflateWith id as :: FingerTree a)
>             (fromList xs)

> -}
