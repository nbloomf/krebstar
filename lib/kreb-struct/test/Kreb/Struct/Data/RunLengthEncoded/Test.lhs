---
title: Kreb.Struct.Data.RunLengthEncoded.Test
---

> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE FlexibleContexts #-}
> 
> module Kreb.Struct.Data.RunLengthEncoded.Test (
>     test_RunLengthEncoded
> ) where
> 
> import Data.Proxy
> import Data.Foldable
> import Data.Monoid
> 
> import Test.Tasty
> 
> import Kreb.Prop
> import Kreb.Struct.Class
> import Kreb.Struct.Data.RunLengthEncoded



> test_RunLengthEncoded :: TestTree
> test_RunLengthEncoded =
>   testGroup "RunLengthEncoded"
>     [ testGroup "Class Laws"
>       [ testGroup "Semigroup"
>         [ test_Semigroup_laws (Proxy :: Proxy RunSize)
> 
>         , test_Semigroup_laws (Proxy :: Proxy (RunLengthEncoded Bool))
>         , test_Semigroup_laws (Proxy :: Proxy (RunLengthEncoded Int))
> 
>         , test_Semigroup_laws (Proxy :: Proxy (NonEmptyRunLengthEncoded Bool))
>         , test_Semigroup_laws (Proxy :: Proxy (NonEmptyRunLengthEncoded Int))
>         ]
> 
>       , testGroup "Subsemigroup"
>         [ test_Subsemigroup_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Trivial Char))
>         , test_Subsemigroup_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Counted Char))
>         ]
> 
>       , testGroup "Ideal"
>         [ test_Ideal_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Trivial Char))
>         , test_Ideal_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Counted Char))
>         ]
> 
>       , testGroup "Monoid"
>         [ test_Monoid_laws (Proxy :: Proxy RunSize)
> 
>         , test_Monoid_laws (Proxy :: Proxy (RunLengthEncoded Bool))
>         , test_Monoid_laws (Proxy :: Proxy (RunLengthEncoded Int))
>         ]
> 
>       , testGroup "Subset"
>         [ test_Subset_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Trivial Char))
>         , test_Subset_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Counted Char))
>         ]
> 
>       , testGroup "NonEmpty"
>         [ test_NonEmpty_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Trivial Char))
>         , test_NonEmpty_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Counted Char))
>         ]
>       ]
> 
>       , testGroup "Singleton"
>         [ test_Singleton_laws "RunLengthEncoded" (Proxy :: Proxy RunLengthEncoded) (Proxy :: Proxy (Counted Char))
>         , test_Singleton_laws "RunLengthEncoded" (Proxy :: Proxy RunLengthEncoded) (Proxy :: Proxy (Self [Bool]))
> 
>         , test_Singleton_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Trivial Char))
>         , test_Singleton_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Counted Char))
>         , test_SubsetSingleton_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Trivial Char))
>         , test_SubsetSingleton_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Counted Char))
>         , test_NonEmptySingleton_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Trivial Char))
>         , test_NonEmptySingleton_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Counted Char))
>         ]
> 
>       , testGroup "Cons and Snoc"
>         [ test_Cons_laws "RunLengthEncoded" (Proxy :: Proxy RunLengthEncoded) (Proxy :: Proxy (Counted Char))
>         , test_Cons_laws "RunLengthEncoded" (Proxy :: Proxy RunLengthEncoded) (Proxy :: Proxy (Self [Bool]))
>         , test_SingletonCons_laws "RunLengthEncoded" (Proxy :: Proxy RunLengthEncoded) (Proxy :: Proxy (Counted Char))
>         , test_SingletonCons_laws "RunLengthEncoded" (Proxy :: Proxy RunLengthEncoded) (Proxy :: Proxy (Self [Bool]))
>         , test_Snoc_laws "RunLengthEncoded" (Proxy :: Proxy RunLengthEncoded) (Proxy :: Proxy (Counted Char))
>         , test_Snoc_laws "RunLengthEncoded" (Proxy :: Proxy RunLengthEncoded) (Proxy :: Proxy (Self [Bool]))
>         , test_SingletonSnoc_laws "RunLengthEncoded" (Proxy :: Proxy RunLengthEncoded) (Proxy :: Proxy (Counted Char))
>         , test_SingletonSnoc_laws "RunLengthEncoded" (Proxy :: Proxy RunLengthEncoded) (Proxy :: Proxy (Self [Bool]))
> 
>         , test_Cons_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Counted Char))
>         , test_Cons_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Self [Bool]))
>         , test_SingletonCons_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Counted Char))
>         , test_SingletonCons_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Self [Bool]))
>         , test_SubsetCons_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Counted Char))
>         , test_SubsetCons_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Self [Bool]))
>         , test_Snoc_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Counted Char))
>         , test_Snoc_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Self [Bool]))
>         , test_SingletonSnoc_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Counted Char))
>         , test_SingletonSnoc_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Self [Bool]))
>         , test_SubsetSnoc_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Counted Char))
>         , test_SubsetSnoc_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Self [Bool]))
>         , test_UnconsNonEmpty_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Counted Char))
>         , test_UnconsNonEmpty_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Self [Bool]))
>         , test_UnsnocNonEmpty_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Counted Char))
>         , test_UnsnocNonEmpty_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Self [Bool]))
>         ]
> 
>       , testGroup "Reverse"
>         [ test_Reverse_laws "RunLengthEncoded" (Proxy :: Proxy RunLengthEncoded) (Proxy :: Proxy (Counted Char))
>         , test_Reverse_laws "RunLengthEncoded" (Proxy :: Proxy RunLengthEncoded) (Proxy :: Proxy (Self [Bool]))
>         , test_ReverseSemigroup_laws "RunLengthEncoded" (Proxy :: Proxy RunLengthEncoded) (Proxy :: Proxy (Counted Char))
>         , test_ReverseSemigroup_laws "RunLengthEncoded" (Proxy :: Proxy RunLengthEncoded) (Proxy :: Proxy (Self [Bool]))
>         , test_ReverseMonoid_laws "RunLengthEncoded" (Proxy :: Proxy RunLengthEncoded) (Proxy :: Proxy (Counted Char))
>         , test_ReverseMonoid_laws "RunLengthEncoded" (Proxy :: Proxy RunLengthEncoded) (Proxy :: Proxy (Self [Bool]))
>         , test_ReverseSingleton_laws "RunLengthEncoded" (Proxy :: Proxy RunLengthEncoded) (Proxy :: Proxy (Counted Char))
>         , test_ReverseSingleton_laws "RunLengthEncoded" (Proxy :: Proxy RunLengthEncoded) (Proxy :: Proxy (Self [Bool]))
>         , test_ReverseConsSnoc_laws "RunLengthEncoded" (Proxy :: Proxy RunLengthEncoded) (Proxy :: Proxy (Counted Char))
>         , test_ReverseConsSnoc_laws "RunLengthEncoded" (Proxy :: Proxy RunLengthEncoded) (Proxy :: Proxy (Self [Bool]))
> 
>         , test_Reverse_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Counted Char))
>         , test_Reverse_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Self [Bool]))
>         , test_ReverseSemigroup_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Counted Char))
>         , test_ReverseSemigroup_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Self [Bool]))
>         , test_ReverseSubset_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Counted Char))
>         , test_ReverseSubset_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Self [Bool]))
>         , test_ReverseSingleton_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Counted Char))
>         , test_ReverseSingleton_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Self [Bool]))
>         , test_ReverseConsSnoc_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Counted Char))
>         , test_ReverseConsSnoc_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy NonEmptyRunLengthEncoded) (Proxy :: Proxy (Self [Bool]))
>         ]
> 
>       , testGroup "FromList"
>         [ test_FromList_laws "RunLengthEncoded" (Proxy :: Proxy RunLengthEncoded) (Proxy :: Proxy (Counted Char))
>         , test_FromList_laws "RunLengthEncoded" (Proxy :: Proxy RunLengthEncoded) (Proxy :: Proxy (Self [Bool]))
>         , test_FromListMonoid_laws "RunLengthEncoded" (Proxy :: Proxy RunLengthEncoded) (Proxy :: Proxy (Counted Char))
>         , test_FromListMonoid_laws "RunLengthEncoded" (Proxy :: Proxy RunLengthEncoded) (Proxy :: Proxy (Self [Bool]))
>         , test_FromListConsSnocReverse_laws "RunLengthEncoded" (Proxy :: Proxy RunLengthEncoded) (Proxy :: Proxy (Counted Char))
>         , test_FromListConsSnocReverse_laws "RunLengthEncoded" (Proxy :: Proxy RunLengthEncoded) (Proxy :: Proxy (Self [Bool]))
> 
>         , test_FromList_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy RunLengthEncoded) (Proxy :: Proxy (Counted Char))
>         , test_FromList_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy RunLengthEncoded) (Proxy :: Proxy (Self [Bool]))
>         , test_FromListMonoid_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy RunLengthEncoded) (Proxy :: Proxy (Counted Char))
>         , test_FromListMonoid_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy RunLengthEncoded) (Proxy :: Proxy (Self [Bool]))
>         , test_FromListConsSnocReverse_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy RunLengthEncoded) (Proxy :: Proxy (Counted Char))
>         , test_FromListConsSnocReverse_laws "NonEmptyRunLengthEncoded" (Proxy :: Proxy RunLengthEncoded) (Proxy :: Proxy (Self [Bool]))
>         ]
> 
>       , testGroup "Functor"
>         [ test_Functor_laws (Proxy :: Proxy Run)
>             (Proxy :: Proxy Int) (Proxy :: Proxy Int) (Proxy :: Proxy Int)
> 
>         , test_Functor_laws (Proxy :: Proxy RunLengthEncoded)
>             (Proxy :: Proxy Int) (Proxy :: Proxy Int) (Proxy :: Proxy Int)
>         , test_Functor_laws (Proxy :: Proxy RunLengthEncoded)
>             (Proxy :: Proxy Char) (Proxy :: Proxy Char) (Proxy :: Proxy Char)
> 
>         , test_Functor_laws (Proxy :: Proxy NonEmptyRunLengthEncoded)
>             (Proxy :: Proxy Int) (Proxy :: Proxy Int) (Proxy :: Proxy Int)
>         , test_Functor_laws (Proxy :: Proxy NonEmptyRunLengthEncoded)
>             (Proxy :: Proxy Char) (Proxy :: Proxy Char) (Proxy :: Proxy Char)
>         ]
> 
>       , testGroup "Foldable laws"
>         [ test_Foldable_laws (Proxy :: Proxy (RunLengthEncoded))
>             (Proxy :: Proxy Int) (Proxy :: Proxy Int) (Proxy :: Proxy (Sum Int))
>         , test_FoldableFunctor_laws (Proxy :: Proxy (RunLengthEncoded))
>             (Proxy :: Proxy Int) (Proxy :: Proxy Int) (Proxy :: Proxy (Sum Int))
> 
>         , test_Foldable_laws (Proxy :: Proxy (NonEmptyRunLengthEncoded))
>             (Proxy :: Proxy Int) (Proxy :: Proxy Int) (Proxy :: Proxy (Sum Int))
>         , test_FoldableFunctor_laws (Proxy :: Proxy (NonEmptyRunLengthEncoded))
>             (Proxy :: Proxy Int) (Proxy :: Proxy Int) (Proxy :: Proxy (Sum Int))
>         ]
>     ]

> {-


> 
>     , testGroup "Properties"
>       [ test_RunLengthEncoded_properties
>           "Char" (Proxy :: Proxy Char)
>       , test_RunLengthEncoded_properties
>           "Int" (Proxy :: Proxy Int)
>       ]
>     ]



> test_RunLengthEncoded_properties
>   :: forall a
>    . ( Eq a, Show a, Arb a, Prune a, MakeTo a, CoArb a )
>   => String -> Proxy a -> TestTree
> test_RunLengthEncoded_properties label _ =
>   let title = "RunLengthEncoded properties (" ++ label ++ ")"
>   in testGroup title
>     [ krebProp
>         "fromRuns (toRuns as) == as" $
>         \(as :: RunLengthEncoded a) ->
>           claimEqual
>             as
>             (fromRuns (toRuns as))
> 
> 
> 
> 
>     , krebProp
>         "cons a (snoc b as) == snoc b (cons a as)" $
>         \(a :: a) (b :: a) (as :: RunLengthEncoded a) ->
>           claimEqual
>             (cons a (snoc b as))
>             (snoc b (cons a as))
>     ]

> -}
