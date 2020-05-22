> module Kreb.Struct.Data.Deque.Test (
>     test_Deque
> ) where

> import Data.Proxy

> import Test.Tasty

> import Kreb.Prop
> import Kreb.Control

> import Kreb.Struct.Class
> import Kreb.Struct.Data.Deque



> test_Deque :: TestTree
> test_Deque =
>   testGroup "Deque"
>     [ testGroup "Class Laws"
>       [ testGroup "Eq"
>         [ test_Eq_laws (Proxy :: Proxy (Deque Char))
>         , test_Eq_laws (Proxy :: Proxy (Deque Integer))
> 
>         , test_Eq_laws (Proxy :: Proxy (NonEmptyDeque Char))
>         , test_Eq_laws (Proxy :: Proxy (NonEmptyDeque Integer))
>         ]
> 
>       , testGroup "Semigroup"
>         [ test_Semigroup_laws (Proxy :: Proxy (Deque Char))
>         , test_Semigroup_laws (Proxy :: Proxy (Deque Integer))
> 
>         , test_Semigroup_laws (Proxy :: Proxy (NonEmptyDeque Char))
>         , test_Semigroup_laws (Proxy :: Proxy (NonEmptyDeque Integer))
>         ]
> 
>       , testGroup "Subsemigroup"
>         [ test_Subsemigroup_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy [Int])
>         , test_Subsemigroup_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy Char)
>         ]
> 
>       , testGroup "Ideal"
>         [ test_Ideal_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy [Int])
>         , test_Ideal_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy Char)
>         ]
> 
>       , testGroup "Monoid"
>         [ test_Monoid_laws (Proxy :: Proxy (Deque Char))
>         , test_Monoid_laws (Proxy :: Proxy (Deque Integer))
>         ]
> 
>       , testGroup "Subset"
>         [ test_Subset_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy [Int])
>         , test_Subset_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy Char)
>         ]
> 
>       , testGroup "NonEmpty"
>         [ test_NonEmpty_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy [Int])
>         , test_NonEmpty_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy Char)
>         ]
>       ]
> 
>       , testGroup "Singleton"
>         [ test_Singleton_laws "Deque" (Proxy :: Proxy Deque) (Proxy :: Proxy Char)
>         , test_Singleton_laws "Deque" (Proxy :: Proxy Deque) (Proxy :: Proxy Integer)
> 
>         , test_Singleton_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy [Int])
>         , test_Singleton_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy Char)
>         , test_SubsetSingleton_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy [Int])
>         , test_SubsetSingleton_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy Char)
>         , test_NonEmptySingleton_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy [Int])
>         , test_NonEmptySingleton_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy Char)
>         ]
> 
>       , testGroup "Cons and Snoc"
>         [ test_Cons_laws "Deque" (Proxy :: Proxy Deque) (Proxy :: Proxy Char)
>         , test_Cons_laws "Deque" (Proxy :: Proxy Deque) (Proxy :: Proxy Integer)
>         , test_SingletonCons_laws "Deque" (Proxy :: Proxy Deque) (Proxy :: Proxy Char)
>         , test_SingletonCons_laws "Deque" (Proxy :: Proxy Deque) (Proxy :: Proxy Integer)
>         , test_Snoc_laws "Deque" (Proxy :: Proxy Deque) (Proxy :: Proxy Char)
>         , test_Snoc_laws "Deque" (Proxy :: Proxy Deque) (Proxy :: Proxy Integer)
>         , test_SingletonSnoc_laws "Deque" (Proxy :: Proxy Deque) (Proxy :: Proxy Char)
>         , test_SingletonSnoc_laws "Deque" (Proxy :: Proxy Deque) (Proxy :: Proxy Integer)
>         , test_ConsSnoc_laws "Deque" (Proxy :: Proxy Deque) (Proxy :: Proxy Char)
>         , test_ConsSnoc_laws "Deque" (Proxy :: Proxy Deque) (Proxy :: Proxy Integer)
> 
>         , test_Cons_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy Char)
>         , test_Cons_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy Integer)
>         , test_SingletonCons_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy Char)
>         , test_SingletonCons_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy Integer)
>         , test_SubsetCons_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy Char)
>         , test_SubsetCons_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy Integer)
>         , test_Snoc_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy Char)
>         , test_Snoc_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy Integer)
>         , test_SingletonSnoc_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy Char)
>         , test_SingletonSnoc_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy Integer)
>         , test_SubsetSnoc_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy Char)
>         , test_SubsetSnoc_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy Integer)
>         , test_UnconsNonEmpty_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy Char)
>         , test_UnconsNonEmpty_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy Integer)
>         , test_UnsnocNonEmpty_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy Char)
>         , test_UnsnocNonEmpty_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy Integer)
>         , test_ConsSnoc_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy Char)
>         , test_ConsSnoc_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy Integer)
>         ]
> 
>       , testGroup "Reverse"
>         [ test_Reverse_laws "Deque" (Proxy :: Proxy Deque) (Proxy :: Proxy Char)
>         , test_Reverse_laws "Deque" (Proxy :: Proxy Deque) (Proxy :: Proxy Integer)
>         , test_ReverseSemigroup_laws "Deque" (Proxy :: Proxy Deque) (Proxy :: Proxy Char)
>         , test_ReverseSemigroup_laws "Deque" (Proxy :: Proxy Deque) (Proxy :: Proxy Integer)
>         , test_ReverseMonoid_laws "Deque" (Proxy :: Proxy Deque) (Proxy :: Proxy Char)
>         , test_ReverseMonoid_laws "Deque" (Proxy :: Proxy Deque) (Proxy :: Proxy Integer)
>         , test_ReverseSingleton_laws "Deque" (Proxy :: Proxy Deque) (Proxy :: Proxy Char)
>         , test_ReverseSingleton_laws "Deque" (Proxy :: Proxy Deque) (Proxy :: Proxy Integer)
>         , test_ReverseConsSnoc_laws "Deque" (Proxy :: Proxy Deque) (Proxy :: Proxy Char)
>         , test_ReverseConsSnoc_laws "Deque" (Proxy :: Proxy Deque) (Proxy :: Proxy Integer)
> 
>         , test_Reverse_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy Char)
>         , test_Reverse_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy Integer)
>         , test_ReverseSemigroup_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy Char)
>         , test_ReverseSemigroup_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy Integer)
>         , test_ReverseSubset_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy Char)
>         , test_ReverseSubset_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy Integer)
>         , test_ReverseSingleton_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy Char)
>         , test_ReverseSingleton_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy Integer)
>         , test_ReverseConsSnoc_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy Char)
>         , test_ReverseConsSnoc_laws "NonEmptyDeque" (Proxy :: Proxy NonEmptyDeque) (Proxy :: Proxy Integer)
>         ]
> 
>       , testGroup "FromList"
>         [ test_FromList_laws "Deque" (Proxy :: Proxy Deque) (Proxy :: Proxy Char)
>         , test_FromList_laws "Deque" (Proxy :: Proxy Deque) (Proxy :: Proxy Integer)
>         , test_FromListMonoid_laws "Deque" (Proxy :: Proxy Deque) (Proxy :: Proxy Char)
>         , test_FromListMonoid_laws "Deque" (Proxy :: Proxy Deque) (Proxy :: Proxy Integer)
>         , test_FromListConsSnocReverse_laws "Deque" (Proxy :: Proxy Deque) (Proxy :: Proxy Char)
>         , test_FromListConsSnocReverse_laws "Deque" (Proxy :: Proxy Deque) (Proxy :: Proxy Integer)
> 
>         , test_FromList_laws "NonEmptyDeque" (Proxy :: Proxy Deque) (Proxy :: Proxy Char)
>         , test_FromList_laws "NonEmptyDeque" (Proxy :: Proxy Deque) (Proxy :: Proxy Integer)
>         , test_FromListMonoid_laws "NonEmptyDeque" (Proxy :: Proxy Deque) (Proxy :: Proxy Char)
>         , test_FromListMonoid_laws "NonEmptyDeque" (Proxy :: Proxy Deque) (Proxy :: Proxy Integer)
>         , test_FromListConsSnocReverse_laws "NonEmptyDeque" (Proxy :: Proxy Deque) (Proxy :: Proxy Char)
>         , test_FromListConsSnocReverse_laws "NonEmptyDeque" (Proxy :: Proxy Deque) (Proxy :: Proxy Integer)
>         ]
> 
>       , testGroup "Functor"
>         [ test_Functor_laws (Proxy :: Proxy Deque)
>             (Proxy :: Proxy Integer) (Proxy :: Proxy Integer) (Proxy :: Proxy Integer)
>         , test_Functor_laws (Proxy :: Proxy NonEmptyDeque)
>             (Proxy :: Proxy Integer) (Proxy :: Proxy Integer) (Proxy :: Proxy Integer)
>         ]
> 
>       , testGroup "Foldable"
>         [ test_Foldable_laws (Proxy :: Proxy Deque)
>             (Proxy :: Proxy Integer) (Proxy :: Proxy Integer) (Proxy :: Proxy [Bool])
> 
>         , test_FoldableFunctor_laws (Proxy :: Proxy Deque)
>             (Proxy :: Proxy Integer) (Proxy :: Proxy Integer) (Proxy :: Proxy [Bool])
> 
>         , test_Foldable_laws (Proxy :: Proxy NonEmptyDeque)
>             (Proxy :: Proxy Integer) (Proxy :: Proxy Integer) (Proxy :: Proxy [Bool])
> 
>         , test_FoldableFunctor_laws (Proxy :: Proxy NonEmptyDeque)
>             (Proxy :: Proxy Integer) (Proxy :: Proxy Integer) (Proxy :: Proxy [Bool])
>         ]
>     ]
