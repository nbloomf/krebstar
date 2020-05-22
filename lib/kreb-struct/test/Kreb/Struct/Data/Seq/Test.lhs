> module Kreb.Struct.Data.Seq.Test (
>     test_Seq
> ) where

> import Data.Proxy

> import Test.Tasty

> import Kreb.Prop
> import Kreb.Control.Constrained

> import Kreb.Struct.Class
> import Kreb.Struct.Data.Seq



> test_Seq :: TestTree
> test_Seq =
>   testGroup "Seq"
>     [ testGroup "Class Laws"
>       [ testGroup "Eq"
>         [ test_Eq_laws (Proxy :: Proxy (Seq Char))
>         , test_Eq_laws (Proxy :: Proxy (Seq Integer))
> 
>         , test_Eq_laws (Proxy :: Proxy (NonEmptySeq Char))
>         , test_Eq_laws (Proxy :: Proxy (NonEmptySeq Integer))
>         ]
> 
>       , testGroup "Semigroup"
>         [ test_Semigroup_laws (Proxy :: Proxy (Seq Char))
>         , test_Semigroup_laws (Proxy :: Proxy (Seq Integer))
> 
>         , test_Semigroup_laws (Proxy :: Proxy (NonEmptySeq Char))
>         , test_Semigroup_laws (Proxy :: Proxy (NonEmptySeq Integer))
>         ]
> 
>       , testGroup "Subsemigroup"
>         [ test_Subsemigroup_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy [Int])
>         , test_Subsemigroup_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy Char)
>         ]
> 
>       , testGroup "Ideal"
>         [ test_Ideal_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy [Int])
>         , test_Ideal_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy Char)
>         ]
> 
>       , testGroup "Monoid"
>         [ test_Monoid_laws (Proxy :: Proxy (Seq Char))
>         , test_Monoid_laws (Proxy :: Proxy (Seq Integer))
>         ]
> 
>       , testGroup "Subset"
>         [ test_Subset_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy [Int])
>         , test_Subset_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy Char)
>         ]
> 
>       , testGroup "NonEmpty"
>         [ test_NonEmpty_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy [Int])
>         , test_NonEmpty_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy Char)
>         ]
>       ]
> 
>       , testGroup "Singleton"
>         [ test_Singleton_laws "Seq" (Proxy :: Proxy Seq) (Proxy :: Proxy Char)
>         , test_Singleton_laws "Seq" (Proxy :: Proxy Seq) (Proxy :: Proxy Integer)
> 
>         , test_Singleton_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy [Int])
>         , test_Singleton_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy Char)
>         , test_SubsetSingleton_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy [Int])
>         , test_SubsetSingleton_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy Char)
>         , test_NonEmptySingleton_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy [Int])
>         , test_NonEmptySingleton_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy Char)
>         ]
> 
>       , testGroup "Cons and Snoc"
>         [ test_Cons_laws "Seq" (Proxy :: Proxy Seq) (Proxy :: Proxy Char)
>         , test_Cons_laws "Seq" (Proxy :: Proxy Seq) (Proxy :: Proxy Integer)
>         , test_SingletonCons_laws "Seq" (Proxy :: Proxy Seq) (Proxy :: Proxy Char)
>         , test_SingletonCons_laws "Seq" (Proxy :: Proxy Seq) (Proxy :: Proxy Integer)
>         , test_Snoc_laws "Seq" (Proxy :: Proxy Seq) (Proxy :: Proxy Char)
>         , test_Snoc_laws "Seq" (Proxy :: Proxy Seq) (Proxy :: Proxy Integer)
>         , test_SingletonSnoc_laws "Seq" (Proxy :: Proxy Seq) (Proxy :: Proxy Char)
>         , test_SingletonSnoc_laws "Seq" (Proxy :: Proxy Seq) (Proxy :: Proxy Integer)
>         , test_ConsSnoc_laws "Seq" (Proxy :: Proxy Seq) (Proxy :: Proxy Char)
>         , test_ConsSnoc_laws "Seq" (Proxy :: Proxy Seq) (Proxy :: Proxy Integer)
> 
>         , test_Cons_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy Char)
>         , test_Cons_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy Integer)
>         , test_SingletonCons_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy Char)
>         , test_SingletonCons_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy Integer)
>         , test_SubsetCons_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy Char)
>         , test_SubsetCons_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy Integer)
>         , test_Snoc_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy Char)
>         , test_Snoc_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy Integer)
>         , test_SingletonSnoc_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy Char)
>         , test_SingletonSnoc_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy Integer)
>         , test_SubsetSnoc_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy Char)
>         , test_SubsetSnoc_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy Integer)
>         , test_UnconsNonEmpty_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy Char)
>         , test_UnconsNonEmpty_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy Integer)
>         , test_UnsnocNonEmpty_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy Char)
>         , test_UnsnocNonEmpty_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy Integer)
>         , test_ConsSnoc_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy Char)
>         , test_ConsSnoc_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy Integer)
>         ]
> 
>       , testGroup "Reverse"
>         [ test_Reverse_laws "Seq" (Proxy :: Proxy Seq) (Proxy :: Proxy Char)
>         , test_Reverse_laws "Seq" (Proxy :: Proxy Seq) (Proxy :: Proxy Integer)
>         , test_ReverseSemigroup_laws "Seq" (Proxy :: Proxy Seq) (Proxy :: Proxy Char)
>         , test_ReverseSemigroup_laws "Seq" (Proxy :: Proxy Seq) (Proxy :: Proxy Integer)
>         , test_ReverseMonoid_laws "Seq" (Proxy :: Proxy Seq) (Proxy :: Proxy Char)
>         , test_ReverseMonoid_laws "Seq" (Proxy :: Proxy Seq) (Proxy :: Proxy Integer)
>         , test_ReverseSingleton_laws "Seq" (Proxy :: Proxy Seq) (Proxy :: Proxy Char)
>         , test_ReverseSingleton_laws "Seq" (Proxy :: Proxy Seq) (Proxy :: Proxy Integer)
>         , test_ReverseConsSnoc_laws "Seq" (Proxy :: Proxy Seq) (Proxy :: Proxy Char)
>         , test_ReverseConsSnoc_laws "Seq" (Proxy :: Proxy Seq) (Proxy :: Proxy Integer)
> 
>         , test_Reverse_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy Char)
>         , test_Reverse_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy Integer)
>         , test_ReverseSemigroup_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy Char)
>         , test_ReverseSemigroup_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy Integer)
>         , test_ReverseSubset_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy Char)
>         , test_ReverseSubset_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy Integer)
>         , test_ReverseSingleton_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy Char)
>         , test_ReverseSingleton_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy Integer)
>         , test_ReverseConsSnoc_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy Char)
>         , test_ReverseConsSnoc_laws "NonEmptySeq" (Proxy :: Proxy NonEmptySeq) (Proxy :: Proxy Integer)
>         ]
> 
>       , testGroup "FromList"
>         [ test_FromList_laws "Seq" (Proxy :: Proxy Seq) (Proxy :: Proxy Char)
>         , test_FromList_laws "Seq" (Proxy :: Proxy Seq) (Proxy :: Proxy Integer)
>         , test_FromListMonoid_laws "Seq" (Proxy :: Proxy Seq) (Proxy :: Proxy Char)
>         , test_FromListMonoid_laws "Seq" (Proxy :: Proxy Seq) (Proxy :: Proxy Integer)
>         , test_FromListConsSnocReverse_laws "Seq" (Proxy :: Proxy Seq) (Proxy :: Proxy Char)
>         , test_FromListConsSnocReverse_laws "Seq" (Proxy :: Proxy Seq) (Proxy :: Proxy Integer)
> 
>         , test_FromList_laws "NonEmptySeq" (Proxy :: Proxy Seq) (Proxy :: Proxy Char)
>         , test_FromList_laws "NonEmptySeq" (Proxy :: Proxy Seq) (Proxy :: Proxy Integer)
>         , test_FromListMonoid_laws "NonEmptySeq" (Proxy :: Proxy Seq) (Proxy :: Proxy Char)
>         , test_FromListMonoid_laws "NonEmptySeq" (Proxy :: Proxy Seq) (Proxy :: Proxy Integer)
>         , test_FromListConsSnocReverse_laws "NonEmptySeq" (Proxy :: Proxy Seq) (Proxy :: Proxy Char)
>         , test_FromListConsSnocReverse_laws "NonEmptySeq" (Proxy :: Proxy Seq) (Proxy :: Proxy Integer)
>         ]
> 
>       , testGroup "Functor"
>         [ test_Functor_laws (Proxy :: Proxy Seq)
>             (Proxy :: Proxy Integer) (Proxy :: Proxy Integer) (Proxy :: Proxy Integer)
>         , test_Functor_laws (Proxy :: Proxy NonEmptySeq)
>             (Proxy :: Proxy Integer) (Proxy :: Proxy Integer) (Proxy :: Proxy Integer)
>         ]
> 
>       , testGroup "Foldable"
>         [ test_Foldable_laws (Proxy :: Proxy Seq)
>             (Proxy :: Proxy Integer) (Proxy :: Proxy Integer) (Proxy :: Proxy [Bool])
> 
>         , test_FoldableFunctor_laws (Proxy :: Proxy Seq)
>             (Proxy :: Proxy Integer) (Proxy :: Proxy Integer) (Proxy :: Proxy [Bool])
> 
>         , test_Foldable_laws (Proxy :: Proxy NonEmptySeq)
>             (Proxy :: Proxy Integer) (Proxy :: Proxy Integer) (Proxy :: Proxy [Bool])
> 
>         , test_FoldableFunctor_laws (Proxy :: Proxy NonEmptySeq)
>             (Proxy :: Proxy Integer) (Proxy :: Proxy Integer) (Proxy :: Proxy [Bool])
>         ]
>     ]
