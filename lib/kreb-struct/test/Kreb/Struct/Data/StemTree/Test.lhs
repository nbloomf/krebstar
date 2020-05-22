---
title: Rose Trees -- Testing
---

> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE FlexibleContexts #-}
> 
> module Kreb.Struct.Data.StemTree.Test (
>     test_StemTree
> ) where
> 
> import Prelude hiding (reverse)
> import Data.Proxy
> import Data.Foldable
> 
> import Test.Tasty
> 
> import qualified Kreb.Format as Fmt
> import Kreb.Prop
> import Kreb.Struct.Class
> import Kreb.Struct.Data.StemTree

> test_StemTree :: TestTree
> test_StemTree = testGroup "RoseTree"
>   [ testGroup "Class Laws"
>     [ testGroup "Subset"
>       [ test_Subset_laws "NonEmptyStemTree" (Proxy :: Proxy NonEmptyStemTree) (Proxy :: Proxy [Int])
>       , test_Subset_laws "NonEmptyStemTree" (Proxy :: Proxy NonEmptyStemTree) (Proxy :: Proxy Char)
>       ]
> 
>     , testGroup "NonEmpty"
>       [ test_NonEmpty_laws "NonEmptyStemTree" (Proxy :: Proxy NonEmptyStemTree) (Proxy :: Proxy [Int])
>       , test_NonEmpty_laws "NonEmptyStemTree" (Proxy :: Proxy NonEmptyStemTree) (Proxy :: Proxy Char)
>       ]
> 
>     , testGroup "Singleton"
>       [ test_Singleton_laws "StemTree" (Proxy :: Proxy StemTree) (Proxy :: Proxy Char)
>       , test_Singleton_laws "StemTree" (Proxy :: Proxy StemTree) (Proxy :: Proxy Integer)
> 
>       , test_Singleton_laws "NonEmptyStemTree" (Proxy :: Proxy NonEmptyStemTree) (Proxy :: Proxy [Int])
>       , test_Singleton_laws "NonEmptyStemTree" (Proxy :: Proxy NonEmptyStemTree) (Proxy :: Proxy Char)
>       , test_SubsetSingleton_laws "NonEmptyStemTree" (Proxy :: Proxy NonEmptyStemTree) (Proxy :: Proxy [Int])
>       , test_SubsetSingleton_laws "NonEmptyStemTree" (Proxy :: Proxy NonEmptyStemTree) (Proxy :: Proxy Char)
>       , test_NonEmptySingleton_laws "NonEmptyStemTree" (Proxy :: Proxy NonEmptyStemTree) (Proxy :: Proxy [Int])
>       , test_NonEmptySingleton_laws "NonEmptyStemTree" (Proxy :: Proxy NonEmptyStemTree) (Proxy :: Proxy Char)
>       ]

>     , testGroup "Functor"
>       [ test_Functor_laws (Proxy :: Proxy StemTree)
>           (Proxy :: Proxy Integer) (Proxy :: Proxy Integer) (Proxy :: Proxy Integer)
>       , test_Functor_laws (Proxy :: Proxy NonEmptyStemTree)
>           (Proxy :: Proxy Integer) (Proxy :: Proxy Integer) (Proxy :: Proxy Integer)
>       ]
> 
>     , testGroup "Foldable"
>       [ test_Foldable_laws (Proxy :: Proxy StemTree)
>           (Proxy :: Proxy Integer) (Proxy :: Proxy Integer) (Proxy :: Proxy [Bool])
> 
>       , test_FoldableFunctor_laws (Proxy :: Proxy StemTree)
>           (Proxy :: Proxy Integer) (Proxy :: Proxy Integer) (Proxy :: Proxy [Bool])
> 
>       , test_Foldable_laws (Proxy :: Proxy NonEmptyStemTree)
>           (Proxy :: Proxy Integer) (Proxy :: Proxy Integer) (Proxy :: Proxy [Bool])
> 
>       , test_FoldableFunctor_laws (Proxy :: Proxy NonEmptyStemTree)
>           (Proxy :: Proxy Integer) (Proxy :: Proxy Integer) (Proxy :: Proxy [Bool])
>       ]
>     ]
> 
>   , test_StemTree_properties "Bool" (Proxy :: Proxy StemTree) (Proxy :: Proxy Bool)
>   , test_StemTree_properties "Int"  (Proxy :: Proxy StemTree) (Proxy :: Proxy Int)
> 
>   , test_StemTree_properties "Bool" (Proxy :: Proxy NonEmptyStemTree) (Proxy :: Proxy Bool)
>   , test_StemTree_properties "Int"  (Proxy :: Proxy NonEmptyStemTree) (Proxy :: Proxy Int)
>   ]



> test_StemTree_properties
>   :: forall a t
>    . ( Arb a, Eq a, Fmt.Display a, Prune a, MakeTo a, CoArb a
>      , Eq (t a), Fmt.Display (t a)
>      , Functor t, StemTreeFromList t )
>   => String -> Proxy (t :: * -> *) -> Proxy (a :: *) -> TestTree
> test_StemTree_properties name _ _ =
>   let title = "StemTree (" ++ name ++ ")" in
>   testGroup title
>     [ krebProp
>         "fmap f (spineFromList a as) == spineFromList (f a) (fmap f as)" $
>         \(f :: Fun a a) (a :: a) (as :: [a]) ->
>           claimEqual
>             (fmap (apFun f) (spineFromList a as :: t a))
>             (spineFromList (apFun f a) (fmap (apFun f) as))
> 
>     , krebProp
>         "fmap f (levelFromList a as) == levelFromList (f a) (fmap f as)" $
>         \(f :: Fun a a) (a :: a) (as :: [a]) ->
>           claimEqual
>             (fmap (apFun f) (levelFromList a as :: t a))
>             (levelFromList (apFun f a) (fmap (apFun f) as))
>     ]
