---
title: Kreb.Struct.Data.FingerTree.Zipper.Test
---

 > {-# LANGUAGE
 >     FlexibleContexts
 > #-}

> 
> module Kreb.Struct.Data.FingerTree.Zipper.Test (
>     test_FingerTreeZipper
> ) where
> 
> import Data.Proxy
> import Data.Foldable
> import GHC.Generics
> 
> import Test.Tasty
> 
> import Kreb.Prop
> import Kreb.Control.Constrained
> 
> import Kreb.Struct.Class
> import Kreb.Struct.Data.FingerTree.Zipper



OnePointedList Test Suite
-------------------------

> test_FingerTreeZipper :: TestTree
> test_FingerTreeZipper =
>   testGroup "FingerTreeZipper"
>     [ testGroup "Class Laws"
>       [ testGroup "Eq"
>         [ test_Eq_laws (Proxy :: Proxy (FingerTreeZipper (Counted Char)))
>         , test_Eq_laws (Proxy :: Proxy (FingerTreeZipper (Self [Bool])))
> 
>         , test_Eq_laws (Proxy :: Proxy (NonEmptyFingerTreeZipper (Counted Char)))
>         , test_Eq_laws (Proxy :: Proxy (NonEmptyFingerTreeZipper (Self [Bool])))
>         ]
> 
>       , testGroup "Subset"
>         [ test_Subset_laws "NonEmptyFingerTreeZipper" (Proxy :: Proxy NonEmptyFingerTreeZipper) (Proxy :: Proxy (Trivial Char))
>         , test_Subset_laws "NonEmptyFingerTreeZipper" (Proxy :: Proxy NonEmptyFingerTreeZipper) (Proxy :: Proxy (Counted Char))
>         ]
> 
>       , testGroup "NonEmpty"
>         [ test_NonEmpty_laws "NonEmptyFingerTreeZipper" (Proxy :: Proxy NonEmptyFingerTreeZipper) (Proxy :: Proxy (Trivial Char))
>         , test_NonEmpty_laws "NonEmptyFingerTreeZipper" (Proxy :: Proxy NonEmptyFingerTreeZipper) (Proxy :: Proxy (Counted Char))
>         ]
> 
>       , testGroup "Singleton"
>         [ test_Singleton_laws "FingerTreeZipper" (Proxy :: Proxy FingerTreeZipper) (Proxy :: Proxy (Counted Char))
>         , test_Singleton_laws "FingerTreeZipper" (Proxy :: Proxy FingerTreeZipper) (Proxy :: Proxy (Self [Bool]))
> 
>         , test_Singleton_laws "NonEmptyFingerTreeZipper" (Proxy :: Proxy NonEmptyFingerTreeZipper) (Proxy :: Proxy (Trivial Char))
>         , test_Singleton_laws "NonEmptyFingerTreeZipper" (Proxy :: Proxy NonEmptyFingerTreeZipper) (Proxy :: Proxy (Counted Char))
>         , test_SubsetSingleton_laws "NonEmptyFingerTreeZipper" (Proxy :: Proxy NonEmptyFingerTreeZipper) (Proxy :: Proxy (Trivial Char))
>         , test_SubsetSingleton_laws "NonEmptyFingerTreeZipper" (Proxy :: Proxy NonEmptyFingerTreeZipper) (Proxy :: Proxy (Counted Char))
>         , test_NonEmptySingleton_laws "NonEmptyFingerTreeZipper" (Proxy :: Proxy NonEmptyFingerTreeZipper) (Proxy :: Proxy (Trivial Char))
>         , test_NonEmptySingleton_laws "NonEmptyFingerTreeZipper" (Proxy :: Proxy NonEmptyFingerTreeZipper) (Proxy :: Proxy (Counted Char))
>         ]
> 
>       , testGroup "ConstrainedFunctor"
>         [ test_ConstrainedFunctor_laws (Proxy :: Proxy FingerTreeZipper)
>             (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool]))
>         , test_ConstrainedFunctor_laws (Proxy :: Proxy NonEmptyFingerTreeZipper)
>             (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool]))
>         ]
> 
>       , testGroup "Foldable"
>         [ test_Foldable_laws (Proxy :: Proxy FingerTreeZipper)
>             (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool]))
> 
>         , test_FoldableConstrainedFunctor_laws (Proxy :: Proxy FingerTreeZipper)
>             (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool]))
> 
>         , test_Foldable_laws (Proxy :: Proxy NonEmptyFingerTreeZipper)
>             (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool]))
> 
>         , test_FoldableConstrainedFunctor_laws (Proxy :: Proxy NonEmptyFingerTreeZipper)
>             (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool]))
>         ]
> 
>       , testGroup "ConstrainedTraversable"
>         [ test_ConstrainedTraversable_laws (Proxy :: Proxy FingerTreeZipper)
>             (Proxy :: Proxy Trivial) (Proxy :: Proxy Trivial)
>             (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool]))
> 
>         , test_ConstrainedTraversable_laws (Proxy :: Proxy FingerTreeZipper)
>             (Proxy :: Proxy Counted) (Proxy :: Proxy Counted)
>             (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool]))
> 
>         , test_ConstrainedTraversable_laws (Proxy :: Proxy NonEmptyFingerTreeZipper)
>             (Proxy :: Proxy Trivial) (Proxy :: Proxy Trivial)
>             (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool]))
> 
>         , test_ConstrainedTraversable_laws (Proxy :: Proxy NonEmptyFingerTreeZipper)
>             (Proxy :: Proxy Counted) (Proxy :: Proxy Counted)
>             (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool])) (Proxy :: Proxy (Self [Bool]))
>         ]

>       , testGroup "LinearZipper"
>         [ test_LinearZipper_laws "FingerTreeZipper"
>             (Proxy :: Proxy FingerTreeZipper)
>             (Proxy :: Proxy (Trivial Char))
>         , test_LinearZipper_laws "FingerTreeZipper"
>             (Proxy :: Proxy FingerTreeZipper)
>             (Proxy :: Proxy (Counted Char))
> 
>         , test_LinearZipper_laws "NonEmptyFingerTreeZipper"
>             (Proxy :: Proxy NonEmptyFingerTreeZipper)
>             (Proxy :: Proxy (Trivial Char))
>         , test_LinearZipper_laws "NonEmptyFingerTreeZipper"
>             (Proxy :: Proxy NonEmptyFingerTreeZipper)
>             (Proxy :: Proxy (Counted Char))
>         ]
>       ]
>     ]

> {-

>     [ testGroup "OnePointedList Properties"
>       [ test_OnePointedList_properties "Counted Char"
>           (Proxy :: Proxy (Counted Char))
>       , test_OnePointedList_properties "Self [Bool]"
>           (Proxy :: Proxy (Self [Bool]))
>       ]
> 
>     , testGroup "Class Laws"
>       [ testGroup "Foldable"

>         ]
>       ]
>     ]

> test_OnePointedList_properties
>   :: forall a
>    . ( Eq a, Valued a, Show a, Arb a, Prune a, CoArb a, MakeTo a
>      , Show (Value a), MakeTo (Value a), CoArb (Value a), Prune (Value a) )
>   => String -> Proxy a -> TestTree
> test_OnePointedList_properties label _ =
>   let title = "OnePointedList properties (" ++ label ++ ")"
>   in testGroup title
>     [ krebProp
>         "isEmpty (singleton a) == False" $
>         \(a :: a) ->
>           let as = singleton a :: OnePointedList a
>           in claimFalse (isEmpty as)
> 
>     , krebProp
>         "readInit (insertInit a as) === Just a" $
>         \(a :: a) (as :: OnePointedList a) ->
>           claimEqual
>             (Just a)
>             (readInit (insertInit a as))
> 
>     , krebProp
>         "(readInit as == Nothing) == (isEmpty as)" $
>         \(as :: OnePointedList a) ->
>           claimEqual
>             (Nothing == readInit as)
>             (isEmpty as)
> 
>     , krebProp
>         "as == deleteInit (insertInit a as)" $
>         \(a :: a) (as :: OnePointedList a) ->
>           claimEqual
>             (as)
>             (deleteInit (insertInit a as))
> 
>     , krebProp
>         "readLast (insertLast a as) === Just a" $
>         \(a :: a) (as :: OnePointedList a) ->
>           claimEqual
>             (Just a)
>             (readLast (insertLast a as))
> 
>     , krebProp
>         "as == deleteLast (insertLast a as)" $
>         \(a :: a) (as :: OnePointedList a) ->
>           claimEqual
>             (as)
>             (deleteLast (insertLast a as))
> 
>     , krebProp
>         "(moveToLast as) == (moveToLast (moveToLast as))" $
>         \(as :: OnePointedList a) ->
>           claimEqual
>             (moveToLast as)
>             (moveToLast (moveToLast as))
> 
>     , krebProp
>         "(isAtLast as) || (as == movePointLeft (movePointRight as))" $
>         \(as :: OnePointedList a) ->
>           claimAny
>             [ claimTrue (isAtLast as)
>             , claimEqual
>                 (as)
>                 (movePointLeft (movePointRight as))
>             ]
> 
>     , krebProp
>         "(isAtInit as) || (as == movePointRight (movePointLeft as))" $
>         \(as :: OnePointedList a) ->
>           claimAny
>             [ claimTrue (isAtInit as)
>             , claimEqual
>                 (as)
>                 (movePointRight (movePointLeft as))
>             ]
> 
>     , krebProp
>         "insertInit u (insertLast v as) == insertLast v (insertInit u as)" $
>         \(u :: a) (v :: a) (as :: OnePointedList a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (insertInit u (insertLast v as))
>                 (insertLast v (insertInit u as))
>             ]
> 
>     , krebProp
>         "deleteInit (deleteLast as) == deleteLast (deleteInit as)" $
>         \(as :: OnePointedList a) ->
>           claimEqual
>             (deleteInit (deleteLast as))
>             (deleteLast (deleteInit as))
> 
>     , krebProp
>         "(isEmpty as) || (isAtLast (moveToLast as))" $
>         \(as :: OnePointedList a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimTrue (isAtLast (moveToLast as))
>             ]
> 
>     , krebProp
>         "(isEmpty as) || (isAtInit (moveToInit as))" $
>         \(as :: OnePointedList a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimTrue (isAtInit (moveToInit as))
>             ]
> 
>     , krebProp
>         "as == deletePointRight (insertPointRight a as)" $
>         \(a :: a) (as :: OnePointedList a) ->
>           claimEqual
>             (as)
>             (deletePointRight (insertPointRight a as))
> 
>     , krebProp
>         "as == deletePointLeft (insertPointLeft a as)" $
>         \(a :: a) (as :: OnePointedList a) ->
>           claimEqual
>             (as)
>             (deletePointLeft (insertPointLeft a as))
> 
>     , krebProp
>         "(isEmpty as) || (readPoint (insertInit a as) == readPoint as)" $
>         \(a :: a) (as :: OnePointedList a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (readPoint (insertInit a as))
>                 (readPoint as)
>             ]
> 
>     , krebProp
>         "(isEmpty as) || (readPoint (insertLast a as) == readPoint as)" $
>         \(a :: a) (as :: OnePointedList a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (readPoint (insertLast a as))
>                 (readPoint as)
>             ]
> 
>     , krebProp
>         "(isAtInit as) || (readPoint (deleteInit as) == readPoint as)" $
>         \(as :: OnePointedList a) ->
>           claimAny
>             [ claimTrue (isAtInit as)
>             , claimEqual
>                 (readPoint (deleteInit as))
>                 (readPoint as)
>             ]
> 
>     , krebProp
>         "(isAtLast as) || (readPoint (deleteLast as) == readPoint as)" $
>         \(as :: OnePointedList a) ->
>           claimAny
>             [ claimTrue (isAtLast as)
>             , claimEqual
>                 (readPoint (deleteLast as))
>                 (readPoint as)
>             ]
> 
>     , krebProp
>         "(isSingleton as) || (alterInit f (alterLast g as) == alterLast g (alterInit f as))" $
>         \(as :: OnePointedList a) (f :: Fun a a) (g :: Fun a a) ->
>           claimAny
>             [ claimTrue (isSingleton as)
>             , claimEqual
>                 (alterInit (apFun f) (alterLast (apFun g) as))
>                 (alterLast (apFun g) (alterInit (apFun f) as))
>             ]
> 
>     , krebProp
>         "(p mempty) || (not (p (value as))) || (as == integrate (split p as))" $
>         \(as :: FT.FingerTree a) (p :: Fun (Value a) Bool) ->
>           claimAny
>             [ claimTrue (apFun p mempty)
>             , claimFalse (apFun p (value as))
>             , claimEqual
>                 (as)
>                 (integrate (split (apFun p) as))
>             ]
> 
>     , krebProp
>         "moveToInit as == fromList (toList as)" $
>         \(as :: OnePointedList a) ->
>           claimEqual
>             (moveToInit as)
>             (fromList (toList as))
> 
>     , krebProp
>         "as == toList (fromList as)" $
>         \(xs :: [a]) ->
>           claimEqual
>             (xs)
>             (toList (fromList xs :: OnePointedList a))
> 
>     , krebProp
>         "prepend as (prepend bs cs) == prepend (prepend as bs) cs" $
>         \(as :: OnePointedList a) (bs :: OnePointedList a) (cs :: OnePointedList a) ->
>           claimEqual
>             (prepend as (prepend bs cs))
>             (prepend (prepend as bs) cs)
> 
>     , krebProp
>         "append as (append bs cs) == append (append as bs) cs" $
>         \(as :: OnePointedList a) (bs :: OnePointedList a) (cs :: OnePointedList a) ->
>           claimEqual
>             (append as (append bs cs))
>             (append (append as bs) cs)
> 
>     , krebProp
>         "prepend as (append bs cs) == append bs (prepend as cs)" $
>         \(as :: OnePointedList a) (bs :: OnePointedList a) (cs :: OnePointedList a) ->
>           claimAny
>             [ claimTrue (isEmpty cs)
>             , claimEqual
>                 (prepend as (append bs cs))
>                 (append bs (prepend as cs))
>             ]
>     ]

> -}
