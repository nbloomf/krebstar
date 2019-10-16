---
title: Kreb.Struct.OnePointedList.Test
---

> {-# LANGUAGE
>     MultiParamTypeClasses
>   , ScopedTypeVariables
>   , KindSignatures
>   , DeriveGeneric
> #-}
> 
> module Kreb.Struct.OnePointedList.Test (
>     test_OnePointedList
> ) where
> 
> import Data.Proxy
> import Data.Foldable
> import GHC.Generics
> 
> import Test.Tasty
> 
> import Kreb.Check
> import Kreb.Struct.FingerTree
> import Kreb.Struct.OnePointedList
> import Kreb.Struct.FingerTree.Test



OnePointedList Test Suite
-------------------------

> test_OnePointedList :: TestTree
> test_OnePointedList =
>   testGroup "OnePointedList"
>     [ test_OnePointedList_properties
>         "Char/Count" (Proxy :: Proxy Char) (Proxy :: Proxy Count)
>     , test_OnePointedList_properties
>         "Bool/Tup" (Proxy :: Proxy Bool) (Proxy :: Proxy Tup)
> 
>     , testGroup "Class Laws"
>       [ test_Foldable_laws_with
>           (fold :: OnePointedList Count Bool -> Bool)
>           (foldMap :: forall u n . (Monoid n) => (u -> n) -> OnePointedList Count u -> n)
>           (foldr :: (Bool -> Bool -> Bool) -> Bool -> OnePointedList Count Bool -> Bool)
>       , test_FoldableFunctor_laws_with
>           (fmapList :: (Bool -> Bool) -> OnePointedList Count Bool -> OnePointedList Count Bool)
>           (fmapList :: (Bool -> Bool) -> OnePointedList Count Bool -> OnePointedList Count Bool)
>           (fold :: OnePointedList Count Bool -> Bool)
>           (foldMap :: forall u n . (Monoid n) => (u -> n) -> OnePointedList Count u -> n)
>       ]
>     ]

> test_OnePointedList_properties
>   :: forall m a
>    . ( Eq a, Valued m a, Show a, Arb a, Prune a, MakeTo a
>      , CoArb a, Show m, MakeTo m, CoArb m, Prune m )
>   => String -> Proxy a -> Proxy m -> TestTree
> test_OnePointedList_properties label _ _ =
>   let title = "OnePointedList properties (" ++ label ++ ")"
>   in testGroup title
>     [ testKreb
>         "isEmpty (singleton a) == False" $
>         \(a :: a) ->
>           let as = singleton a :: OnePointedList m a
>           in claimFalse (isEmpty as)
> 
>     , testKreb
>         "readInit (insertInit a as) === Just a" $
>         \(a :: a) (as :: OnePointedList m a) ->
>           claimEqual
>             (Just a)
>             (readInit (insertInit a as))
> 
>     , testKreb
>         "(readInit as == Nothing) == (isEmpty as)" $
>         \(as :: OnePointedList m a) ->
>           claimEqual
>             (Nothing == readInit as)
>             (isEmpty as)
> 
>     , testKreb
>         "as == deleteInit (insertInit a as)" $
>         \(a :: a) (as :: OnePointedList m a) ->
>           claimEqual
>             (as)
>             (deleteInit (insertInit a as))
> 
>     , testKreb
>         "(moveToInit as) == (moveToInit (moveToInit as))" $
>         \(as :: OnePointedList m a) ->
>           claimEqual
>             (moveToInit as)
>             (moveToInit (moveToInit as))
> 
>     , testKreb
>         "readLast (insertLast a as) === Just a" $
>         \(a :: a) (as :: OnePointedList m a) ->
>           claimEqual
>             (Just a)
>             (readLast (insertLast a as))
> 
>     , testKreb
>         "as == deleteLast (insertLast a as)" $
>         \(a :: a) (as :: OnePointedList m a) ->
>           claimEqual
>             (as)
>             (deleteLast (insertLast a as))
> 
>     , testKreb
>         "(moveToLast as) == (moveToLast (moveToLast as))" $
>         \(as :: OnePointedList m a) ->
>           claimEqual
>             (moveToLast as)
>             (moveToLast (moveToLast as))
> 
>     , testKreb
>         "(isAtLast as) || (as == movePointLeft (movePointRight as))" $
>         \(as :: OnePointedList m a) ->
>           claimAny
>             [ claimTrue (isAtLast as)
>             , claimEqual
>                 (as)
>                 (movePointLeft (movePointRight as))
>             ]
> 
>     , testKreb
>         "(isAtInit as) || (as == movePointRight (movePointLeft as))" $
>         \(as :: OnePointedList m a) ->
>           claimAny
>             [ claimTrue (isAtInit as)
>             , claimEqual
>                 (as)
>                 (movePointRight (movePointLeft as))
>             ]
> 
>     , testKreb
>         "insertInit u (insertLast v as) == insertLast v (insertInit u as)" $
>         \(u :: a) (v :: a) (as :: OnePointedList m a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (insertInit u (insertLast v as))
>                 (insertLast v (insertInit u as))
>             ]
> 
>     , testKreb
>         "deleteInit (deleteLast as) == deleteLast (deleteInit as)" $
>         \(as :: OnePointedList m a) ->
>           claimEqual
>             (deleteInit (deleteLast as))
>             (deleteLast (deleteInit as))
> 
>     , testKreb
>         "(isEmpty as) || (isAtLast (moveToLast as))" $
>         \(as :: OnePointedList m a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimTrue (isAtLast (moveToLast as))
>             ]
> 
>     , testKreb
>         "(isEmpty as) || (isAtInit (moveToInit as))" $
>         \(as :: OnePointedList m a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimTrue (isAtInit (moveToInit as))
>             ]
> 
>     , testKreb
>         "as == deletePointRight (insertPointRight a as)" $
>         \(a :: a) (as :: OnePointedList m a) ->
>           claimEqual
>             (as)
>             (deletePointRight (insertPointRight a as))
> 
>     , testKreb
>         "as == deletePointLeft (insertPointLeft a as)" $
>         \(a :: a) (as :: OnePointedList m a) ->
>           claimEqual
>             (as)
>             (deletePointLeft (insertPointLeft a as))
> 
>     , testKreb
>         "(isEmpty as) || (readPoint (insertInit a as) == readPoint as)" $
>         \(a :: a) (as :: OnePointedList m a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (readPoint (insertInit a as))
>                 (readPoint as)
>             ]
> 
>     , testKreb
>         "(isEmpty as) || (readPoint (insertLast a as) == readPoint as)" $
>         \(a :: a) (as :: OnePointedList m a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (readPoint (insertLast a as))
>                 (readPoint as)
>             ]
> 
>     , testKreb
>         "(isAtInit as) || (readPoint (deleteInit as) == readPoint as)" $
>         \(as :: OnePointedList m a) ->
>           claimAny
>             [ claimTrue (isAtInit as)
>             , claimEqual
>                 (readPoint (deleteInit as))
>                 (readPoint as)
>             ]
> 
>     , testKreb
>         "(isAtLast as) || (readPoint (deleteLast as) == readPoint as)" $
>         \(as :: OnePointedList m a) ->
>           claimAny
>             [ claimTrue (isAtLast as)
>             , claimEqual
>                 (readPoint (deleteLast as))
>                 (readPoint as)
>             ]
> 
>     , testKreb
>         "(isSingleton as) || (alterInit f (alterLast g as) == alterLast g (alterInit f as))" $
>         \(as :: OnePointedList m a) (f :: Fun a a) (g :: Fun a a) ->
>           claimAny
>             [ claimTrue (isSingleton as)
>             , claimEqual
>                 (alterInit (apFun f) (alterLast (apFun g) as))
>                 (alterLast (apFun g) (alterLast (apFun f) as))
>             ]
> 
>     , testKreb
>         "(p mempty) || (not (p (value as))) || (as == integrate (split p as))" $
>         \(as :: FingerTree m a) (p :: Fun m Bool) ->
>           claimAny
>             [ claimTrue (apFun p mempty)
>             , claimFalse (apFun p (value as))
>             , claimEqual
>                 (as)
>                 (integrate (split (apFun p) as))
>             ]
> 
>     , testKreb
>         "moveToInit as == makeFromList (toList as)" $
>         \(as :: OnePointedList m a) ->
>           claimEqual
>             (moveToInit as)
>             (makeFromList (toList as))
> 
>     , testKreb
>         "as == toList (makeFromList as)" $
>         \(xs :: [a]) ->
>           claimEqual
>             (xs)
>             (toList (makeFromList xs :: OnePointedList m a))
>     ]
