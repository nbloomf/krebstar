---
title: Kreb.Struct.Sequence.Test
---



> {-# LANGUAGE
>     UndecidableInstances
>   , FlexibleContexts
>   , ScopedTypeVariables
> #-}
> 
> module Kreb.Struct.Sequence.Test (
>     test_Sequence
> ) where
> 
> import Data.Proxy
> import Data.Foldable
> 
> import Test.Tasty
> 
> import Kreb.Prop
> import Kreb.Struct.Data.Deque.Zipper



> test_Sequence :: TestTree
> test_Sequence =
>   testGroup "Sequence"
>     [ test_Sequence_properties "Bool" (Proxy :: Proxy Bool)
>     , test_Sequence_properties "Int" (Proxy :: Proxy Int)
>     ]


> test_Sequence_properties
>   :: forall m a
>    . ( Eq a, Show a, Arb a, Prune a, MakeTo a, CoArb a )
>   => String -> Proxy a -> TestTree
> test_Sequence_properties label _ =
>   let title = "Sequence properties (" ++ label ++ ")"
>   in testGroup title []

> {-

>     [ krebProp
>         "isEmpty (singleton a) == False" $
>         \(a :: a) ->
>           let as = singleton a :: Sequence a
>           in claimFalse (isEmpty as)
> 
>     , krebProp
>         "readInit (insertInit a as) === Just a" $
>         \(a :: a) (as :: Sequence a) ->
>           claimEqual
>             (Just a)
>             (readInit (insertInit a as))
> 
>     , krebProp
>         "(readInit as == Nothing) == (isEmpty as)" $
>         \(as :: Sequence a) ->
>           claimEqual
>             (Nothing == readInit as)
>             (isEmpty as)
> 
>     , krebProp
>         "as == deleteInit (insertInit a as)" $
>         \(a :: a) (as :: Sequence a) ->
>           claimEqual
>             (as)
>             (deleteInit (insertInit a as))
> 
>     , krebProp
>         "(moveToInit as) == (moveToInit (moveToInit as))" $
>         \(as :: Sequence a) ->
>           claimEqual
>             (moveToInit as)
>             (moveToInit (moveToInit as))
> 
>     , krebProp
>         "readLast (insertLast a as) === Just a" $
>         \(a :: a) (as :: Sequence a) ->
>           claimEqual
>             (Just a)
>             (readLast (insertLast a as))
> 
>     , krebProp
>         "as == deleteLast (insertLast a as)" $
>         \(a :: a) (as :: Sequence a) ->
>           claimEqual
>             (as)
>             (deleteLast (insertLast a as))
> 
>     , krebProp
>         "(moveToLast as) == (moveToLast (moveToLast as))" $
>         \(as :: Sequence a) ->
>           claimEqual
>             (moveToLast as)
>             (moveToLast (moveToLast as))
> 
>     , krebProp
>         "(isAtLast as) || (as == movePointLeft (movePointRight as))" $
>         \(as :: Sequence a) ->
>           claimAny
>             [ claimTrue (isAtLast as)
>             , claimEqual
>                 (as)
>                 (movePointLeft (movePointRight as))
>             ]
> 
>     , krebProp
>         "(isAtInit as) || (as == movePointRight (movePointLeft as))" $
>         \(as :: Sequence a) ->
>           claimAny
>             [ claimTrue (isAtInit as)
>             , claimEqual
>                 (as)
>                 (movePointRight (movePointLeft as))
>             ]
> 
>     , krebProp
>         "insertInit u (insertLast v as) == insertLast v (insertInit u as)" $
>         \(u :: a) (v :: a) (as :: Sequence a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (insertInit u (insertLast v as))
>                 (insertLast v (insertInit u as))
>             ]
> 
>     , krebProp
>         "deleteInit (deleteLast as) == deleteLast (deleteInit as)" $
>         \(as :: Sequence a) ->
>           claimEqual
>             (deleteInit (deleteLast as))
>             (deleteLast (deleteInit as))
> 
>     , krebProp
>         "(isEmpty as) || (isAtLast (moveToLast as))" $
>         \(as :: Sequence a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimTrue (isAtLast (moveToLast as))
>             ]
> 
>     , krebProp
>         "(isEmpty as) || (isAtInit (moveToInit as))" $
>         \(as :: Sequence a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimTrue (isAtInit (moveToInit as))
>             ]
> 
>     , krebProp
>         "as == deletePointRight (insertPointRight a as)" $
>         \(a :: a) (as :: Sequence a) ->
>           claimEqual
>             (as)
>             (deletePointRight (insertPointRight a as))
> 
>     , krebProp
>         "as == deletePointLeft (insertPointLeft a as)" $
>         \(a :: a) (as :: Sequence a) ->
>           claimEqual
>             (as)
>             (deletePointLeft (insertPointLeft a as))
> 
>     , krebProp
>         "(isEmpty as) || (readPoint (insertInit a as) == readPoint as)" $
>         \(a :: a) (as :: Sequence a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (readPoint (insertInit a as))
>                 (readPoint as)
>             ]
> 
>     , krebProp
>         "(isEmpty as) || (readPoint (insertLast a as) == readPoint as)" $
>         \(a :: a) (as :: Sequence a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (readPoint (insertLast a as))
>                 (readPoint as)
>             ]
> 
>     , krebProp
>         "(isAtInit as) || (readPoint (deleteInit as) == readPoint as)" $
>         \(as :: Sequence a) ->
>           claimAny
>             [ claimTrue (isAtInit as)
>             , claimEqual
>                 (readPoint (deleteInit as))
>                 (readPoint as)
>             ]
> 
>     , krebProp
>         "(isAtLast as) || (readPoint (deleteLast as) == readPoint as)" $
>         \(as :: Sequence a) ->
>           claimAny
>             [ claimTrue (isAtLast as)
>             , claimEqual
>                 (readPoint (deleteLast as))
>                 (readPoint as)
>             ]
> 
>     , krebProp
>         "(isSingleton as) || (alterInit f (alterLast g as) == alterLast g (alterInit f as))" $
>         \(as :: Sequence a) (f :: Fun a a) (g :: Fun a a) ->
>           claimAny
>             [ claimTrue (isSingleton as)
>             , claimEqual
>                 (alterInit (apFun f) (alterLast (apFun g) as))
>                 (alterLast (apFun g) (alterInit (apFun f) as))
>             ]
> 
>     , krebProp
>         "moveToInit as == makeFromList (toList as)" $
>         \(as :: Sequence a) ->
>           claimEqual
>             (moveToInit as)
>             (fromList (toList as))
> 
>     , krebProp
>         "as == toList (makeFromList as)" $
>         \(xs :: [a]) ->
>           claimEqual
>             (xs)
>             (toList (fromList xs :: Sequence a))
> 
>     , krebProp
>         "moveToInit as == moveToIndex 0 as" $
>         \(as :: Sequence a) ->
>           claimEqual
>             (moveToInit as)
>             (moveToIndex 0 as)
> 
>     , krebProp
>         "movetoLast as == moveToIndex (getLength as - 1) as" $
>         \(as :: Sequence a) ->
>           claimEqual
>             (moveToLast as)
>             (moveToIndex (getLength as - 1) as)
> 
>     , krebProp
>         "prepend as (prepend bs cs) == prepend (prepend as bs) cs" $
>         \(as :: Sequence a) (bs :: Sequence a) (cs :: Sequence a) ->
>           claimEqual
>             (prepend as (prepend bs cs))
>             (prepend (prepend as bs) cs)
> 
>     , krebProp
>         "append as (append bs cs) == append (append as bs) cs" $
>         \(as :: Sequence a) (bs :: Sequence a) (cs :: Sequence a) ->
>           claimEqual
>             (append as (append bs cs))
>             (append (append as bs) cs)
> 
>     , krebProp
>         "prepend as (append bs cs) == append bs (prepend as cs)" $
>         \(as :: Sequence a) (bs :: Sequence a) (cs :: Sequence a) ->
>           claimAny
>             [ claimTrue (isEmpty cs)
>             , claimEqual
>                 (prepend as (append bs cs))
>                 (append bs (prepend as cs))
>             ]
>     ]

> -}
