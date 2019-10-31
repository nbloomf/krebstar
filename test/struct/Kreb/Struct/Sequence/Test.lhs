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
> import Kreb.Check
> import Kreb.Struct.Sequence



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
>   in testGroup title
>     [ testKreb
>         "isEmpty (singleton a) == False" $
>         \(a :: a) ->
>           let as = singleton a :: Sequence a
>           in claimFalse (isEmpty as)
> 
>     , testKreb
>         "readInit (insertInit a as) === Just a" $
>         \(a :: a) (as :: Sequence a) ->
>           claimEqual
>             (Just a)
>             (readInit (insertInit a as))
> 
>     , testKreb
>         "(readInit as == Nothing) == (isEmpty as)" $
>         \(as :: Sequence a) ->
>           claimEqual
>             (Nothing == readInit as)
>             (isEmpty as)
> 
>     , testKreb
>         "as == deleteInit (insertInit a as)" $
>         \(a :: a) (as :: Sequence a) ->
>           claimEqual
>             (as)
>             (deleteInit (insertInit a as))
> 
>     , testKreb
>         "(moveToInit as) == (moveToInit (moveToInit as))" $
>         \(as :: Sequence a) ->
>           claimEqual
>             (moveToInit as)
>             (moveToInit (moveToInit as))
> 
>     , testKreb
>         "readLast (insertLast a as) === Just a" $
>         \(a :: a) (as :: Sequence a) ->
>           claimEqual
>             (Just a)
>             (readLast (insertLast a as))
> 
>     , testKreb
>         "as == deleteLast (insertLast a as)" $
>         \(a :: a) (as :: Sequence a) ->
>           claimEqual
>             (as)
>             (deleteLast (insertLast a as))
> 
>     , testKreb
>         "(moveToLast as) == (moveToLast (moveToLast as))" $
>         \(as :: Sequence a) ->
>           claimEqual
>             (moveToLast as)
>             (moveToLast (moveToLast as))
> 
>     , testKreb
>         "(isAtLast as) || (as == movePointLeft (movePointRight as))" $
>         \(as :: Sequence a) ->
>           claimAny
>             [ claimTrue (isAtLast as)
>             , claimEqual
>                 (as)
>                 (movePointLeft (movePointRight as))
>             ]
> 
>     , testKreb
>         "(isAtInit as) || (as == movePointRight (movePointLeft as))" $
>         \(as :: Sequence a) ->
>           claimAny
>             [ claimTrue (isAtInit as)
>             , claimEqual
>                 (as)
>                 (movePointRight (movePointLeft as))
>             ]
> 
>     , testKreb
>         "insertInit u (insertLast v as) == insertLast v (insertInit u as)" $
>         \(u :: a) (v :: a) (as :: Sequence a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (insertInit u (insertLast v as))
>                 (insertLast v (insertInit u as))
>             ]
> 
>     , testKreb
>         "deleteInit (deleteLast as) == deleteLast (deleteInit as)" $
>         \(as :: Sequence a) ->
>           claimEqual
>             (deleteInit (deleteLast as))
>             (deleteLast (deleteInit as))
> 
>     , testKreb
>         "(isEmpty as) || (isAtLast (moveToLast as))" $
>         \(as :: Sequence a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimTrue (isAtLast (moveToLast as))
>             ]
> 
>     , testKreb
>         "(isEmpty as) || (isAtInit (moveToInit as))" $
>         \(as :: Sequence a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimTrue (isAtInit (moveToInit as))
>             ]
> 
>     , testKreb
>         "as == deletePointRight (insertPointRight a as)" $
>         \(a :: a) (as :: Sequence a) ->
>           claimEqual
>             (as)
>             (deletePointRight (insertPointRight a as))
> 
>     , testKreb
>         "as == deletePointLeft (insertPointLeft a as)" $
>         \(a :: a) (as :: Sequence a) ->
>           claimEqual
>             (as)
>             (deletePointLeft (insertPointLeft a as))
> 
>     , testKreb
>         "(isEmpty as) || (readPoint (insertInit a as) == readPoint as)" $
>         \(a :: a) (as :: Sequence a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (readPoint (insertInit a as))
>                 (readPoint as)
>             ]
> 
>     , testKreb
>         "(isEmpty as) || (readPoint (insertLast a as) == readPoint as)" $
>         \(a :: a) (as :: Sequence a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (readPoint (insertLast a as))
>                 (readPoint as)
>             ]
> 
>     , testKreb
>         "(isAtInit as) || (readPoint (deleteInit as) == readPoint as)" $
>         \(as :: Sequence a) ->
>           claimAny
>             [ claimTrue (isAtInit as)
>             , claimEqual
>                 (readPoint (deleteInit as))
>                 (readPoint as)
>             ]
> 
>     , testKreb
>         "(isAtLast as) || (readPoint (deleteLast as) == readPoint as)" $
>         \(as :: Sequence a) ->
>           claimAny
>             [ claimTrue (isAtLast as)
>             , claimEqual
>                 (readPoint (deleteLast as))
>                 (readPoint as)
>             ]
> 
>     , testKreb
>         "(isSingleton as) || (alterInit f (alterLast g as) == alterLast g (alterInit f as))" $
>         \(as :: Sequence a) (f :: Fun a a) (g :: Fun a a) ->
>           claimAny
>             [ claimTrue (isSingleton as)
>             , claimEqual
>                 (alterInit (apFun f) (alterLast (apFun g) as))
>                 (alterLast (apFun g) (alterInit (apFun f) as))
>             ]
> 
>     , testKreb
>         "moveToInit as == makeFromList (toList as)" $
>         \(as :: Sequence a) ->
>           claimEqual
>             (moveToInit as)
>             (fromList (toList as))
> 
>     , testKreb
>         "as == toList (makeFromList as)" $
>         \(xs :: [a]) ->
>           claimEqual
>             (xs)
>             (toList (fromList xs :: Sequence a))
> 
>     , testKreb
>         "moveToInit as == moveToIndex 0 as" $
>         \(as :: Sequence a) ->
>           claimEqual
>             (moveToInit as)
>             (moveToIndex 0 as)
> 
>     , testKreb
>         "movetoLast as == moveToIndex (getLength as - 1) as" $
>         \(as :: Sequence a) ->
>           claimEqual
>             (moveToLast as)
>             (moveToIndex (getLength as - 1) as)
> 
>     , testKreb
>         "prepend as (prepend bs cs) == prepend (prepend as bs) cs" $
>         \(as :: Sequence a) (bs :: Sequence a) (cs :: Sequence a) ->
>           claimEqual
>             (prepend as (prepend bs cs))
>             (prepend (prepend as bs) cs)
> 
>     , testKreb
>         "append as (append bs cs) == append (append as bs) cs" $
>         \(as :: Sequence a) (bs :: Sequence a) (cs :: Sequence a) ->
>           claimEqual
>             (append as (append bs cs))
>             (append (append as bs) cs)
> 
>     , testKreb
>         "prepend as (append bs cs) == append bs (prepend as cs)" $
>         \(as :: Sequence a) (bs :: Sequence a) (cs :: Sequence a) ->
>           claimAny
>             [ claimTrue (isEmpty cs)
>             , claimEqual
>                 (prepend as (append bs cs))
>                 (append bs (prepend as cs))
>             ]
>     ]
