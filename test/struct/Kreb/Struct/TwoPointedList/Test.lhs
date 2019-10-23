---
title: Kreb.Struct.TwoPointedList.Test
---

> {-# LANGUAGE
>     MultiParamTypeClasses
>   , ScopedTypeVariables
>   , KindSignatures
>   , DeriveGeneric
> #-}
> 
> module Kreb.Struct.TwoPointedList.Test (
>     test_TwoPointedList
> ) where
> 
> import Data.Proxy
> import Data.Foldable
> import GHC.Generics
> 
> import Test.Tasty
> 
> import           Kreb.Check
> import           Kreb.Struct.Valued
> import qualified Kreb.Struct.FingerTree as FT
> import           Kreb.Struct.TwoPointedList
> import           Kreb.Struct.FingerTree.Test


> test_TwoPointedList :: TestTree
> test_TwoPointedList =
>   testGroup "TwoPointedList"
>     [ test_TwoPointedList_properties
>         "Char/Count" (Proxy :: Proxy Char) (Proxy :: Proxy Count)
>     , test_TwoPointedList_properties
>         "Bool/Tup" (Proxy :: Proxy Bool) (Proxy :: Proxy Tup)
> 
>     , testGroup "Class Laws"
>       [ test_Foldable_laws_with
>           (fold :: TwoPointedList Count Bool -> Bool)
>           (foldMap :: forall u n . (Monoid n) => (u -> n) -> TwoPointedList Count u -> n)
>           (foldr :: (Bool -> Bool -> Bool) -> Bool -> TwoPointedList Count Bool -> Bool)
>       , test_FoldableFunctor_laws_with
>           (fmapList :: (Bool -> Bool) -> TwoPointedList Count Bool -> TwoPointedList Count Bool)
>           (fmapList :: (Bool -> Bool) -> TwoPointedList Count Bool -> TwoPointedList Count Bool)
>           (fold :: TwoPointedList Count Bool -> Bool)
>           (foldMap :: forall u n . (Monoid n) => (u -> n) -> TwoPointedList Count u -> n)
>       ]
>     ]


> test_TwoPointedList_properties
>   :: forall m a
>    . ( Eq a, Valued m a, Show a, Arb a, Prune a, MakeTo a
>      , CoArb a, Show m, MakeTo m, CoArb m, Prune m )
>   => String -> Proxy a -> Proxy m -> TestTree
> test_TwoPointedList_properties label _ _ =
>   let title = "TwoPointedList properties (" ++ label ++ ")"
>   in testGroup title
>     [ testKreb
>         "isEmpty (singleton a) == False" $
>         \(a :: a) ->
>           let as = singleton a :: TwoPointedList m a
>           in claimFalse (isEmpty as)
> 
>     , testKreb
>         "movePointToStart (movePointToStart as) == movePointToStart as" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (movePointToStart (movePointToStart as))
>             (movePointToStart as)
> 
>     , testKreb
>         "movePointToEnd (movePointToEnd as) == movePointToEnd as" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (movePointToEnd (movePointToEnd as))
>             (movePointToEnd as)
> 
>     , testKreb
>         "moveMarkToStart (moveMarkToStart as) == moveMarkToStart as" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (moveMarkToStart (moveMarkToStart as))
>             (moveMarkToStart as)
> 
>     , testKreb
>         "moveMarkToEnd (moveMarkToEnd as) == moveMarkToEnd as" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (moveMarkToEnd (moveMarkToEnd as))
>             (moveMarkToEnd as)
> 
>     , testKreb
>         "(isPointAtEnd as) || (as == movePointLeft (movePointRight as))" $
>         \(as :: TwoPointedList m a) ->
>           claimAny
>             [ claimTrue (isPointAtEnd as)
>             , claimEqual
>                 (as)
>                 (movePointLeft (movePointRight as))
>             ]
> 
>     , testKreb
>         "(isPointAtStart as) || (as == movePointRight (movePointLeft as))" $
>         \(as :: TwoPointedList m a) ->
>           claimAny
>             [ claimTrue (isPointAtStart as)
>             , claimEqual
>                 (as)
>                 (movePointRight (movePointLeft as))
>             ]
> 
>     , testKreb
>         "(isMarkAtEnd as) || (as == moveMarkLeft (moveMarkRight as))" $
>         \(as :: TwoPointedList m a) ->
>           claimAny
>             [ claimTrue (isMarkAtEnd as)
>             , claimEqual
>                 (as)
>                 (moveMarkLeft (moveMarkRight as))
>             ]
> 
>     , testKreb
>         "(isMarkAtStart as) || (as == moveMarkRight (moveMarkLeft as))" $
>         \(as :: TwoPointedList m a) ->
>           claimAny
>             [ claimTrue (isMarkAtStart as)
>             , claimEqual
>                 (as)
>                 (moveMarkRight (moveMarkLeft as))
>             ]
> 
>     , testKreb
>         "movePointToStart (moveMarkToStart as) == moveMarkToStart (movePointToStart as)" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (movePointToStart (moveMarkToStart as))
>             (moveMarkToStart (movePointToStart as))
> 
>     , testKreb
>         "movePointToStart (moveMarkToEnd as) == moveMarkToEnd (movePointToStart as)" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (movePointToStart (moveMarkToEnd as))
>             (moveMarkToEnd (movePointToStart as))
> 
>     , testKreb
>         "movePointToEnd (moveMarkToStart as) == moveMarkToStart (movePointToEnd as)" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (movePointToEnd (moveMarkToStart as))
>             (moveMarkToStart (movePointToEnd as))
> 
>     , testKreb
>         "movePointToEnd (moveMarkToEnd as) == moveMarkToEnd (movePointToEnd as)" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (movePointToEnd (moveMarkToEnd as))
>             (moveMarkToEnd (movePointToEnd as))
> 
>     , testKreb
>         "movePointLeft (moveMarkLeft as) == moveMarkLeft (movePointLeft as)" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (movePointLeft (moveMarkLeft as))
>             (moveMarkLeft (movePointLeft as))
> 
>     , testKreb
>         "movePointLeft (moveMarkRight as) == moveMarkRight (movePointLeft as)" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (movePointLeft (moveMarkRight as))
>             (moveMarkRight (movePointLeft as))
> 
>     , testKreb
>         "movePointRight (moveMarkLeft as) == moveMarkLeft (movePointRight as)" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (movePointRight (moveMarkLeft as))
>             (moveMarkLeft (movePointRight as))
> 
>     , testKreb
>         "movePointRight (moveMarkRight as) == moveMarkRight (movePointRight as)" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (movePointRight (moveMarkRight as))
>             (moveMarkRight (movePointRight as))
> 
>     , testKreb
>         "movePointLeft (moveMarkToStart as) == moveMarkToStart (movePointLeft as)" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (movePointLeft (moveMarkToStart as))
>             (moveMarkToStart (movePointLeft as))
> 
>     , testKreb
>         "movePointRight (moveMarkToStart as) == moveMarkToStart (movePointRight as)" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (movePointRight (moveMarkToStart as))
>             (moveMarkToStart (movePointRight as))
> 
>     , testKreb
>         "movePointLeft (moveMarkToEnd as) == moveMarkToEnd (movePointLeft as)" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (movePointLeft (moveMarkToEnd as))
>             (moveMarkToEnd (movePointLeft as))
> 
>     , testKreb
>         "movePointRight (moveMarkToEnd as) == moveMarkToEnd (movePointRight as)" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (movePointRight (moveMarkToEnd as))
>             (moveMarkToEnd (movePointRight as))
> 
>     , testKreb
>         "movePointToStart (moveMarkLeft as) == moveMarkLeft (movePointToStart as)" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (movePointToStart (moveMarkLeft as))
>             (moveMarkLeft (movePointToStart as))
> 
>     , testKreb
>         "movePointToStart (moveMarkRight as) == moveMarkRight (movePointToStart as)" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (movePointToStart (moveMarkRight as))
>             (moveMarkRight (movePointToStart as))
> 
>     , testKreb
>         "movePointToEnd (moveMarkLeft as) == moveMarkLeft (movePointToEnd as)" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (movePointToEnd (moveMarkLeft as))
>             (moveMarkLeft (movePointToEnd as))
> 
>     , testKreb
>         "movePointToEnd (moveMarkRight as) == moveMarkRight (movePointToEnd as)" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (movePointToEnd (moveMarkRight as))
>             (moveMarkRight (movePointToEnd as))
> 
>     , testKreb
>        "readPoint (moveMarkToStart as) == readPoint as" $
>        \(as :: TwoPointedList m a) ->
>          claimEqual
>            (readPoint as)
>            (readPoint (moveMarkToStart as))
> 
>     , testKreb
>        "readPoint (moveMarkToEnd as) == readPoint as" $
>        \(as :: TwoPointedList m a) ->
>          claimEqual
>            (readPoint as)
>            (readPoint (moveMarkToEnd as))
> 
>     , testKreb
>        "readPoint (moveMarkLeft as) == readPoint as" $
>        \(as :: TwoPointedList m a) ->
>          claimEqual
>            (readPoint as)
>            (readPoint (moveMarkLeft as))
> 
>     , testKreb
>        "readPoint (moveMarkRight as) == readPoint as" $
>        \(as :: TwoPointedList m a) ->
>          claimEqual
>            (readPoint as)
>            (readPoint (moveMarkRight as))
> 
>     , testKreb
>        "readMark (movePointToStart as) == readMark as" $
>        \(as :: TwoPointedList m a) ->
>          claimEqual
>            (readMark as)
>            (readMark (movePointToStart as))
> 
>     , testKreb
>        "readMark (movePointToEnd as) == readMark as" $
>        \(as :: TwoPointedList m a) ->
>          claimEqual
>            (readMark as)
>            (readMark (movePointToEnd as))
> 
>     , testKreb
>        "readMark (movePointLeft as) == readMark as" $
>        \(as :: TwoPointedList m a) ->
>          claimEqual
>            (readMark as)
>            (readMark (movePointLeft as))
> 
>     , testKreb
>        "readMark (movePointRight as) == readMark as" $
>        \(as :: TwoPointedList m a) ->
>          claimEqual
>            (readMark as)
>            (readMark (movePointRight as))
> 
>     , testKreb
>         "clearMark (clearMark as) == clearMark as" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (clearMark (clearMark as))
>             (clearMark as)
> 
>     , testKreb
>         "leaveMark (leaveMark as) == leaveMark as" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (leaveMark (leaveMark as))
>             (leaveMark as)
> 
>     , testKreb
>         "clearMark (leaveMark as) == clearMark as" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (clearMark (leaveMark as))
>             (clearMark as)
> 
>     , testKreb
>         "deleteAtStart (insertAtStart u as) == as" $
>         \(u :: a) (as :: TwoPointedList m a) ->
>           claimEqual
>             (as)
>             (deleteAtStart (insertAtStart u as))
> 
>     , testKreb
>         "deleteAtEnd (insertAtEnd u as) == as" $
>         \(u :: a) (as :: TwoPointedList m a) ->
>           claimEqual
>             (as)
>             (deleteAtEnd (insertAtEnd u as))
> 
>     , testKreb
>         "deleteAtStart (deleteAtEnd as) == deleteAtEnd (deleteAtStart as)" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (deleteAtEnd (deleteAtStart as))
>             (deleteAtStart (deleteAtEnd as))
> 
>     , testKreb
>         "(isEmpty as) || (insertAtStart u (insertAtEnd v as) == insertAtEnd v (insertAtStart u as))" $
>         \(u :: a) (v :: a) (as :: TwoPointedList m a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (insertAtStart u (insertAtEnd v as))
>                 (insertAtEnd v (insertAtStart u as))
>             ]
> 
>     , testKreb
>         "(isEmpty as) || (deletePointLeft (insertPointLeft u as) == as)" $
>         \(u :: a) (as :: TwoPointedList m a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (as)
>                 (deletePointLeft (insertPointLeft u as))
>             ]
> 
>     , testKreb
>         "(isEmpty as) || (deletePointRight (insertPointRight u as) == as)" $
>         \(u :: a) (as :: TwoPointedList m a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (as)
>                 (deletePointRight (insertPointRight u as))
>             ]
> 
>     , testKreb
>         "(isEmpty as) || (deletePointRight (insertPointLeft u as) == insertPointLeft u (deletePointRight as))" $
>         \(u :: a) (as :: TwoPointedList m a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (deletePointRight (insertPointLeft u as))
>                 (insertPointLeft u (deletePointRight as))
>             ]
> 
>     , testKreb
>         "(isEmpty as) || (deletePointLeft (insertPointRight u as) == insertPointRight u (deletePointLeft as))" $
>         \(u :: a) (as :: TwoPointedList m a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (deletePointLeft (insertPointRight u as))
>                 (insertPointRight u (deletePointLeft as))
>             ]
> 
>     , testKreb
>         "(isEmpty as) || (deletePointLeft (deletePointRight as) == deletePointRight (deletePointLeft as))" $
>         \(as :: TwoPointedList m a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (deletePointRight (deletePointLeft as))
>                 (deletePointLeft (deletePointRight as))
>             ]
> 
>     , testKreb
>         "(isEmpty as) || (insertPointLeft u (insertPointRight v as) == insertPointRight v (insertPointLeft u as))" $
>         \(u :: a) (v :: a) (as :: TwoPointedList m a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (insertPointLeft u (insertPointRight v as))
>                 (insertPointRight v (insertPointLeft u as))
>             ]
> 
>     , testKreb
>         "(isEmpty as) || (readPoint (insertPointLeft u as) == readPoint as)" $
>         \(u :: a) (as :: TwoPointedList m a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (readPoint as)
>                 (readPoint (insertPointLeft u as))
>             ]
> 
>     , testKreb
>         "(isEmpty as) || (readPoint (insertPointRight u as) == readPoint as)" $
>         \(u :: a) (as :: TwoPointedList m a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (readPoint as)
>                 (readPoint (insertPointRight u as))
>             ]
> 
>     , testKreb
>         "(isEmpty as) || (readPoint (insertAtStart u as) == readPoint as)" $
>         \(u :: a) (as :: TwoPointedList m a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (readPoint as)
>                 (readPoint (insertAtStart u as))
>             ]
> 
>     , testKreb
>         "(isEmpty as) || (readPoint (insertAtEnd u as) == readPoint as)" $
>         \(u :: a) (as :: TwoPointedList m a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (readPoint as)
>                 (readPoint (insertAtEnd u as))
>             ]
> 
>     , testKreb
>         "isSingleton (singleton a)" $
>         \(a :: a) ->
>           let as = singleton a :: TwoPointedList m a
>           in claimTrue (isSingleton as)
> 
>     , testKreb
>         "Just (a, as) == viewAtStart (insertAtStart a as)" $
>         \(a :: a) (as :: TwoPointedList m a) ->
>           claimEqual
>             (Just (a, as))
>             (viewAtStart (insertAtStart a as))
> 
>     , testKreb
>         "Just (a, as) == viewAtEnd (insertAtEnd a as)" $
>         \(a :: a) (as :: TwoPointedList m a) ->
>           claimEqual
>             (Just (a, as))
>             (viewAtEnd (insertAtEnd a as))
> 
>     , testKreb
>         "(isEmpty us) || (isEmpty as) || (Just (us, clearMark as) == cutRegionL (insertRegionL us (clearMark as)))" $
>         \(us :: FT.FingerTree m a) (as :: TwoPointedList m a) ->
>           claimAny
>             [ claimTrue (FT.isEmpty us)
>             , claimTrue (isEmpty as)
>             , claimEqual
>                 (Just (us, clearMark as))
>                 (cutRegionL (insertRegionL us (clearMark as)))
>             ]
> 
>     , testKreb
>         "fst <$> cutRegionL as == copyRegionL as" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (fst <$> cutRegionL as)
>             (copyRegionL as)
>     ]
