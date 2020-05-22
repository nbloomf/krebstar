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
> import           Kreb.Prop
> import           Kreb.Struct.Valued
> import qualified Kreb.Struct.FingerTree as FT
> import           Kreb.Struct.TwoPointedList
> import           Kreb.Struct.FingerTree.Test


> test_TwoPointedList :: TestTree
> test_TwoPointedList =
>   testGroup "TwoPointedList"
>     [
>     ]

> {-

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
>           (fmapTPL :: (Bool -> Bool) -> TwoPointedList Count Bool -> TwoPointedList Count Bool)
>           (fmapTPL :: (Bool -> Bool) -> TwoPointedList Count Bool -> TwoPointedList Count Bool)
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
>     [ krebProp
>         "isEmpty (singleton a) == False" $
>         \(a :: a) ->
>           let as = singleton a :: TwoPointedList m a
>           in claimFalse (isEmpty as)
> 
>     , krebProp
>         "movePointToStart (movePointToStart as) == movePointToStart as" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (movePointToStart (movePointToStart as))
>             (movePointToStart as)
> 
>     , krebProp
>         "movePointToEnd (movePointToEnd as) == movePointToEnd as" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (movePointToEnd (movePointToEnd as))
>             (movePointToEnd as)
> 
>     , krebProp
>         "moveMarkToStart (moveMarkToStart as) == moveMarkToStart as" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (moveMarkToStart (moveMarkToStart as))
>             (moveMarkToStart as)
> 
>     , krebProp
>         "moveMarkToEnd (moveMarkToEnd as) == moveMarkToEnd as" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (moveMarkToEnd (moveMarkToEnd as))
>             (moveMarkToEnd as)
> 
>     , krebProp
>         "(isPointAtEnd as) || (as == movePointLeft (movePointRight as))" $
>         \(as :: TwoPointedList m a) ->
>           claimAny
>             [ claimTrue (isPointAtEnd as)
>             , claimEqual
>                 (as)
>                 (movePointLeft (movePointRight as))
>             ]
> 
>     , krebProp
>         "(isPointAtStart as) || (as == movePointRight (movePointLeft as))" $
>         \(as :: TwoPointedList m a) ->
>           claimAny
>             [ claimTrue (isPointAtStart as)
>             , claimEqual
>                 (as)
>                 (movePointRight (movePointLeft as))
>             ]
> 
>     , krebProp
>         "(isMarkAtEnd as) || (as == moveMarkLeft (moveMarkRight as))" $
>         \(as :: TwoPointedList m a) ->
>           claimAny
>             [ claimTrue (isMarkAtEnd as)
>             , claimEqual
>                 (as)
>                 (moveMarkLeft (moveMarkRight as))
>             ]
> 
>     , krebProp
>         "(isMarkAtStart as) || (as == moveMarkRight (moveMarkLeft as))" $
>         \(as :: TwoPointedList m a) ->
>           claimAny
>             [ claimTrue (isMarkAtStart as)
>             , claimEqual
>                 (as)
>                 (moveMarkRight (moveMarkLeft as))
>             ]
> 
>     , krebProp
>         "movePointToStart (moveMarkToStart as) == moveMarkToStart (movePointToStart as)" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (movePointToStart (moveMarkToStart as))
>             (moveMarkToStart (movePointToStart as))
> 
>     , krebProp
>         "movePointToStart (moveMarkToEnd as) == moveMarkToEnd (movePointToStart as)" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (movePointToStart (moveMarkToEnd as))
>             (moveMarkToEnd (movePointToStart as))
> 
>     , krebProp
>         "movePointToEnd (moveMarkToStart as) == moveMarkToStart (movePointToEnd as)" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (movePointToEnd (moveMarkToStart as))
>             (moveMarkToStart (movePointToEnd as))
> 
>     , krebProp
>         "movePointToEnd (moveMarkToEnd as) == moveMarkToEnd (movePointToEnd as)" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (movePointToEnd (moveMarkToEnd as))
>             (moveMarkToEnd (movePointToEnd as))
> 
>     , krebProp
>         "movePointLeft (moveMarkLeft as) == moveMarkLeft (movePointLeft as)" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (movePointLeft (moveMarkLeft as))
>             (moveMarkLeft (movePointLeft as))
> 
>     , krebProp
>         "movePointLeft (moveMarkRight as) == moveMarkRight (movePointLeft as)" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (movePointLeft (moveMarkRight as))
>             (moveMarkRight (movePointLeft as))
> 
>     , krebProp
>         "movePointRight (moveMarkLeft as) == moveMarkLeft (movePointRight as)" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (movePointRight (moveMarkLeft as))
>             (moveMarkLeft (movePointRight as))
> 
>     , krebProp
>         "movePointRight (moveMarkRight as) == moveMarkRight (movePointRight as)" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (movePointRight (moveMarkRight as))
>             (moveMarkRight (movePointRight as))
> 
>     , krebProp
>         "movePointLeft (moveMarkToStart as) == moveMarkToStart (movePointLeft as)" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (movePointLeft (moveMarkToStart as))
>             (moveMarkToStart (movePointLeft as))
> 
>     , krebProp
>         "movePointRight (moveMarkToStart as) == moveMarkToStart (movePointRight as)" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (movePointRight (moveMarkToStart as))
>             (moveMarkToStart (movePointRight as))
> 
>     , krebProp
>         "movePointLeft (moveMarkToEnd as) == moveMarkToEnd (movePointLeft as)" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (movePointLeft (moveMarkToEnd as))
>             (moveMarkToEnd (movePointLeft as))
> 
>     , krebProp
>         "movePointRight (moveMarkToEnd as) == moveMarkToEnd (movePointRight as)" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (movePointRight (moveMarkToEnd as))
>             (moveMarkToEnd (movePointRight as))
> 
>     , krebProp
>         "movePointToStart (moveMarkLeft as) == moveMarkLeft (movePointToStart as)" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (movePointToStart (moveMarkLeft as))
>             (moveMarkLeft (movePointToStart as))
> 
>     , krebProp
>         "movePointToStart (moveMarkRight as) == moveMarkRight (movePointToStart as)" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (movePointToStart (moveMarkRight as))
>             (moveMarkRight (movePointToStart as))
> 
>     , krebProp
>         "movePointToEnd (moveMarkLeft as) == moveMarkLeft (movePointToEnd as)" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (movePointToEnd (moveMarkLeft as))
>             (moveMarkLeft (movePointToEnd as))
> 
>     , krebProp
>         "movePointToEnd (moveMarkRight as) == moveMarkRight (movePointToEnd as)" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (movePointToEnd (moveMarkRight as))
>             (moveMarkRight (movePointToEnd as))
> 
>     , krebProp
>        "readPoint (moveMarkToStart as) == readPoint as" $
>        \(as :: TwoPointedList m a) ->
>          claimEqual
>            (readPoint as)
>            (readPoint (moveMarkToStart as))
> 
>     , krebProp
>        "readPoint (moveMarkToEnd as) == readPoint as" $
>        \(as :: TwoPointedList m a) ->
>          claimEqual
>            (readPoint as)
>            (readPoint (moveMarkToEnd as))
> 
>     , krebProp
>        "readPoint (moveMarkLeft as) == readPoint as" $
>        \(as :: TwoPointedList m a) ->
>          claimEqual
>            (readPoint as)
>            (readPoint (moveMarkLeft as))
> 
>     , krebProp
>        "readPoint (moveMarkRight as) == readPoint as" $
>        \(as :: TwoPointedList m a) ->
>          claimEqual
>            (readPoint as)
>            (readPoint (moveMarkRight as))
> 
>     , krebProp
>        "readMark (movePointToStart as) == readMark as" $
>        \(as :: TwoPointedList m a) ->
>          claimEqual
>            (readMark as)
>            (readMark (movePointToStart as))
> 
>     , krebProp
>        "readMark (movePointToEnd as) == readMark as" $
>        \(as :: TwoPointedList m a) ->
>          claimEqual
>            (readMark as)
>            (readMark (movePointToEnd as))
> 
>     , krebProp
>        "readMark (movePointLeft as) == readMark as" $
>        \(as :: TwoPointedList m a) ->
>          claimEqual
>            (readMark as)
>            (readMark (movePointLeft as))
> 
>     , krebProp
>        "readMark (movePointRight as) == readMark as" $
>        \(as :: TwoPointedList m a) ->
>          claimEqual
>            (readMark as)
>            (readMark (movePointRight as))
> 
>     , krebProp
>         "clearMark (clearMark as) == clearMark as" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (clearMark (clearMark as))
>             (clearMark as)
> 
>     , krebProp
>         "leaveMark (leaveMark as) == leaveMark as" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (leaveMark (leaveMark as))
>             (leaveMark as)
> 
>     , krebProp
>         "clearMark (leaveMark as) == clearMark as" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (clearMark (leaveMark as))
>             (clearMark as)
> 
>     , krebProp
>         "deleteAtStart (insertAtStart u as) == as" $
>         \(u :: a) (as :: TwoPointedList m a) ->
>           claimEqual
>             (as)
>             (deleteAtStart (insertAtStart u as))
> 
>     , krebProp
>         "deleteAtEnd (insertAtEnd u as) == as" $
>         \(u :: a) (as :: TwoPointedList m a) ->
>           claimEqual
>             (as)
>             (deleteAtEnd (insertAtEnd u as))
> 
>     , krebProp
>         "deleteAtStart (deleteAtEnd as) == deleteAtEnd (deleteAtStart as)" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (deleteAtEnd (deleteAtStart as))
>             (deleteAtStart (deleteAtEnd as))
> 
>     , krebProp
>         "(isEmpty as) || (insertAtStart u (insertAtEnd v as) == insertAtEnd v (insertAtStart u as))" $
>         \(u :: a) (v :: a) (as :: TwoPointedList m a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (insertAtStart u (insertAtEnd v as))
>                 (insertAtEnd v (insertAtStart u as))
>             ]
> 
>     , krebProp
>         "(isEmpty as) || (deletePointLeft (insertPointLeft u as) == as)" $
>         \(u :: a) (as :: TwoPointedList m a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (as)
>                 (deletePointLeft (insertPointLeft u as))
>             ]
> 
>     , krebProp
>         "(isEmpty as) || (deletePointRight (insertPointRight u as) == as)" $
>         \(u :: a) (as :: TwoPointedList m a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (as)
>                 (deletePointRight (insertPointRight u as))
>             ]
> 
>     , krebProp
>         "(isEmpty as) || (deletePointRight (insertPointLeft u as) == insertPointLeft u (deletePointRight as))" $
>         \(u :: a) (as :: TwoPointedList m a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (deletePointRight (insertPointLeft u as))
>                 (insertPointLeft u (deletePointRight as))
>             ]
> 
>     , krebProp
>         "(isEmpty as) || (deletePointLeft (insertPointRight u as) == insertPointRight u (deletePointLeft as))" $
>         \(u :: a) (as :: TwoPointedList m a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (deletePointLeft (insertPointRight u as))
>                 (insertPointRight u (deletePointLeft as))
>             ]
> 
>     , krebProp
>         "(isEmpty as) || (deletePointLeft (deletePointRight as) == deletePointRight (deletePointLeft as))" $
>         \(as :: TwoPointedList m a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (deletePointRight (deletePointLeft as))
>                 (deletePointLeft (deletePointRight as))
>             ]
> 
>     , krebProp
>         "(isEmpty as) || (insertPointLeft u (insertPointRight v as) == insertPointRight v (insertPointLeft u as))" $
>         \(u :: a) (v :: a) (as :: TwoPointedList m a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (insertPointLeft u (insertPointRight v as))
>                 (insertPointRight v (insertPointLeft u as))
>             ]
> 
>     , krebProp
>         "(isEmpty as) || (readPoint (insertPointLeft u as) == readPoint as)" $
>         \(u :: a) (as :: TwoPointedList m a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (readPoint as)
>                 (readPoint (insertPointLeft u as))
>             ]
> 
>     , krebProp
>         "(isEmpty as) || (readPoint (insertPointRight u as) == readPoint as)" $
>         \(u :: a) (as :: TwoPointedList m a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (readPoint as)
>                 (readPoint (insertPointRight u as))
>             ]
> 
>     , krebProp
>         "(isEmpty as) || (readPoint (insertAtStart u as) == readPoint as)" $
>         \(u :: a) (as :: TwoPointedList m a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (readPoint as)
>                 (readPoint (insertAtStart u as))
>             ]
> 
>     , krebProp
>         "(isEmpty as) || (readPoint (insertAtEnd u as) == readPoint as)" $
>         \(u :: a) (as :: TwoPointedList m a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (readPoint as)
>                 (readPoint (insertAtEnd u as))
>             ]
> 
>     , krebProp
>         "isSingleton (singleton a)" $
>         \(a :: a) ->
>           let as = singleton a :: TwoPointedList m a
>           in claimTrue (isSingleton as)
> 
>     , krebProp
>         "Just (a, as) == viewAtStart (insertAtStart a as)" $
>         \(a :: a) (as :: TwoPointedList m a) ->
>           claimEqual
>             (Just (a, as))
>             (viewAtStart (insertAtStart a as))
> 
>     , krebProp
>         "Just (a, as) == viewAtEnd (insertAtEnd a as)" $
>         \(a :: a) (as :: TwoPointedList m a) ->
>           claimEqual
>             (Just (a, as))
>             (viewAtEnd (insertAtEnd a as))
> 
>     , krebProp
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
>     , krebProp
>         "fst <$> cutRegionL as == copyRegionL as" $
>         \(as :: TwoPointedList m a) ->
>           claimEqual
>             (fst <$> cutRegionL as)
>             (copyRegionL as)
> 
>     , krebProp
>         "prependFT as (prependFT bs xs) == prependFT (as <> bs) xs" $
>         \(as :: FT.FingerTree m a)
>          (bs :: FT.FingerTree m a)
>          (xs :: TwoPointedList m a) ->
>           claimEqual
>             (prependFT as (prependFT bs xs))
>             (prependFT (as <> bs) xs)
> 
>     , krebProp
>         "appendFT as (appendFT bs xs) == appendFT (bs <> as) xs" $
>         \(as :: FT.FingerTree m a)
>          (bs :: FT.FingerTree m a)
>          (xs :: TwoPointedList m a) ->
>           claimEqual
>             (appendFT as (appendFT bs xs))
>             (appendFT (bs <> as) xs)
> 
>     , krebProp
>         "prependFT mempty xs == xs" $
>         \(xs :: TwoPointedList m a) ->
>           claimEqual
>             (xs)
>             (prependFT mempty xs)
> 
>     , krebProp
>         "appendFT mempty xs == xs" $
>         \(xs :: TwoPointedList m a) ->
>           claimEqual
>             (xs)
>             (appendFT mempty xs)
>     ]

> -}
