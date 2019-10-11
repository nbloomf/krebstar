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
> import Kreb.Check
> import Kreb.Struct.FingerTree
> import Kreb.Struct.TwoPointedList
> import Kreb.Struct.FingerTree.Test


> test_TwoPointedList :: TestTree
> test_TwoPointedList =
>   testGroup "TwoPointedList"
>     [ test_TwoPointedList_properties
>         "Char/Count" (Proxy :: Proxy Char) (Proxy :: Proxy Count)
>     , test_TwoPointedList_properties
>         "Bool/Tup" (Proxy :: Proxy Bool) (Proxy :: Proxy Tup)
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
>     ]





