---
title: Pointed Rose Trees -- Testing
---

> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE FlexibleContexts #-}
> 
> module Kreb.Struct.PointedRoseTree.Test (
>     test_PointedRoseTree
> ) where
> 
> import Prelude hiding (reverse)
> import Data.Proxy
> import Data.Foldable
> 
> import Test.Tasty
> 
> import Kreb.Check
> import Kreb.Struct.PointedRoseTree

> test_PointedRoseTree :: TestTree
> test_PointedRoseTree = testGroup "PointedRoseTree"
>   [ test_PointedRoseTree_properties "Bool" (Proxy :: Proxy Bool)
>   , test_PointedRoseTree_properties "Int" (Proxy :: Proxy Int)
>   ]



> test_PointedRoseTree_properties
>   :: forall a
>    . ( Arb a, Eq a, Show a, Prune a, MakeTo a, CoArb a )
>   => String -> Proxy a -> TestTree
> test_PointedRoseTree_properties name _ =
>   let title = "PointedRoseTree (" ++ name ++ ")" in
>   testGroup title
>     [ testKreb
>         "rewind (rewind as) == rewind as" $
>         \(as :: PointedRoseTree a) ->
>           claimEqual
>             (rewind as)
>             (rewind (rewind as))
>     ]
