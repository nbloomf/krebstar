---
title: Rose Trees -- Testing
---

> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE FlexibleContexts #-}
> 
> module Kreb.Struct.RoseTree.Test (
>     test_RoseTree
> ) where
> 
> import Prelude hiding (reverse)
> import Data.Proxy
> import Data.Foldable
> 
> import Test.Tasty
> 
> import Kreb.Check
> import Kreb.Struct.RoseTree

> test_RoseTree :: TestTree
> test_RoseTree = testGroup "RoseTree"
>   [ test_RoseTree_properties "Bool" (Proxy :: Proxy Bool)
>   , test_RoseTree_properties "Int" (Proxy :: Proxy Int)
>   ]



> test_RoseTree_properties
>   :: forall a
>    . ( Arb a, Eq a, Show a, Prune a, MakeTo a, CoArb a )
>   => String -> Proxy a -> TestTree
> test_RoseTree_properties name _ =
>   let title = "RoseTree (" ++ name ++ ")" in
>   testGroup title
>     [ testKreb
>         "isSingleton (singleton a) == True" $
>         \(a :: a) ->
>           claimTrue (isSingleton (singleton a))
> 
>     , testKreb
>         "fmap f (spineFromList a as) == spineFromList (f a) (fmap f as)" $
>         \(f :: Fun a a) (a :: a) (as :: [a]) ->
>           claimEqual
>             (fmap (apFun f) (spineFromList a as))
>             (spineFromList (apFun f a) (fmap (apFun f) as))
> 
>     , testKreb
>         "fmap f (levelFromList a as) == levelFromList (f a) (fmap f as)" $
>         \(f :: Fun a a) (a :: a) (as :: [a]) ->
>           claimEqual
>             (fmap (apFun f) (levelFromList a as))
>             (levelFromList (apFun f a) (fmap (apFun f) as))
>     ]
