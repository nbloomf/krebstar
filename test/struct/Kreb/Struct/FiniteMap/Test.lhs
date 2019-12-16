---
title: Kreb.Struct.FiniteMap.Test
---

> {-# LANGUAGE
>     MultiParamTypeClasses
>   , ScopedTypeVariables
>   , KindSignatures
>   , DeriveGeneric
> #-}
> 
> module Kreb.Struct.FiniteMap.Test (
>     test_FiniteMap
> ) where
> 
> import Prelude hiding (lookup)
> 
> import Data.Proxy
> import Data.List (sort, nub)
> 
> import Test.Tasty
> 
> import Kreb.Check
> import Kreb.Struct.FiniteMap

> test_FiniteMap :: TestTree
> test_FiniteMap = testGroup "Finite Map"
>   [ test_FiniteMap_properties "Bool/Char" (Proxy :: Proxy Bool) (Proxy :: Proxy Char)
>   , test_FiniteMap_properties "Int/Char" (Proxy :: Proxy Int) (Proxy :: Proxy Char)
>   ]

> test_FiniteMap_properties
>   :: forall k v
>    . ( Ord k, Arb k, Show k, Prune k, Eq v, Arb v, Show v, Prune v )
>   => String -> Proxy k -> Proxy v -> TestTree
> test_FiniteMap_properties name _ _ =
>   testGroup ("FiniteMap: " ++ name)
>     [ testKreb
>         "isEmpty empty == True" $
>         claimTrue (isEmpty (empty :: FiniteMap k v))
> 
>     , testKreb
>         "isEmpty (singleton a) == False" $
>         \(k :: k) (v :: v) ->
>           claimFalse (isEmpty (singleton k v))
> 
>     , testKreb
>         "fromList (toList as) == as" $
>         \(as :: FiniteMap k v) ->
>           claimEqual (fromList (toList as)) as
> 
>     , testKreb
>         "lookup k (singleton k v) == Just v" $
>         \(k :: k) (v :: v) ->
>           claimEqual (lookup k (singleton k v)) (Just v)
> 
>     , testKreb
>         "defineAt k v1 (singleton k v2) == Nothing" $
>         \(k :: k) (v1 :: v) (v2 :: v) ->
>           claimEqual (defineAt k v1 (singleton k v2)) Nothing
> 
>     , testKreb
>         "undefineAt k (singleton k v) == Just empty" $
>         \(k :: k) (v :: v) ->
>           claimEqual (undefineAt k (singleton k v)) (Just empty)
> 
>     , testKreb
>         "redefineAt k v2 (singleton k v1) == Just (singleton k v2)" $
>         \(k :: k) (v1 :: v) (v2 :: v) ->
>           claimEqual (redefineAt k v2 (singleton k v1)) (Just (singleton k v2))
>     ]
