---
title: Pointed Rose Trees -- Testing
---

> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE FlexibleContexts #-}
> 
> module Kreb.Struct.Data.StemTree.Zipper.Test (
>     test_StemTreeZipper
> ) where
> 
> import Prelude hiding (reverse)
> import Data.Proxy
> import Data.Foldable
> 
> import Test.Tasty
> 
> import Kreb.Prop
> import Kreb.Struct.Class
> import Kreb.Struct.Data.StemTree.Zipper

> test_StemTreeZipper :: TestTree
> test_StemTreeZipper = testGroup "StemTreeZipper"
>   [ test_StemTreeZipper_properties "Bool" (Proxy :: Proxy Bool)
>   , test_StemTreeZipper_properties "Int" (Proxy :: Proxy Int)
> 
>   , testGroup "Class Laws"
>     [ testGroup "Zipper"
>       [ test_Zipper_laws "StemTreeZipper" (Proxy :: Proxy StemTreeZipper) (Proxy :: Proxy Char)
>       ]
>     ]
>   ]



> test_StemTreeZipper_properties
>   :: forall a
>    . ( Arb a, Eq a, Show a, Prune a, MakeTo a, CoArb a )
>   => String -> Proxy a -> TestTree
> test_StemTreeZipper_properties name _ =
>   let title = "StemTreeZipper (" ++ name ++ ")" in
>   testGroup title []

> {-

>     [ krebProp
>         "rewind (rewind as) == rewind as" $
>         \(as :: PointedRoseTree a) ->
>           claimEqual
>             (rewind as)
>             (rewind (rewind as))
>     ]

> -}
