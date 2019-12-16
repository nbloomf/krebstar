> {-# LANGUAGE ScopedTypeVariables #-}

> module Kreb.Editor.Mock.Test (
>     test_Mock
> ) where
> 
> import Data.Proxy
> import Data.Function ((&))
> 
> import Test.Tasty
> 
> import Kreb.Check
> import Kreb.Text
> import Kreb.Editor
> import Kreb.Effect
> import Kreb.Control.Identity

> test_Mock :: TestTree
> test_Mock =
>   testGroup "Mock"
>     [ testKreb
>         "Empty session" $
>         \(chi :: Chaos) ->
>           let
>             w0 = initMockWorld [[Quit]]
>             w1 = unIdentity $
>               runMockEditor chi w0 (EventId 0 "test") "stdlib.txt" (30,20)
>           in claimAll
>             [ claimTrueBecause "Filesystem should be empty"
>                 $ _filesystem_is_empty w1
>             , claimTrueBecause "stdout should be empty"
>                 $ _stdout_is_empty w1
>             ]
>     ]
