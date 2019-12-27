> {-# LANGUAGE ScopedTypeVariables #-}

> module Kreb.Editor.Core.Mock.Test (
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
> import Kreb.Editor.Core
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
>             [ claimFilesystemIsEmpty w1
>             , claimStdoutIsEmpty w1
>             , claimAny
>                 [ (claimStderrEquals w1 ["DEBUG Quit"])
>                     .&&. (claimNoIOErrors w1)
>                 , (claimStderrIsEmpty w1)
>                     .&&. (claimHasSomeIOError w1)
>                 ]
>             ]
>     ]
