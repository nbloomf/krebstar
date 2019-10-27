---
title: Rose Trees -- Testing
---

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
>   [
>   ]