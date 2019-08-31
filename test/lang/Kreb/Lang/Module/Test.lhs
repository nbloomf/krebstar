> module Kreb.Lang.Module.Test (
>     test_Module
> ) where

> import Prelude hiding (Word)

> import qualified Data.Map as M
> import qualified Data.Set as S

> import Data.Proxy

> import Test.QuickCheck
> import Test.Tasty
> import Test.Tasty.QuickCheck

> import Kreb.Lang.Type
> import Kreb.Lang.Expr

> import Kreb.Check



> test_Module :: TestTree
> test_Module =
>   testGroup "Module"
>     [
>     ]
