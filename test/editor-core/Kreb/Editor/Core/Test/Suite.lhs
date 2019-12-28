> module Kreb.Editor.Core.Test.Suite where

> import Test.Tasty

> import Kreb.Editor.Core.Data.Panel.Test
> import Kreb.Editor.Core.Mock.Test



> kreb_editor_test_suite :: TestTree
> kreb_editor_test_suite =
>   testGroup "Editor Core Tests"
>     [ test_Panel
>     , test_Mock
>     ]
