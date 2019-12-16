> module Kreb.Editor.Test.Suite where

> import Test.Tasty

> import Kreb.Editor.Panel.Test
> import Kreb.Editor.Mock.Test



> kreb_editor_test_suite :: TestTree
> kreb_editor_test_suite =
>   testGroup "Editor Tests"
>     [ test_Panel
>     , test_Mock
>     ]
