> module Kreb.Editor.Test.Suite where

> import Test.Tasty

> import Kreb.Editor.Panel.Test



> kreb_editor_test_suite :: TestTree
> kreb_editor_test_suite =
>   testGroup "Editor Tests"
>     [ test_Panel
>     ]
