> module Kreb.Lang.Test.Suite where

> import Test.Tasty

> import Kreb.Lang.Type.Test
> import Kreb.Lang.Module.Test
> import Kreb.Lang.Interpreter.Test
> import Kreb.Lang.Parser.Test



> kreb_lang_test_suite :: TestTree
> kreb_lang_test_suite =
>   testGroup "Lang"
>     [ test_Type
>     , test_Module
>     , test_Interpreter
>     , test_Parser
>     ]
