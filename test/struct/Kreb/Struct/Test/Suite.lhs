> module Kreb.Struct.Test.Suite where

> import Test.Tasty

> import Kreb.Struct.FingerTree.Test
> import Kreb.Struct.OnePointedList.Test
> import Kreb.Struct.Seq.Test
> import Kreb.Struct.RunLengthEncoding.Test

> kreb_struct_test_suite :: TestTree
> kreb_struct_test_suite =
>   testGroup "All Tests"
>     [ test_FingerTree
>     , test_OnePointedList
>     , test_Seq
>     , test_RunLengthEncoding
>     ]