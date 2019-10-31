> module Kreb.Struct.Test.Suite where

> import Test.Tasty

> import Kreb.Struct.FingerTree.Test
> import Kreb.Struct.OnePointedList.Test
> import Kreb.Struct.TwoPointedList.Test
> import Kreb.Struct.Sequence.Test
> import Kreb.Struct.RunLengthEncoded.Test
> import Kreb.Struct.RoseTree.Test
> import Kreb.Struct.PointedRoseTree.Test

> kreb_struct_test_suite :: TestTree
> kreb_struct_test_suite =
>   testGroup "All Tests"
>     [ test_FingerTree
>     , test_OnePointedList
>     , test_TwoPointedList
>     , test_Sequence
>     , test_RunLengthEncoded
>     , test_RoseTree
>     , test_PointedRoseTree
>     ]