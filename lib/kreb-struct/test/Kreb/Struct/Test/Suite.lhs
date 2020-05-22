> module Kreb.Struct.Test.Suite where

> import Test.Tasty

> import Kreb.Struct.Data.FingerTree.Test
> import Kreb.Struct.Data.Deque.Test
> import Kreb.Struct.Data.Seq.Test
> import Kreb.Struct.Data.RunLengthEncoded.Test
> 
> import Kreb.Struct.Data.StemTree.Test
> import Kreb.Struct.Data.StemTree.Zipper.Test

> import Kreb.Struct.Data.FingerTree.Zipper.Test

> import Kreb.Struct.Sequence.Test
> import Kreb.Struct.RedBlackTree.Test
> import Kreb.Struct.FiniteMap.Test

> kreb_struct_test_suite :: TestTree
> kreb_struct_test_suite =
>   testGroup "All Tests"
>     [ test_FingerTree
>     , test_FingerTreeZipper
>     , test_Deque
>     , test_Seq
>     , test_RunLengthEncoded
>     , test_StemTree
>     , test_StemTreeZipper

>     , test_Sequence
>     , test_RedBlackTree
>     , test_FiniteMap
>     ]
