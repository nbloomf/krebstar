> module Kreb.Category.Test.Suite where

> import Test.Tasty

> import Kreb.Category.CategoryC.Test
> import Kreb.Category.BifunctorC.Test
> import Kreb.Category.ProfunctorC.Test
> import Kreb.Category.MonoidalCategoryC.Test
> import Kreb.Category.MonoidalActionC.Test

> kreb_category_test_suite :: TestTree
> kreb_category_test_suite =
>   testGroup "All Tests"
>     [ test_CategoryC
>     , test_BifunctorC
>     , test_ProfunctorC
>     , test_MonoidalCategoryC
>     , test_MonoidalActionC
>     ]
