> module Kreb.Text.Test.Suite (
>     kreb_text_test_suite
> ) where

> import Test.Tasty

> import Kreb.Text.Buffer.Test
> import Kreb.Text.TextBox.Test
> import Kreb.Text.MeasureText.Test
> import Kreb.Text.ScreenOffset.Test

> kreb_text_test_suite :: TestTree
> kreb_text_test_suite =
>   testGroup "kreb-text"
>     [ test_ScreenOffset
>     , test_MeasureText
>     , test_Buffer
>  --   , test_TextBox
>     ]
