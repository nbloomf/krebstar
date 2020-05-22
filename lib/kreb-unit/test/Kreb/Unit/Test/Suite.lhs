> {-# LANGUAGE
>     TypeFamilies
>   , ScopedTypeVariables
>   , FlexibleInstances
>   , DataKinds
> #-}

> module Kreb.Unit.Test.Suite (
>     kreb_unit_test_suite
> ) where

> import Data.Proxy

> import Test.Tasty

> import Kreb.Unit


> kreb_unit_test_suite :: TestTree
> kreb_unit_test_suite =
>   testGroup "All Tests"
>     [ testGroup "Bool"
>       [ krebUnit "Bool" $
>           True
> 
>       , krebUnit "declareTrue" $
>           declareTrue True
> 
>       , krebUnit "declareFalse" $
>           declareFalse False
>       ]
> 
>     , testGroup "IO"
>       [ krebUnit "Expect SomeException" $
>           declareRaisesSomeException (raiseMsgIO "FOOO")
>       ]
>     ]

