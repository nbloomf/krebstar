---
title: Integrating with Tasty
author: nbloomf
---

::: frontmatter

> module Kreb.Unit.Tasty where
> 
> import Data.Typeable
> 
> import Test.Tasty
> import Test.Tasty.Options
> import Test.Tasty.Providers
> 
> import Kreb.Unit.Declare
> import Kreb.Unit.Declarable

:::


> newtype KU = KU Declaration
>   deriving Typeable

> instance IsTest KU where
>   testOptions = return []
> 
>   run _ (KU dec) _ = do
>     result <- testDeclaration dec
>     return $ case result of
>       Nothing -> testPassed ""
>       Just err -> testFailed err

> krebUnit
>   :: ( Declarable assert )
>   => TestName -> assert -> TestTree
> krebUnit name dec =
>   singleTest name $ KU $ declare dec
