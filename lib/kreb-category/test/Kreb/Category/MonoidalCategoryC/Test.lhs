> module Kreb.Category.MonoidalCategoryC.Test where

> import Data.Proxy
> import Data.Void

> import Test.Tasty



> import Kreb.Category

> test_MonoidalCategoryC :: TestTree
> test_MonoidalCategoryC = testGroup "MonoidalCategoryC"
>   [ testGroup "Hask with (MapOn Hask) and (PairC Hask Hask)"
>     [ test_MonoidalCategoryC_laws
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy (PairC Hask Hask)) (Proxy :: Proxy ())
>         (Proxy :: Proxy Bool) (Proxy :: Proxy Bool)
>         (Proxy :: Proxy Bool) (Proxy :: Proxy Bool)
>         (Proxy :: Proxy ())
>         (Proxy :: Proxy ((),()))
>         (Proxy :: Proxy ((),()))
>         (Proxy :: Proxy ((),((),())))
>         (Proxy :: Proxy (((),()),()))
>         (Proxy :: Proxy ((),()))
>         (Proxy :: Proxy ((((),()),()),()))
>         (Proxy :: Proxy ((),((),((),()))))
> 
>     , test_MonoidalCategoryC_laws
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy (PairC Hask Hask)) (Proxy :: Proxy ())
>         (Proxy :: Proxy Char) (Proxy :: Proxy Char)
>         (Proxy :: Proxy Char) (Proxy :: Proxy Char)
>         (Proxy :: Proxy ())
>         (Proxy :: Proxy ((),()))
>         (Proxy :: Proxy ((),()))
>         (Proxy :: Proxy ((),((),())))
>         (Proxy :: Proxy (((),()),()))
>         (Proxy :: Proxy ((),()))
>         (Proxy :: Proxy ((((),()),()),()))
>         (Proxy :: Proxy ((),((),((),()))))
> 
>     , test_MonoidalCategoryC_laws
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy (PairC Hask Hask)) (Proxy :: Proxy ())
>         (Proxy :: Proxy Int) (Proxy :: Proxy Int)
>         (Proxy :: Proxy Int) (Proxy :: Proxy Int)
>         (Proxy :: Proxy ())
>         (Proxy :: Proxy ((),()))
>         (Proxy :: Proxy ((),()))
>         (Proxy :: Proxy ((),((),())))
>         (Proxy :: Proxy (((),()),()))
>         (Proxy :: Proxy ((),()))
>         (Proxy :: Proxy ((((),()),()),()))
>         (Proxy :: Proxy ((),((),((),()))))
>     ]
> 
>   , testGroup "Hask with (MapOn Hask) and (ChoiceC Hask Hask)"
>     [ test_MonoidalCategoryC_laws
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy (ChoiceC Hask Hask)) (Proxy :: Proxy Void)
>         (Proxy :: Proxy Bool) (Proxy :: Proxy Bool)
>         (Proxy :: Proxy Bool) (Proxy :: Proxy Bool)
>         (Proxy :: Proxy ())
>         (Proxy :: Proxy ((),()))
>         (Proxy :: Proxy ((),()))
>         (Proxy :: Proxy ((),((),())))
>         (Proxy :: Proxy (((),()),()))
>         (Proxy :: Proxy ((),()))
>         (Proxy :: Proxy ((((),()),()),()))
>         (Proxy :: Proxy ((),((),((),()))))
> 
>     , test_MonoidalCategoryC_laws
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy (ChoiceC Hask Hask)) (Proxy :: Proxy Void)
>         (Proxy :: Proxy Char) (Proxy :: Proxy Char)
>         (Proxy :: Proxy Char) (Proxy :: Proxy Char)
>         (Proxy :: Proxy ())
>         (Proxy :: Proxy ((),()))
>         (Proxy :: Proxy ((),()))
>         (Proxy :: Proxy ((),((),())))
>         (Proxy :: Proxy (((),()),()))
>         (Proxy :: Proxy ((),()))
>         (Proxy :: Proxy ((((),()),()),()))
>         (Proxy :: Proxy ((),((),((),()))))
> 
>     , test_MonoidalCategoryC_laws
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy (ChoiceC Hask Hask)) (Proxy :: Proxy Void)
>         (Proxy :: Proxy Int) (Proxy :: Proxy Int)
>         (Proxy :: Proxy Int) (Proxy :: Proxy Int)
>         (Proxy :: Proxy ())
>         (Proxy :: Proxy ((),()))
>         (Proxy :: Proxy ((),()))
>         (Proxy :: Proxy ((),((),())))
>         (Proxy :: Proxy (((),()),()))
>         (Proxy :: Proxy ((),()))
>         (Proxy :: Proxy ((((),()),()),()))
>         (Proxy :: Proxy ((),((),((),()))))
>     ]
>   ]
