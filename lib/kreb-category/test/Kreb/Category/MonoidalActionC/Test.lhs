> module Kreb.Category.MonoidalActionC.Test where

> import Data.Proxy
> import Data.Void

> import Test.Tasty



> import Kreb.Category

> test_MonoidalActionC :: TestTree
> test_MonoidalActionC = testGroup "MonoidalActionC"
>   [ testGroup "Hask acting on Hask via (PairC Hask Hask)"
>     [ test_MonoidalActionC_laws
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy (PairC Hask Hask)) (Proxy :: Proxy ())
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy (PairC Hask Hask))
>         (Proxy :: Proxy Bool) (Proxy :: Proxy Bool)
>         (Proxy :: Proxy Bool) (Proxy :: Proxy Bool)
>         (Proxy :: Proxy ())
>         (Proxy :: Proxy ((),()))
>         (Proxy :: Proxy ((),((),())))
>         (Proxy :: Proxy (((),()),()))
>         (Proxy :: Proxy ((((),()),()),()))
>         (Proxy :: Proxy ((),((),((),()))))
> 
>     , test_MonoidalActionC_laws
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy (PairC Hask Hask)) (Proxy :: Proxy ())
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy (PairC Hask Hask))
>         (Proxy :: Proxy Char) (Proxy :: Proxy Char)
>         (Proxy :: Proxy Char) (Proxy :: Proxy Char)
>         (Proxy :: Proxy ())
>         (Proxy :: Proxy ((),()))
>         (Proxy :: Proxy ((),((),())))
>         (Proxy :: Proxy (((),()),()))
>         (Proxy :: Proxy ((((),()),()),()))
>         (Proxy :: Proxy ((),((),((),()))))
> 
>     , test_MonoidalActionC_laws
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy (PairC Hask Hask)) (Proxy :: Proxy ())
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy (PairC Hask Hask))
>         (Proxy :: Proxy Int) (Proxy :: Proxy Int)
>         (Proxy :: Proxy Int) (Proxy :: Proxy Int)
>         (Proxy :: Proxy ())
>         (Proxy :: Proxy ((),()))
>         (Proxy :: Proxy ((),((),())))
>         (Proxy :: Proxy (((),()),()))
>         (Proxy :: Proxy ((((),()),()),()))
>         (Proxy :: Proxy ((),((),((),()))))
>     ]
> 
>   , testGroup "Hask acting on Hask via (ChoiceC Hask Hask)"
>     [ test_MonoidalActionC_laws
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy (ChoiceC Hask Hask)) (Proxy :: Proxy Void)
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy (ChoiceC Hask Hask))
>         (Proxy :: Proxy Bool) (Proxy :: Proxy Bool)
>         (Proxy :: Proxy Bool) (Proxy :: Proxy Bool)
>         (Proxy :: Proxy ())
>         (Proxy :: Proxy ((),()))
>         (Proxy :: Proxy ((),((),())))
>         (Proxy :: Proxy (((),()),()))
>         (Proxy :: Proxy ((((),()),()),()))
>         (Proxy :: Proxy ((),((),((),()))))
> 
>     , test_MonoidalActionC_laws
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy (ChoiceC Hask Hask)) (Proxy :: Proxy Void)
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy (ChoiceC Hask Hask))
>         (Proxy :: Proxy Char) (Proxy :: Proxy Char)
>         (Proxy :: Proxy Char) (Proxy :: Proxy Char)
>         (Proxy :: Proxy ())
>         (Proxy :: Proxy ((),()))
>         (Proxy :: Proxy ((),((),())))
>         (Proxy :: Proxy (((),()),()))
>         (Proxy :: Proxy ((((),()),()),()))
>         (Proxy :: Proxy ((),((),((),()))))
> 
>     , test_MonoidalActionC_laws
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy (ChoiceC Hask Hask)) (Proxy :: Proxy Void)
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy (ChoiceC Hask Hask))
>         (Proxy :: Proxy Int) (Proxy :: Proxy Int)
>         (Proxy :: Proxy Int) (Proxy :: Proxy Int)
>         (Proxy :: Proxy ())
>         (Proxy :: Proxy ((),()))
>         (Proxy :: Proxy ((),((),())))
>         (Proxy :: Proxy (((),()),()))
>         (Proxy :: Proxy ((((),()),()),()))
>         (Proxy :: Proxy ((),((),((),()))))
>     ]
>   ]
