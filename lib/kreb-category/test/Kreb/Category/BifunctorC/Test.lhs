> module Kreb.Category.BifunctorC.Test where
> 
> import Data.Proxy
> import Test.Tasty
> import Kreb.Category



> test_BifunctorC :: TestTree
> test_BifunctorC = testGroup "BifunctorC"
>   [ testGroup "(Hask, MapOn Hask) and (Hask, MapOn Hask) with PairC"
>     [ test_BifunctorC_laws
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy (PairC Hask Hask))
>         (Proxy :: Proxy Bool) (Proxy :: Proxy Bool)
>         (Proxy :: Proxy Bool) (Proxy :: Proxy Bool)
>         (Proxy :: Proxy Bool) (Proxy :: Proxy Bool)
>         (Proxy :: Proxy ((),()))
> 
>     , test_BifunctorC_laws
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy (PairC Hask Hask))
>         (Proxy :: Proxy Char) (Proxy :: Proxy Char)
>         (Proxy :: Proxy Char) (Proxy :: Proxy Char)
>         (Proxy :: Proxy Char) (Proxy :: Proxy Char)
>         (Proxy :: Proxy ((),()))
> 
>     , test_BifunctorC_laws
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy (PairC Hask Hask))
>         (Proxy :: Proxy Int) (Proxy :: Proxy Int)
>         (Proxy :: Proxy Int) (Proxy :: Proxy Int)
>         (Proxy :: Proxy Int) (Proxy :: Proxy Int)
>         (Proxy :: Proxy ((),()))
>     ]
> 
>   , testGroup "(Hask, MapOn Hask) and (Hask, MapOn Hask) with ChoiceC"
>     [ test_BifunctorC_laws
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy (ChoiceC Hask Hask))
>         (Proxy :: Proxy Bool) (Proxy :: Proxy Bool)
>         (Proxy :: Proxy Bool) (Proxy :: Proxy Bool)
>         (Proxy :: Proxy Bool) (Proxy :: Proxy Bool)
>         (Proxy :: Proxy ((),()))
> 
>     , test_BifunctorC_laws
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy (ChoiceC Hask Hask))
>         (Proxy :: Proxy Char) (Proxy :: Proxy Char)
>         (Proxy :: Proxy Char) (Proxy :: Proxy Char)
>         (Proxy :: Proxy Char) (Proxy :: Proxy Char)
>         (Proxy :: Proxy ((),()))
> 
>     , test_BifunctorC_laws
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy (ChoiceC Hask Hask))
>         (Proxy :: Proxy Int) (Proxy :: Proxy Int)
>         (Proxy :: Proxy Int) (Proxy :: Proxy Int)
>         (Proxy :: Proxy Int) (Proxy :: Proxy Int)
>         (Proxy :: Proxy ((),()))
>     ]
>   ]
