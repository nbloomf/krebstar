> module Kreb.Category.ProfunctorC.Test where
> 
> import Data.Proxy
> import Test.Tasty
> import Kreb.Category



> test_ProfunctorC :: TestTree
> test_ProfunctorC = testGroup "ProfunctorC"
>   [ testGroup "(Hask, MapOn Hask) and (Hask, MapOn Hask) with Map"
>     [ test_ProfunctorC_laws
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy (Map Hask Hask))
>         (Proxy :: Proxy Bool) (Proxy :: Proxy Bool)
>         (Proxy :: Proxy Bool) (Proxy :: Proxy Bool)
>         (Proxy :: Proxy Bool) (Proxy :: Proxy Bool)
>         (Proxy :: Proxy ((), Bool))
> 
>     , test_ProfunctorC_laws
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy (Map Hask Hask))
>         (Proxy :: Proxy Char) (Proxy :: Proxy Char)
>         (Proxy :: Proxy Char) (Proxy :: Proxy Char)
>         (Proxy :: Proxy Char) (Proxy :: Proxy Char)
>         (Proxy :: Proxy ((), Char))
> 
>     , test_ProfunctorC_laws
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy (Map Hask Hask))
>         (Proxy :: Proxy Int) (Proxy :: Proxy Int)
>         (Proxy :: Proxy Int) (Proxy :: Proxy Int)
>         (Proxy :: Proxy Int) (Proxy :: Proxy Int)
>         (Proxy :: Proxy ((), Int))
>     ]
>   ]
