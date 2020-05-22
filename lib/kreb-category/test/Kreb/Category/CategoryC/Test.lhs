> module Kreb.Category.CategoryC.Test where

> import Data.Proxy

> import Test.Tasty



> import Kreb.Category.Class.CategoryC
> import Kreb.Category.Class.CategoryC.Laws

> test_CategoryC :: TestTree
> test_CategoryC = testGroup "CategoryC"
>   [ testGroup "Hask with (MapOn Hask)"
>     [ test_CategoryC_laws
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy Bool) (Proxy :: Proxy Bool)
>         (Proxy :: Proxy Bool) (Proxy :: Proxy Bool)
>         (Proxy :: Proxy ())
> 
>     , test_CategoryC_laws
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy Char) (Proxy :: Proxy Char)
>         (Proxy :: Proxy Char) (Proxy :: Proxy Char)
>         (Proxy :: Proxy ())
> 
>     , test_CategoryC_laws
>         (Proxy :: Proxy Hask) (Proxy :: Proxy (MapOn Hask))
>         (Proxy :: Proxy Int) (Proxy :: Proxy Int)
>         (Proxy :: Proxy Int) (Proxy :: Proxy Int)
>         (Proxy :: Proxy ())
>     ]
> 
>   , testGroup "Eq with (MapOn Eq)"
>     [ test_CategoryC_laws
>         (Proxy :: Proxy Eq) (Proxy :: Proxy (MapOn Eq))
>         (Proxy :: Proxy Bool) (Proxy :: Proxy Bool)
>         (Proxy :: Proxy Bool) (Proxy :: Proxy Bool)
>         (Proxy :: Proxy ())
> 
>     , test_CategoryC_laws
>         (Proxy :: Proxy Eq) (Proxy :: Proxy (MapOn Eq))
>         (Proxy :: Proxy Char) (Proxy :: Proxy Char)
>         (Proxy :: Proxy Char) (Proxy :: Proxy Char)
>         (Proxy :: Proxy ())
> 
>     , test_CategoryC_laws
>         (Proxy :: Proxy Eq) (Proxy :: Proxy (MapOn Eq))
>         (Proxy :: Proxy Int) (Proxy :: Proxy Int)
>         (Proxy :: Proxy Int) (Proxy :: Proxy Int)
>         (Proxy :: Proxy ())
>     ]
>   ]
