> module Kreb.Category.Class.MonoidalCategoryC where

> import Data.Void

> import Kreb.Category.Class.CategoryC
> import Kreb.Category.Class.BifunctorC

Laws:

> class
>   ( CategoryC obj mor
>   , BifunctorC obj mor obj mor obj mor otimes
>   ) => MonoidalCategoryC obj mor otimes unit
>   where
>     assocL
>       :: ( obj x, obj y, obj z )
>       => mor
>           (otimes x (otimes y z))
>           (otimes (otimes x y) z)
>     assocR
>       :: ( obj x, obj y, obj z )
>       => mor
>           (otimes (otimes x y) z)
>           (otimes x (otimes y z))
>     unitL
>       :: ( obj x, obj unit )
>       => mor (otimes x unit) x
>     unitL'
>       :: ( obj x, obj unit )
>       => mor x (otimes x unit)
>     unitR
>       :: ( obj x, obj unit )
>       => mor (otimes unit x) x
>     unitR'
>       :: ( obj x, obj unit )
>       => mor x (otimes unit x)
