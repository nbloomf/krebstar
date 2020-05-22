> module Kreb.Control.EqIn where

> import Data.Void

> class EqIn ctx t where
>   eqIn :: ctx -> t -> t -> Bool

> instance EqIn ctx Void    where eqIn _ = (==)
> instance EqIn ctx ()      where eqIn _ = (==)
> instance EqIn ctx Bool    where eqIn _ = (==)
> instance EqIn ctx Char    where eqIn _ = (==)
> instance EqIn ctx Int     where eqIn _ = (==)
> instance EqIn ctx Integer where eqIn _ = (==)

> instance
>   ( EqIn ctx b
>   ) => EqIn (ctx, a) (a -> b)
>   where
>     eqIn (ctx, a) f g =
>       eqIn ctx (f a) (g a)
