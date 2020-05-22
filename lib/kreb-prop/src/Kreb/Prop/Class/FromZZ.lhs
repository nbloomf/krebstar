> module Kreb.Prop.Class.FromZZ where

> class FromZZ a where
>   fromZZ :: (Integral n) => n -> a

> instance FromZZ Int     where fromZZ = fromIntegral
> instance FromZZ Integer where fromZZ = fromIntegral
