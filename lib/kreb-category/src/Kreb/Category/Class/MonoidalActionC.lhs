> module Kreb.Category.Class.MonoidalActionC where

> import Data.Void

> import Kreb.Category.Class.CategoryC
> import Kreb.Category.Class.BifunctorC
> import Kreb.Category.Class.MonoidalCategoryC

> class
>   ( MonoidalCategoryC objm morm otimes unit
>   , CategoryC objc morc
>   , BifunctorC objm morm objc morc objc morc f
>   ) => MonoidalActionC objm morm otimes unit objc morc f
>   where
>     unitor
>       :: ( objc x, objm unit )
>       => morc (f unit x) x
>     unitor'
>       :: ( objc x, objm unit )
>       => morc x (f unit x)
>     compor
>       :: ( objm p, objm q, objc x )
>       => morc (f p (f q x)) (f (otimes p q) x)
>     compor'
>       :: ( objm p, objm q, objc x )
>       => morc (f (otimes p q) x) (f p (f q x))
