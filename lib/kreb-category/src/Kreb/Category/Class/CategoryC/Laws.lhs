> {-# LANGUAGE FlexibleContexts #-}

> module Kreb.Category.Class.CategoryC.Laws where

> import Data.Proxy

> import Test.Tasty

> import           Kreb.Control
> import qualified Kreb.Format as Fmt
> import           Kreb.Prop

> import Kreb.Category.Class.CategoryC



> test_CategoryC_laws
>   :: forall obj mor a b c d t
>    . ( CategoryC obj mor, obj a, obj b, obj c, obj d
>      , EqIn (t,a) (mor a b), EqIn (t,a) (mor a d)
>      , Fmt.Display (mor a c), Fmt.Display (mor a d)
>      , Fmt.Display t, Arb t, Prune t
>      , Fmt.Display a, Arb a, Prune a
>      , Fmt.Display (mor a b), Arb (mor a b), Prune (mor a b)
>      , Fmt.Display (mor b c), Arb (mor b c), Prune (mor b c)
>      , Fmt.Display (mor c d), Arb (mor c d), Prune (mor c d)
>      )
>   => Proxy (obj :: Obj) -> Proxy mor
>   -> Proxy a -> Proxy b -> Proxy c -> Proxy d
>   -> Proxy t
>   -> TestTree
> test_CategoryC_laws po pm pa pb pc pd pt =
>   testGroup "CategoryC Laws"
>     [ krebProp "comp unit f == f" $
>         \(f :: mor a b) (t :: t) (a :: a) ->
>           claimEqualIn (t,a)
>             (comp @obj (unit @obj) f)
>             (f)
> 
>     , krebProp "comp f unit == f" $
>         \(f :: mor a b) (t :: t) (a :: a) ->
>           claimEqualIn (t,a)
>             (comp @obj f (unit @obj))
>             (f)
> 
>     , krebProp "comp f (comp g h) == comp (comp g h) f" $
>         \(f :: mor c d) (g :: mor b c) (h :: mor a b) (t :: t) (a :: a) ->
>           claimEqualIn (t,a)
>             (comp @obj f (comp @obj g h))
>             (comp @obj (comp @obj f g) h)
>     ]
