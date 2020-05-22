> {-# LANGUAGE FlexibleContexts #-}

> module Kreb.Category.Class.BifunctorC.Laws where

> import Data.Proxy

> import Test.Tasty

> import           Kreb.Control
> import qualified Kreb.Format as Fmt
> import           Kreb.Prop

> import Kreb.Category.Class.CategoryC
> import Kreb.Category.Class.BifunctorC



> test_BifunctorC_laws
>   :: forall obj1 mor1 obj2 mor2 obj3 mor3 a1 b1 c1 a2 b2 c2 t f
>    . ( BifunctorC obj1 mor1 obj2 mor2 obj3 mor3 f
>      , obj1 a1, obj1 b1, obj1 c1, obj2 a2, obj2 b2, obj2 c2
>      , EqIn (t, f a1 a2) (mor3 (f a1 a2) (f a1 a2))
>      , EqIn (t, f a1 a2) (mor3 (f a1 a2) (f c1 c2))
>      , Fmt.Display (mor3 (f a1 a2) (f a1 a2))
>      , Fmt.Display (mor3 (f a1 a2) (f c1 c2))
>      , Fmt.Display t, Arb t, Prune t
>      , Fmt.Display (f a1 a2), Arb (f a1 a2), Prune (f a1 a2)
>      , Fmt.Display (mor1 a1 b1), Arb (mor1 a1 b1), Prune (mor1 a1 b1)
>      , Fmt.Display (mor1 b1 c1), Arb (mor1 b1 c1), Prune (mor1 b1 c1)
>      , Fmt.Display (mor2 a2 b2), Arb (mor2 a2 b2), Prune (mor2 a2 b2)
>      , Fmt.Display (mor2 b2 c2), Arb (mor2 b2 c2), Prune (mor2 b2 c2)
>      )
>   => Proxy (obj1 :: Obj) -> Proxy mor1
>   -> Proxy (obj2 :: Obj) -> Proxy mor2
>   -> Proxy (obj3 :: Obj) -> Proxy mor3
>   -> Proxy f
>   -> Proxy a1 -> Proxy a2
>   -> Proxy b1 -> Proxy b2
>   -> Proxy c1 -> Proxy c2
>   -> Proxy t
>   -> TestTree
> test_BifunctorC_laws _ _ _ _ _ _ _ _ _ _ _ _ _ _ =
>   testGroup "BifunctorC Laws"
>     [ krebProp "bimapC unit unit == unit" $
>         \(t :: t) (x :: f a1 a2) ->
>           claimEqualIn @(t, f a1 a2) @(mor3 (f a1 a2) (f a1 a2)) (t,x)
>             (bimapC @obj1 @mor1 @obj2 @mor2 @obj3 @mor3
>               (unit @obj1 @mor1) (unit @obj2 @mor2))
>             (unit @obj3 @mor3)
> 
>     , krebProp "bimapC (comp f g) (comp h k) == bimapC f h . bimapC g k" $
>         \(f :: mor1 b1 c1) (g :: mor1 a1 b1)
>           (h :: mor2 b2 c2) (k :: mor2 a2 b2)
>           (t :: t) (x :: f a1 a2) ->
>           claimEqualIn @(t, f a1 a2) @(mor3 (f a1 a2) (f c1 c2)) (t,x)
>             (bimapC @obj1 @mor1 @obj2 @mor2 @obj3 @mor3 @f
>               (comp @obj1 f g) (comp @obj2 h k))
>             (comp @obj3
>               (bimapC @obj1 @mor1 @obj2 @mor2 @obj3 @mor3 @f f h)
>               (bimapC @obj1 @mor1 @obj2 @mor2 @obj3 @mor3 @f g k))
>     ]
