> {-# LANGUAGE FlexibleContexts #-}

> module Kreb.Category.Class.FunctorC.Laws where

> import Data.Proxy
> import Data.Foldable
> 
> import Test.Tasty
> 
> import           Kreb.Control
> import qualified Kreb.Format as Fmt
> import           Kreb.Prop

> import Kreb.Category.Class.CategoryC
> import Kreb.Category.Class.FunctorC

> test_FunctorC_laws
>   :: forall obj1 mor1 obj2 mor2 f a b c t
>    . ( FunctorC obj1 mor1 obj2 mor2 f
>      , obj1 a, obj1 b, obj1 c
>      , EqIn (t, f a) (mor2 (f a) (f a))
>      , EqIn (t, f a) (mor2 (f a) (f c))
>      , Fmt.Display (mor2 (f a) (f a))
>      , Fmt.Display (mor2 (f a) (f c))
>      , Fmt.Display t, Arb t, Prune t
>      , Fmt.Display (f a), Arb (f a), Prune (f a)
>      , Fmt.Display (mor1 a b), Arb (mor1 a b), Prune (mor1 a b)
>      , Fmt.Display (mor1 b c), Arb (mor1 b c), Prune (mor1 b c)
>      )
>   => Proxy (obj1 :: Obj) -> Proxy mor1
>   -> Proxy (obj2 :: Obj) -> Proxy mor2
>   -> Proxy f -> Proxy a -> Proxy b -> Proxy c -> Proxy t
>   -> TestTree
> test_FunctorC_laws _ _ _ _ _ _ _ _ _ =
>   testGroup "FunctorC laws"
>     [ krebProp "fmapC unit === unit" $
>         \(t :: t) (x :: f a) ->
>           claimEqualIn @(t, f a) @(mor2 (f a) (f a)) (t,x)
>             (fmapC @obj1 @mor1 @obj2 @mor2 (unit @obj1 @mor1))
>             (unit @obj2 @mor2)
> 
>     , krebProp "fmapC (comp u v) === comp (fmapC u) (fmapC v)" $
>         \(t :: t) (x :: f a) (u :: mor1 b c) (v :: mor1 a b) ->
>           claimEqualIn @(t, f a) @(mor2 (f a) (f c)) (t,x)
>             (fmapC @obj1 @mor1 @obj2 @mor2 @f
>               (comp @obj1 @mor1 u v))
>             (comp @obj2 @mor2
>               (fmapC @obj1 @mor1 @obj2 @mor2 @f u)
>               (fmapC @obj1 @mor1 @obj2 @mor2 @f v))
>     ]

> test_FoldableFunctorC_laws
>   :: forall obj1 obj2 f a b m t
>    . ( FunctorC obj1 (->) obj2 (->) f, Foldable f, Monoid m
>      , obj1 a, obj1 b, obj1 m, obj2 m
>      , EqIn t m
>      , Fmt.Display a, Arb a, Prune a, CoArb a, MakeTo a
>      , Fmt.Display b, Arb b, Prune b, CoArb b, MakeTo b
>      , Fmt.Display (f a), Arb (f a), Prune (f a)
>      , Fmt.Display m, Arb m, Prune m
>      , Fmt.Display t, Arb t, Prune t
>      )
>   => Proxy (obj1 :: Obj) -> Proxy (obj2 :: Obj)
>   -> Proxy f -> Proxy a -> Proxy b -> Proxy m -> Proxy t
>   -> TestTree
> test_FoldableFunctorC_laws _ _ _ _ _ _ _ =
>   testGroup "Foldable FunctorC laws"
>     [ krebProp "foldMap f === comp fold fmapC f" $
>         \(t :: t) (f :: Fun a m) (x :: f a) ->
>           claimEqualIn t
>             ((foldMap (apFun f)) x)
>             ((comp @obj2 @(->) fold
>               (fmapC @obj1 @(->) @obj2 @(->) (apFun f))) x)
> 
>     , krebProp "comp (foldMap f) (fmapC g) === foldMap (comp f g)" $
>         \(t :: t) (g :: Fun b m) (f :: Fun a b) (x :: f a) ->
>           claimEqualIn t
>             ((comp @obj2 @(->)
>               (foldMap (apFun g))
>               (fmapC @obj1 @(->) @obj2 @(->) @f (apFun f))) x)
>             ((foldMap (comp @obj1 @(->) (apFun g) (apFun f))) x)
>     ]
