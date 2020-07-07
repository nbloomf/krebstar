> {-# LANGUAGE FlexibleContexts #-}

> module Kreb.Category.Class.TraversableC.Laws where

> import Data.Proxy
> import Data.Foldable

> import Test.Tasty

> import           Kreb.Control
> import qualified Kreb.Format as Fmt
> import           Kreb.Prop

> import Kreb.Category.Class.CategoryC
> import Kreb.Category.Class.FunctorC
> import Kreb.Category.Class.TraversableC



> test_TraversableC_laws
>   :: forall obj1 obj2 f a b c t1 t2 t3 t4 u v
>    . ( TraversableC obj1 (->) obj2 (->) f, Applicative u, Applicative v
>      , obj1 a, obj1 b, obj1 (Identity a), obj1 (u a), obj1 (v (u a)), obj1 (Compose v u a), obj1 (u b)
>      , obj1 (v b), obj1 (v (u c)), obj1 (Compose v u c), obj1 c
>      , obj2 (Identity (f a)), obj2 (Identity (f b)), obj2 (v (f (u a))), obj2 (v (u (f a))), obj2 (Compose v u (f a))
>      , obj2 (u (f b)), obj2 (v (f b)), obj2 (v (u (f c))), obj2 (Compose v u (f c)), obj2 (u (f a)), obj2 (u b)
>      , EqIn t1 (Identity (f a))
>      , EqIn t1 (Identity (f b))
>      , EqIn t2 (Compose v u (f a))
>      , EqIn t2 (Compose v u (f c))
>      , EqIn t3 (u (f a))
>      , EqIn t3 (u (f b))
>      , EqIn t4 (u b)
>      , Fmt.Display a, Arb a, Prune a, CoArb a, MakeTo a
>      , Fmt.Display b, Arb b, Prune b, CoArb b, MakeTo b
>      , Fmt.Display t1, Arb t1, Prune t1
>      , Fmt.Display t2, Arb t2, Prune t2
>      , Fmt.Display t3, Arb t3, Prune t3
>      , Fmt.Display t4, Arb t4, Prune t4
>      , Fmt.Display (f a), Arb (f a), Prune (f a), CoArb (f a), MakeTo (f a)
>      , Fmt.Display (f (u a)), Arb (f (u a)), Prune (f (u a))
>      , Fmt.Display (f (v (u a))), Arb (f (v (u a))), Prune (f (v (u a)))
>      , Fmt.Display (v b), Arb (v b), Prune (v b)
>      , Fmt.Display (u b), Arb (u b), Prune (u b)
>      , Fmt.Display (u c), Arb (u c), Prune (u c)
>      , Fmt.Display (f b)
>      , Fmt.Display (u (f a))
>      , Fmt.Display (u (f b))
>      , Fmt.Display (v (u (f a)))
>      , Fmt.Display (v (u (f c)))
>      )
>   => Proxy (obj1 :: Obj) -> Proxy (obj2 :: Obj)
>   -> Proxy f -> Proxy a -> Proxy b -> Proxy c
>   -> Proxy t1 -> Proxy t2 -> Proxy t3 -> Proxy t4 -> Proxy u -> Proxy v
>   -> TestTree
> test_TraversableC_laws _ _ _ _ _ _ _ _ _ _ _ _ =
>   testGroup "TraversableC Laws"
>     [ krebProp
>         "Unitarity Law (sequenceAC): sequenceAC . fmapC Identity == Identity" $
>         \(t :: t1) (x :: f a) ->
>           claimEqualIn t
>             ((comp @obj2 @(->)
>               (sequenceAC @obj1 @(->) @obj2 @(->) @f @Identity @a)
>               (fmapC @obj1 @(->) @obj2 @(->) @f Identity)) x)
>             (Identity @(f a) x)
> 
>     , krebProp
>         "Unitarity Law (traverseC): traverseC (Identity . f) == Identity . fmapC f" $
>         \(t :: t1) (f :: Fun a b) (x :: f a) ->
>           claimEqualIn t
>             ((traverseC @obj1 @(->) @obj2 @(->) @f @Identity @a
>               (Identity . (apFun f))) x)
>             ((comp @obj2 @(->)
>               Identity
>               (fmapC @obj1 @(->) @obj2 @(->) @f (apFun f))) x)
> 
>     , krebProp
>         "Linearity Law (sequenceAC): sequenceAC . fmap Compose == Compose . fmap sequenceAC . sequenceAC" $
>         \(t :: t2) (x :: f (v (u a))) ->
>           claimEqualIn t
>             ((comp @obj2 @(->)
>               (sequenceAC @obj1 @(->) @obj2 @(->) @f @(Compose v u) @a)
>               (fmapC @obj1 @(->) @obj2 @(->) @f Compose)) x)
>             ((comp @obj2 @(->)
>               (Compose)
>               (comp @obj2 @(->)
>                 (fmap @v @(f (u a))
>                   (sequenceAC @obj1 @(->) @obj2 @(->) @f @u @a))
>                 (sequenceAC @obj1 @(->) @obj2 @(->) @f @v @(u a))) x))
> 
>     , krebProp
>         "Linearity Law: (traverseC): traverseC (Compose . fmap g . f) === Compose . fmap (traverseC g) . traverseC f" $
>         \(t :: t2) (x :: f a) (f :: Fun a (v b)) (g :: Fun b (u c)) ->
>           claimEqualIn t
>             ((traverseC @obj1 @(->) @obj2 @(->) @f @(Compose v u)
>               (comp @obj1 @(->)
>                 Compose
>                 (comp @obj1 @(->)
>                   (fmap (apFun g))
>                   (apFun f)))) x)
>             ((comp @obj2 @(->)
>               Compose
>               (comp @obj2 @(->)
>                 (fmap (traverseC @obj1 @(->) @obj2 @(->) @f @u @b (apFun g)))
>                 (traverseC @obj1 @(->) @obj2 @(->) @f @v @a (apFun f)))) x)
> 
>     , krebProp
>         "traverseC f == sequenceAC . fmap f" $
>         \(t :: t3) (x :: f a) (f :: Fun a (u b)) ->
>           claimEqualIn t
>             (traverseC @obj1 @(->) @obj2 @(->) @f (apFun f) x)
>             ((comp @obj2 @(->)
>               (sequenceAC @obj1 @(->) @obj2 @(->) @f)
>               (fmapC @obj1 @(->) @obj2 @(->) @f (apFun f))) x)
> 
>     , krebProp
>         "sequenceAC == consumeC id" $
>         \(t :: t3) (x :: f (u a)) ->
>           claimEqualIn t
>             (sequenceAC @obj1 @(->) @obj2 @(->) @f x)
>             (consumeC @obj1 @(->) @obj2 @(->) @f id x)
> 
>     , krebProp
>         "consumeC f == fmap f . traverseC id" $
>         \(t :: t4) (x :: f (u a)) (f :: Fun (f a) b) ->
>           claimEqualIn t
>             (consumeC @obj1 @(->) @obj2 @(->) @f (apFun f) x)
>             (comp @obj2 @(->)
>               (fmap (apFun f))
>               (traverseC @obj1 @(->) @obj2 @(->) @f id) x)
>     ]
