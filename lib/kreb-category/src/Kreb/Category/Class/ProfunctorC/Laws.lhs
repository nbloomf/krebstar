> {-# LANGUAGE FlexibleContexts #-}

> module Kreb.Category.Class.ProfunctorC.Laws where

> import Data.Proxy

> import Test.Tasty

> import           Kreb.Control
> import qualified Kreb.Format as Fmt
> import           Kreb.Prop

> import Kreb.Category.Class.CategoryC
> import Kreb.Category.Class.ProfunctorC



> test_ProfunctorC_laws
>   :: forall
>       (obj1 :: Obj) (mor1 :: Mor)
>       (obj2 :: Obj) (mor2 :: Mor)
>       (obj3 :: Obj) (mor3 :: Mor)
>       a1 b1 c1 a2 b2 c2 t
>       (p :: * -> * -> *)
>    . ( ProfunctorC obj1 mor1 obj2 mor2 obj3 mor3 p
>      , obj1 a1, obj1 b1, obj1 c1, obj2 a2, obj2 b2, obj2 c2
>      , EqIn (t, p a1 a2) (mor3 (p a1 a2) (p a1 a2))
>      , EqIn (t, p a1 a2) (mor3 (p a1 a2) (p c1 c2))
>      , Fmt.Display (mor3 (p a1 a2) (p a1 a2))
>      , Fmt.Display (mor3 (p a1 a2) (p c1 c2))
>      , Fmt.Display t, Arb t, Prune t
>      , Fmt.Display (p a1 a2), Arb (p a1 a2), Prune (p a1 a2)
>      , Fmt.Display (mor1 b1 a1), Arb (mor1 b1 a1), Prune (mor1 b1 a1)
>      , Fmt.Display (mor1 c1 b1), Arb (mor1 c1 b1), Prune (mor1 c1 b1)
>      , Fmt.Display (mor2 a2 b2), Arb (mor2 a2 b2), Prune (mor2 a2 b2)
>      , Fmt.Display (mor2 b2 c2), Arb (mor2 b2 c2), Prune (mor2 b2 c2)
>      )
>   => Proxy (obj1 :: Obj) -> Proxy mor1
>   -> Proxy (obj2 :: Obj) -> Proxy mor2
>   -> Proxy (obj3 :: Obj) -> Proxy mor3
>   -> Proxy p
>   -> Proxy a1 -> Proxy b1 -> Proxy c1
>   -> Proxy a2 -> Proxy b2 -> Proxy c2
>   -> Proxy t
>   -> TestTree
> test_ProfunctorC_laws _ _ _ _ _ _ _ _ _ _ _ _ _ _ =
>   testGroup "ProfunctorC Laws"
>     [ krebProp "dimapC unit unit == unit" $
>         \(t :: t) (x :: p a1 a2) ->
>           claimEqualIn @(t, p a1 a2) @(mor3 (p a1 a2) (p a1 a2)) (t,x)
>             (dimapC @obj1 @mor1 @obj2 @mor2 @obj3 @mor3 @p
>               (unit @obj1 @mor1) (unit @obj2 @mor2))
>             (unit @obj3 @mor3)
> 
>     , krebProp "dimapC (comp f g) (comp h k) == dimapC g h . dimapC f k" $
>         \(f :: mor1 b1 a1) (g :: mor1 c1 b1)
>           (h :: mor2 b2 c2) (k :: mor2 a2 b2)
>           (t :: t) (x :: p a1 a2) ->
>           claimEqualIn @(t, p a1 a2) @(mor3 (p a1 a2) (p c1 c2)) (t,x)
>             (dimapC @obj1 @mor1 @obj2 @mor2 @obj3 @mor3 @p
>               (comp @obj1 f g) (comp @obj2 h k))
>             (comp @obj3 @mor3
>               (dimapC @obj1 @mor1 @obj2 @mor2 @obj3 @mor3 @p g h)
>               (dimapC @obj1 @mor1 @obj2 @mor2 @obj3 @mor3 @p f k))
>     ]
