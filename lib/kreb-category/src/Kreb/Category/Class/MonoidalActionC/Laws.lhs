> {-# LANGUAGE FlexibleContexts #-}

> module Kreb.Category.Class.MonoidalActionC.Laws where

> import Data.Proxy

> import Test.Tasty

> import           Kreb.Control
> import qualified Kreb.Format as Fmt
> import           Kreb.Prop

> import Kreb.Category.Class.CategoryC
> import Kreb.Category.Class.BifunctorC
> import Kreb.Category.Class.MonoidalCategoryC
> import Kreb.Category.Class.MonoidalActionC



> test_MonoidalActionC_laws
>   :: forall objm morm otimes unit objc morc f a b c d
>       t1 -- Equality context of 'd'
>       t2 -- Equality context of 'otimes unit d'
>       t3 -- Equality context of 'f a (f b c)'
>       t4 -- Equality context of 'f (otimes a b) c'
>       t5 -- Equality context of 'otimes (otimes (otimes a b) c) d'
>       t6 -- Equality context of 'otimes a (otimes b (otimes c d))'
>    . ( MonoidalActionC objm morm otimes unit objc morc f
>      , objm unit, objm a, objm b, objm c, objc d
>      , EqIn
>          (t1, d)
>          (morc d d)
>      , EqIn
>          (t2, f unit d)
>          (morc (f unit d) (f unit d))
>      , EqIn
>          (t3, f b (f c d))
>          (morc (f b (f c d)) (f b (f c d)))
>      , EqIn
>          (t4, f (otimes b c) d)
>          (morc (f (otimes b c) d) (f (otimes b c) d))
>      , EqIn
>          (t5, f a (f b (f c d)))
>          (morc (f a (f b (f c d))) (f (otimes (otimes a b) c) d))
>      , EqIn
>          (t6, f (otimes (otimes a b) c) d)
>          (morc (f (otimes (otimes a b) c) d) (f a (f b (f c d))))
>      , Fmt.Display (morc d d)
>      , Fmt.Display (morc (f unit d) (f unit d))
>      , Fmt.Display (morc (f b (f c d)) (f b (f c d)))
>      , Fmt.Display (morc (f (otimes b c) d) (f (otimes b c) d))
>      , Fmt.Display (morc (f (otimes (otimes a b) c) d) (f a (f b (f c d))))
>      , Fmt.Display (morc (f a (f b (f c d))) (f (otimes (otimes a b) c) d))
>      , Fmt.Display d
>        , Arb d, Prune d
>      , Fmt.Display (f unit d)
>        , Arb (f unit d), Prune (f unit d)
>      , Fmt.Display (f a (f b c))
>        , Arb (f a (f b c)), Prune (f a (f b c))
>      , Fmt.Display (f (otimes a b) c)
>        , Arb (f (otimes a b) c), Prune (f (otimes a b) c)
>      , Fmt.Display (f b (f c d))
>        , Arb (f b (f c d)), Prune (f b (f c d))
>      , Fmt.Display (f (otimes b c) d)
>        , Arb (f (otimes b c) d), Prune (f (otimes b c) d)
>      , Fmt.Display (f (otimes (otimes a b) c) d)
>        , Arb (f (otimes (otimes a b) c) d), Prune (f (otimes (otimes a b) c) d)
>      , Fmt.Display (f a (f b (f c d)))
>        , Arb (f a (f b (f c d))), Prune (f a (f b (f c d)))
>      , Fmt.Display t1, Arb t1, Prune t1
>      , Fmt.Display t2, Arb t2, Prune t2
>      , Fmt.Display t3, Arb t3, Prune t3
>      , Fmt.Display t4, Arb t4, Prune t4
>      , Fmt.Display t5, Arb t5, Prune t5
>      , Fmt.Display t6, Arb t6, Prune t6
>      )
>   => Proxy (objm :: Obj) -> Proxy morm -> Proxy otimes -> Proxy unit
>   -> Proxy (objc :: Obj) -> Proxy morc -> Proxy f
>   -> Proxy a -> Proxy b -> Proxy c -> Proxy d
>   -> Proxy t1 -> Proxy t2 -> Proxy t3 -> Proxy t4 -> Proxy t5 -> Proxy t6
>   -> TestTree
> test_MonoidalActionC_laws _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ =
>   testGroup "MonoidalActionC Laws"
>     [ krebProp "comp unitor unitor' == unit" $
>         \(t :: t1) (x :: d) ->
>           claimEqualIn (t,x)
>             (comp @objc @morc
>               (unitor  @objm @morm @otimes @unit @objc @morc @f @d)
>               (unitor' @objm @morm @otimes @unit @objc @morc @f @d))
>             (unit @objc @morc)
> 
>     , krebProp "comp unitor' unitor == unit" $
>         \(t :: t2) (x :: f unit d) ->
>           claimEqualIn (t,x)
>             (comp @objc @morc
>               (unitor' @objm @morm @otimes @unit @objc @morc @f @d)
>               (unitor  @objm @morm @otimes @unit @objc @morc @f @d))
>             (unit @objc @morc)
> 
>     , krebProp "comp compor' compor == unit" $
>         \(t :: t3) (x :: f b (f c d)) ->
>           claimEqualIn (t,x)
>             (comp @objc @morc
>               (compor' @objm @morm @otimes @unit @objc @morc @f @b @c @d)
>               (compor  @objm @morm @otimes @unit @objc @morc @f @b @c @d))
>             (unit @objc @morc)
> 
>     , krebProp "comp compor compor' == unit" $
>         \(t :: t4) (x :: f (otimes b c) d) ->
>           claimEqualIn (t,x)
>             (comp @objc @morc
>               (compor  @objm @morm @otimes @unit @objc @morc @f @b @c @d)
>               (compor' @objm @morm @otimes @unit @objc @morc @f @b @c @d))
>             (unit @objc @morc)
> 
>     , krebProp "compor . compor == bimap compor unit . compor . bimap unit compor" $
>         \(t :: t5) (x :: f a (f b (f c d))) ->
>           claimEqualIn (t,x)
>             (comp @objc @morc
>               (compor @objm @morm @otimes @unit @objc @morc @f @(otimes a b) @c @d)
>               (compor @objm @morm @otimes @unit @objc @morc @f @a @b @(f c d)))
>             (comp @objc @morc
>               (bimapC @objm @morm @objc @morc @objc @morc
>                 (assocL @objm @morm @otimes @unit @a @b @c)
>                 (unit @objc @morc @d))
>               (comp @objc @morc
>                 (compor @objm @morm @otimes @unit @objc @morc @f @a @(otimes b c) @d)
>                 (bimapC @objm @morm @objc @morc @objc @morc
>                   (unit @objm @morm @a)
>                   (compor @objm @morm @otimes @unit @objc @morc @f @b @c @d))))
> 
>     , krebProp "compor' . compor' ==  bimap unit compor' . compor' . bimap compor' unit" $
>         \(t :: t6) (x :: f (otimes (otimes a b) c) d) ->
>           claimEqualIn (t,x)
>             (comp @objc @morc
>               (compor' @objm @morm @otimes @unit @objc @morc @f @a @b @(f c d))
>               (compor' @objm @morm @otimes @unit @objc @morc @f @(otimes a b) @c @d))
>             (comp @objc @morc
>               (bimapC @objm @morm @objc @morc @objc @morc
>                 (unit @objm @morm @a)
>                 (compor' @objm @morm @otimes @unit @objc @morc @f @b @c @d))
>               (comp @objc @morc
>                 (compor' @objm @morm @otimes @unit @objc @morc @f @a @(otimes b c) @d)
>                 (bimapC @objm @morm @objc @morc @objc @morc
>                   (assocR @objm @morm @otimes @unit @a @b @c)
>                   (unit @objc @morc @d))))
>     ]
