> {-# LANGUAGE FlexibleContexts #-}

> module Kreb.Category.Class.MonoidalCategoryC.Laws where

> import Data.Proxy

> import Test.Tasty

> import           Kreb.Control
> import qualified Kreb.Format as Fmt
> import           Kreb.Prop

> import Kreb.Category.Class.CategoryC
> import Kreb.Category.Class.BifunctorC
> import Kreb.Category.Class.MonoidalCategoryC



> test_MonoidalCategoryC_laws
>   :: forall obj mor otimes unit a b c d
>       t1 -- Equality context of 'a'
>       t2 -- Equality context of 'otimes a unit'
>       t3 -- Equality context of 'otimes unit a'
>       t4 -- Equality context of 'otimes a (otimes b c)'
>       t5 -- Equality context of 'otimes (otimes a b) c'
>       t6 -- Equality context of 'otimes a b'
>       t7 -- Equality context of 'otimes (otimes (otimes a b) c) d'
>       t8 -- Equality context of 'otimes a (otimes b (otimes c d))'
>    . ( MonoidalCategoryC obj mor otimes unit
>      , obj a, obj b, obj c, obj d, obj unit
>      , EqIn
>          (t1, a)
>          (mor a a)
>      , EqIn
>          (t2, otimes a unit)
>          (mor (otimes a unit) (otimes a unit))
>      , EqIn
>          (t3, otimes unit a)
>          (mor (otimes unit a) (otimes unit a))
>      , EqIn
>          (t4, otimes a (otimes b c))
>          (mor (otimes a (otimes b c)) (otimes a (otimes b c)))
>      , EqIn
>          (t5, otimes (otimes a b) c)
>          (mor (otimes (otimes a b) c) (otimes (otimes a b) c))
>      , EqIn
>          (t6, otimes a (otimes unit b))
>          (mor (otimes a (otimes unit b)) (otimes a b))
>      , EqIn
>          (t6, otimes (otimes a unit) b)
>          (mor (otimes (otimes a unit) b) (otimes a b))
>      , EqIn
>          (t7, otimes a (otimes b (otimes c d)))
>          (mor (otimes a (otimes b (otimes c d))) (otimes (otimes (otimes a b) c) d))
>      , EqIn
>          (t8, otimes (otimes (otimes a b) c) d)
>          (mor (otimes (otimes (otimes a b) c) d) (otimes a (otimes b (otimes c d))))
>      , Fmt.Display (mor a a)
>      , Fmt.Display (mor (otimes a unit) (otimes a unit))
>      , Fmt.Display (mor (otimes unit a) (otimes unit a))
>      , Fmt.Display (mor (otimes a (otimes unit b)) (otimes a b))
>      , Fmt.Display (mor (otimes (otimes a unit) b) (otimes a b))
>      , Fmt.Display (mor (otimes a (otimes b (otimes c d))) (otimes (otimes (otimes a b) c) d))
>      , Fmt.Display (mor (otimes (otimes (otimes a b) c) d) (otimes a (otimes b (otimes c d))))
>      , Fmt.Display a
>        , Arb a, Prune a
>      , Fmt.Display (otimes a unit)
>        , Arb (otimes a unit), Prune (otimes a unit)
>      , Fmt.Display (otimes unit a)
>        , Arb (otimes unit a), Prune (otimes unit a)
>      , Fmt.Display
>          (mor (otimes (otimes a b) c) (otimes (otimes a b) c))
>      , Fmt.Display
>          (mor (otimes a (otimes b c)) (otimes a (otimes b c)))
>      , Fmt.Display (otimes a (otimes b c))
>        , Arb (otimes a (otimes b c)), Prune (otimes a (otimes b c))
>      , Fmt.Display (otimes (otimes a b) c)
>        , Arb (otimes (otimes a b) c), Prune (otimes (otimes a b) c)
>      , Fmt.Display (otimes a (otimes unit b))
>        , Arb (otimes a (otimes unit b)), Prune (otimes a (otimes unit b))
>      , Fmt.Display (otimes (otimes a unit) b)
>        , Arb (otimes (otimes a unit) b), Prune (otimes (otimes a unit) b)
>      , Fmt.Display (otimes a (otimes b (otimes c d)))
>        , Arb (otimes a (otimes b (otimes c d))), Prune (otimes a (otimes b (otimes c d)))
>      , Fmt.Display (otimes (otimes (otimes a b) c) d)
>        , Arb (otimes (otimes (otimes a b) c) d), Prune (otimes (otimes (otimes a b) c) d)
>      , Fmt.Display t1, Arb t1, Prune t1
>      , Fmt.Display t2, Arb t2, Prune t2
>      , Fmt.Display t3, Arb t3, Prune t3
>      , Fmt.Display t4, Arb t4, Prune t4
>      , Fmt.Display t5, Arb t5, Prune t5
>      , Fmt.Display t6, Arb t6, Prune t6
>      , Fmt.Display t7, Arb t7, Prune t7
>      , Fmt.Display t8, Arb t8, Prune t8
>      )
>   => Proxy (obj :: Obj) -> Proxy mor -> Proxy otimes -> Proxy unit
>   -> Proxy a -> Proxy b -> Proxy c -> Proxy d
>   -> Proxy t1 -> Proxy t2 -> Proxy t3 -> Proxy t4
>   -> Proxy t5 -> Proxy t6 -> Proxy t7 -> Proxy t8
>   -> TestTree
> test_MonoidalCategoryC_laws _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ =
>   testGroup "CategoryC Laws"
>     [ krebProp "comp unitL unitL' == unit" $
>         \(t :: t1) (x :: a) ->
>           claimEqualIn (t,x)
>             (comp @obj @mor
>               (unitL  @obj @mor @otimes @unit @a)
>               (unitL' @obj @mor @otimes @unit @a))
>             (unit @obj @mor)
> 
>     , krebProp "comp unitL' unitL == unit" $
>         \(t :: t2) (x :: otimes a unit) ->
>           claimEqualIn (t,x)
>             (comp @obj @mor
>               (unitL' @obj @mor @otimes @unit @a)
>               (unitL  @obj @mor @otimes @unit @a))
>             (unit @obj @mor)
> 
>     , krebProp "comp unitR unitR' == unit" $
>         \(t :: t1) (x :: a) ->
>           claimEqualIn (t,x)
>             (comp @obj @mor
>               (unitR  @obj @mor @otimes @unit @a)
>               (unitR' @obj @mor @otimes @unit @a))
>             (unit @obj @mor)
> 
>     , krebProp "comp unitR' unitR == unit" $
>         \(t :: t3) (x :: otimes unit a) ->
>           claimEqualIn (t,x)
>             (comp @obj @mor
>               (unitR' @obj @mor @otimes @unit @a)
>               (unitR  @obj @mor @otimes @unit @a))
>             (unit @obj @mor)
> 
>     , krebProp "comp assocR assocL == unit" $
>         \(t :: t4) (x :: otimes a (otimes b c)) ->
>           claimEqualIn (t,x)
>             (comp @obj @mor
>               (assocR @obj @mor @otimes @unit @a @b @c)
>               (assocL @obj @mor @otimes @unit @a @b @c))
>             (unit @obj @mor)
> 
>     , krebProp "comp assocL assocR == unit" $
>         \(t :: t5) (x :: otimes (otimes a b) c) ->
>           claimEqualIn (t,x)
>             (comp @obj @mor
>               (assocL @obj @mor @otimes @unit @a @b @c)
>               (assocR @obj @mor @otimes @unit @a @b @c))
>             (unit @obj @mor)
> 
>     , krebProp "bimap unit unitR == comp (bimap unitL unit) assocL" $
>         \(t :: t6) (x :: otimes a (otimes unit b)) ->
>           claimEqualIn (t,x)
>             (bimapC @obj @mor @obj @mor @obj @mor
>               (unit @obj @mor @a)
>               (unitR @obj @mor @otimes @unit @b))
>             (comp @obj @mor
>               (bimapC @obj @mor @obj @mor @obj @mor
>                 (unitL @obj @mor @otimes @unit @a)
>                 (unit @obj @mor @b))
>               (assocL @obj @mor @otimes @unit))
> 
>     , krebProp "bimap unitL unit == comp (bimap unit unitR) assocR" $
>         \(t :: t6) (x :: otimes (otimes a unit) b) ->
>           claimEqualIn (t,x)
>             (bimapC @obj @mor @obj @mor @obj @mor
>               (unitL @obj @mor @otimes @unit @a)
>               (unit @obj @mor @b))
>             (comp @obj @mor
>               (bimapC @obj @mor @obj @mor @obj @mor
>                 (unit @obj @mor @a)
>                 (unitR @obj @mor @otimes @unit @b))
>               (assocR @obj @mor @otimes @unit))
> 
>     , krebProp "assocL . assocL == bimap assocL unit . assocL . bimap unit assocL" $
>         \(t :: t7) (x :: otimes a (otimes b (otimes c d))) ->
>           claimEqualIn (t,x)
>             (comp @obj @mor
>               (assocL @obj @mor @otimes @unit @(otimes a b) @c @d)
>               (assocL @obj @mor @otimes @unit @a @b @(otimes c d)))
>             (comp @obj @mor
>               (bimapC @obj @mor @obj @mor @obj @mor
>                 (assocL @obj @mor @otimes @unit @a @b @c)
>                 (unit @obj @mor @d))
>               (comp @obj @mor
>                 (assocL @obj @mor @otimes @unit @a @(otimes b c) @d)
>                 (bimapC @obj @mor @obj @mor @obj @mor
>                   (unit @obj @mor @a)
>                   (assocL @obj @mor @otimes @unit @b @c @d))))
> 
>     , krebProp "assocR . assocR ==  bimap unit assocR . assocR . bimap assocR unit" $
>         \(t :: t8) (x :: otimes (otimes (otimes a b) c) d) ->
>           claimEqualIn (t,x)
>             (comp @obj @mor
>               (assocR @obj @mor @otimes @unit @a @b @(otimes c d))
>               (assocR @obj @mor @otimes @unit @(otimes a b) @c @d))
>             (comp @obj @mor
>               (bimapC @obj @mor @obj @mor @obj @mor
>                 (unit @obj @mor @a)
>                 (assocR @obj @mor @otimes @unit @b @c @d))
>               (comp @obj @mor
>                 (assocR @obj @mor @otimes @unit @a @(otimes b c) @d)
>                 (bimapC @obj @mor @obj @mor @obj @mor
>                   (assocR @obj @mor @otimes @unit @a @b @c)
>                   (unit @obj @mor @d))))
>     ]
