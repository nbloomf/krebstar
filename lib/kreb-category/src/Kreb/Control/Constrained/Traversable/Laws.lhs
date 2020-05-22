> module Kreb.Control.Constrained.Traversable.Laws where

> import Data.Proxy
> import Data.Foldable

> import Test.Tasty

> import qualified Kreb.Format as Fmt
> import           Kreb.Prop

> import Kreb.Control.Functor.Compose
> import Kreb.Control.Monad.Identity

> import Kreb.Control.Constrained.Functor
> import Kreb.Control.Constrained.Traversable



> test_ConstrainedTraversable_laws
>   :: forall a b c t f g
>    . ( ConstrainedTraversable t, Applicative f, Applicative g
>      , FunctorConstraint t a, FunctorConstraint t (f a), FunctorConstraint t (Identity (f a))
>      , FunctorConstraint t b, FunctorConstraint t (f b)
>      , FunctorConstraint t c
>      , FunctorConstraint t (g a), FunctorConstraint t (f (g a)), FunctorConstraint t (Compose f g a)
>      , Eq (t (f a)), Fmt.Display (t (f a)), Arb (t (f a)), Prune (t (f a))
>      , Eq (f (t a)), Fmt.Display (f (t a))
>      , Eq (f (g (t a))), Fmt.Display (f (g (t a)))
>      , Fmt.Display (t (f (g a))), Arb (t (f (g a))), Prune (t (f (g a)))
>      , Fmt.Display a, Prune a, MakeTo a, CoArb a
>      , Fmt.Display b, Arb b, Prune b, MakeTo b, CoArb b
>      , Fmt.Display (t a), Arb (t a), Prune (t a), MakeTo (t a), CoArb (t a)
>      , Eq (t b), Fmt.Display (t b)
>      , Eq (f b)
>      , Eq (f (t b)), Fmt.Display (f (t b))
>      , Fmt.Display (f b), Arb (f b), Prune (f b)
>      , Fmt.Display (g c), Arb (g c), Prune (g c)
>      , Eq (f (g (t c))), Fmt.Display (f (g (t c)))
>      )
>   => Proxy t -> Proxy f -> Proxy g -> Proxy a -> Proxy b -> Proxy c
>   -> TestTree
> test_ConstrainedTraversable_laws pt pf pg pa pb pc =
>   testGroup "ConstrainedTraversable laws"
>     [ krebProp
>         "Unitarity Law (sequenceAC): sequenceAC . fmapC Identity == Identity" $
>         \(x :: t (f a)) ->
>           claimEqual (sequenceAC $ fmapC Identity x) (Identity x)
> 
>     , krebProp
>         "Linearity Law (sequenceAC): sequenceAC . fmap Compose == Compose . fmap sequenceAC . sequenceAC" $
>         \(x :: t (f (g a))) ->
>           claimEqual
>             (sequenceAC $ fmapC Compose x)
>             (Compose $ fmap sequenceAC $ sequenceAC x)
> 
>     , krebProp
>         "Unitarity Law (traverseC): traverseC (Identity . f) == Identity . fmap f" $
>         \(x :: t a) (f :: Fun a b) ->
>           claimEqual
>             (traverseC (Identity . (apFun f)) x)
>             (Identity $ fmapC (apFun f) x)
> 
>     , krebProp
>         "Linearity Law: (traverseC): " $
>         \(x :: t a) (f :: Fun a (f b)) (g :: Fun b (g c)) ->
>           claimEqual
>             (traverseC (Compose . fmap (apFun g) . (apFun f)) x)
>             (Compose $ fmap (traverseC (apFun g)) $ traverseC (apFun f) x)
> 
>     , krebProp
>         "traverseC f == sequenceAC . fmap f" $
>         \(x :: t a) (f :: Fun a (f b)) ->
>           claimEqual
>             (traverseC (apFun f) x)
>             (sequenceAC $ fmapC (apFun f) x)
> 
>     , krebProp
>         "sequenceAC == consumeC id" $
>         \(x :: t (f a)) ->
>           claimEqual (sequenceAC x) (consumeC id x)
> 
>     , krebProp
>         "consumeC f == fmap f . traverseC id" $
>         \(x :: t (f a)) (f :: Fun (t a) b) ->
>           claimEqual (consumeC (apFun f) x) (fmap (apFun f) $ traverseC id x)
>     ]
