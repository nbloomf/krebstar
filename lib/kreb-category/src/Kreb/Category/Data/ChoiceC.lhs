> {-# LANGUAGE OverloadedStrings, UndecidableInstances #-}

> module Kreb.Category.Data.ChoiceC (
>     ChoiceC(..)
> ) where

> import Data.Proxy
> import Data.Void

> import           Kreb.Control
> import qualified Kreb.Format as Fmt
> import           Kreb.Format ((<+>), display)
> import           Kreb.Prop

> import Kreb.Category.Class

> data ChoiceC (obj1 :: Obj) (obj2 :: Obj) a b where
>   Choice1
>     :: ( obj1 a, obj2 b )
>     => a -> ChoiceC obj1 obj2 a b
>   Choice2
>     :: ( obj1 a, obj2 b )
>     => b -> ChoiceC obj1 obj2 a b

> instance
>   ( Eq a, Eq b
>   ) => Eq (ChoiceC obj1 obj2 a b)
>   where
>     u == v = case (u,v) of
>       (Choice1 a, Choice1 b) -> a == b
>       (Choice2 a, Choice2 b) -> a == b
>       _ -> False

> instance
>   ( EqIn ctxa a, EqIn ctxb b
>   ) => EqIn (ctxa, ctxb) (ChoiceC obj1 obj2 a b)
>   where
>     eqIn (ctxa, ctxb) u v = case u of
>       Choice1 a1 -> case v of
>         Choice1 a2 -> eqIn ctxa a1 a2
>         _ -> False
>       Choice2 b1 -> case v of
>         Choice2 b2 -> eqIn ctxb b1 b2
>         _ -> False



> instance
>   ( Fmt.Display a, Fmt.Display b
>   ) => Fmt.Display (ChoiceC obj1 obj2 a b)
>   where
>     display x = case x of
>       Choice1 a -> "Choice1" <+> display a
>       Choice2 b -> "Choice2" <+> display b



Using some type hackery to get instances for 'ChoiceC Void a'
and 'Choice a Void' without overlapping instances. This is weird!

> data VoidL
> data VoidR
> data VoidN
> 
> type family VoidChoice a where
>   VoidChoice ( ChoiceC obj1 obj2 Void b    ) = VoidL
>   VoidChoice ( ChoiceC obj1 obj2 a    Void ) = VoidR
>   VoidChoice ( ChoiceC obj1 obj2 a    b    ) = VoidN
> 
> class ChoiceArb flag obj1 obj2 a b where
>   choiceArb
>     :: flag -> Proxy a -> Proxy b
>     -> Sample (ChoiceC obj1 obj2 a b)
> 
> instance
>   ( Arb a, obj1 a, obj2 Void
>   ) => ChoiceArb VoidR obj1 obj2 a Void
>   where
>     choiceArb _ _ _ = Choice1 <$> arb
> 
> instance
>   ( Arb b, obj1 Void, obj2 b
>   ) => ChoiceArb VoidL obj1 obj2 Void b
>   where
>     choiceArb _ _ _ = Choice2 <$> arb
> 
> instance
>   ( Arb a, Arb b, obj1 a, obj2 b
>   ) => ChoiceArb VoidN obj1 obj2 a b
>   where
>     choiceArb _ _ _ = freq
>       [ (1, Choice1 <$> arb)
>       , (1, Choice2 <$> arb)
>       ]
> 
> instance
>   ( VoidChoice (ChoiceC obj1 obj2 a b) ~ flag
>   , ChoiceArb flag obj1 obj2 a b
>   ) => Arb (ChoiceC obj1 obj2 a b)
>   where
>     arb = choiceArb (undefined :: flag) Proxy Proxy



> instance
>   ( Prune a, Prune b
>   ) => Prune (ChoiceC obj1 obj2 a b)
>   where
>     prune x = case x of
>       Choice1 a -> map Choice1 $ prune a
>       Choice2 b -> map Choice2 $ prune b



> instance
>   ( forall x y. (obj1 x, obj2 y) => obj3 (ChoiceC obj1 obj2 x y)
>   ) => BifunctorC
>     (obj1 :: Obj) (MapOn obj1)
>     (obj2 :: Obj) (MapOn obj2)
>     (obj3 :: Obj) (MapOn obj3)
>     (ChoiceC obj1 obj2)
>   where
>     bimapC (Map f) (Map g) =
>       let
>         h x = case x of
>           Choice1 a -> Choice1 (f a)
>           Choice2 b -> Choice2 (g b)
>         in Map h



Monoidal Category Instances

> instance
>   ( forall x y. (obj x, obj y) => obj (ChoiceC obj obj x y)
>   ) => MonoidalCategoryC (obj :: Obj) (MapOn obj) (ChoiceC obj obj) Void
>   where
>     assocL = Map $ \x -> case x of
>       Choice1 x           -> Choice1 (Choice1 x)
>       Choice2 (Choice1 y) -> Choice1 (Choice2 y)
>       Choice2 (Choice2 z) -> Choice2 z
>     assocR = Map $ \x -> case x of
>       Choice1 (Choice1 x) -> Choice1 x
>       Choice1 (Choice2 y) -> Choice2 (Choice1 y)
>       Choice2 z           -> Choice2 (Choice2 z)
>     unitL = Map $ \x -> case x of
>       Choice1 x -> x
>       Choice2 _ -> absurd undefined
>     unitL' = Map Choice1
>     unitR = Map $ \x -> case x of
>       Choice1 _ -> absurd undefined
>       Choice2 y -> y
>     unitR' = Map Choice2



Monoidal Action Instances

> instance
>   ( forall x y. (obj x, obj y) => obj (ChoiceC obj obj x y)
>   ) => MonoidalActionC
>     (obj :: Obj) (MapOn obj) (ChoiceC obj obj) Void
>     (obj :: Obj) (MapOn obj)
>     (ChoiceC obj obj)
>   where
>     unitor  = Map $ \x -> case x of
>       Choice1 _ -> absurd undefined; Choice2 a -> a
>     unitor' = Map Choice2
>     compor  = Map $ \x -> case x of
>       Choice1 p           -> Choice1 (Choice1 p)
>       Choice2 (Choice1 q) -> Choice1 (Choice2 q)
>       Choice2 (Choice2 a) -> Choice2 a
>     compor' = Map $ \x -> case x of
>       Choice1 (Choice1 p) -> Choice1 p
>       Choice1 (Choice2 q) -> Choice2 (Choice1 q)
>       Choice2 a           -> Choice2 (Choice2 a)
