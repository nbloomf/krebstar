> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE DeriveGeneric #-}
> 
> module Kreb.Struct.Class.Valued where
> 
> import GHC.Generics
> 
> import qualified Kreb.Format as Fmt
> import           Kreb.Format (string, display, (<+>))
> import           Kreb.Control
> import           Kreb.Prop hiding (Trivial)


The Valued Class
================

The secret power of finger trees is that they aren't just _a_ data structure. They are a whole _family_ of data structures parameterized by a monoid. Different choices of this monoid give structures with different characteristics, but all are constructed and accessed in the same way.

The _content_ type `a` of a finger tree must have a distinguished mapping into some monoid type `m`. We can enforce this in Haskell using a typeclass with two parameters. It looks a little bit like we're saying that `a` has an `m`-valuation, so we'll call this class `Valued`.

> class
>   ( Eq (Value a), Monoid (Value a)
>   ) => Valued a
>   where
>     type Value a :: *
>     value :: a -> Value a

This is where the `MultiParamTypeClasses` extension is needed. And the extension is genuinely necessary here; in general we may want multiple valuations on the same `a`, and likewise multiple types valued by the same `m`. So as far as I can see we can't drop MPTC in favor of some other mechanism like associated type families.

Note that the `Valued` class does not impose any laws. It can't, really, because at this level of abstraction there's no structure on `a` to relate to that of `m`. Normally this is a design smell -- generally typeclasses should come with laws -- but here we'll let it slide.

Now lets define a concrete monoid and `Valued` instance for testing. Note that while `Valued` is a many-to-many type class constraint, for most applications we'll want to define a specific monoid type `m` just for that purpose -- even if it's just a `newtype` wrapper around some other type. For example, here's a wrapper around `Int` that is a monoid under addition.

> newtype Trivial a = Trivial
>   { unTrivial :: a
>   } deriving (Eq, Show)

> instance Functor Trivial where
>   fmap f (Trivial a) = Trivial (f a)

> instance Foldable Trivial where
>   foldr f e (Trivial x) = f x e

> instance Applicative Trivial where
>   pure = Trivial
>   (Trivial f) <*> (Trivial x) = Trivial (f x)

> instance Traversable Trivial where
>   traverse f (Trivial x) = fmap Trivial $ f x

> instance Valued (Trivial a) where
>   type Value (Trivial a) = ()
> 
>   value :: Trivial a -> ()
>   value _ = ()

> instance (Num a) => Num (Trivial a) where
>   fromInteger = Trivial . fromInteger
>   (Trivial u) + (Trivial v) = Trivial (u + v)
>   (Trivial u) * (Trivial v) = Trivial (u * v)
>   negate (Trivial u) = Trivial (negate u)
>   abs (Trivial u) = Trivial (abs u)
>   signum (Trivial u) = Trivial (signum u)

> instance (Fmt.Display a) => Fmt.Display (Trivial a) where
>   display (Trivial a) = string "Trivial" <+> display a
> 
> instance (Arb a) => Arb (Trivial a) where
>   arb = fmap Trivial arb
> 
> instance (Prune a) => Prune (Trivial a) where
>   prune = map Trivial . prune . unTrivial

> instance (CoArb a) => CoArb (Trivial a) where
>   coarb (Trivial a) = coarb a

> instance (MakeTo a) => MakeTo (Trivial a) where
>   makeTo = makeToExtendWith makeTo h g
>     where
>       g :: a -> Trivial a
>       g = Trivial
> 
>       h :: Trivial a -> a
>       h = unTrivial



> data Count
>   = Count { unCount :: Int }
>   deriving (Eq, Show, Generic)
> 
> instance Semigroup Count where
>   (Count a) <> (Count b) = Count (a + b)
> 
> instance Monoid Count where
>   mempty = Count 0
> 
> instance Fmt.Display Count where
>   display (Count a) = string "Count" <+> display a
> 
> instance Arb Count where
>   arb = Count <$> arb
> 
> instance Prune Count where
>   prune (Count k) =
>     map Count $ prune k
> 
> instance CoArb Count where
>   coarb (Count k) = coarb k
> 
> instance MakeTo Count where
>   makeTo = makeToIntegralWith g h
>     where
>       g :: Count -> Integer
>       g (Count k) = fromIntegral k
> 
>       h :: Integer -> Count
>       h k = Count $ fromInteger $ abs k



> newtype Counted a = Counted
>   { unCounted :: a
>   } deriving Eq

> instance (Show a) => Show (Counted a) where
>   show (Counted a) = "Counted " ++ f (show a)
>     where
>       f xs = if elem ' ' xs then "(" ++ xs ++ ")" else xs

> instance Valued (Counted a) where
>   type Value (Counted a) = Count
> 
>   value :: Counted a -> Count
>   value _ = Count 1

> instance Functor Counted where
>   fmap f (Counted x) = Counted (f x)

> instance Foldable Counted where
>   foldr f e (Counted x) = f x e

> instance Applicative Counted where
>   pure = Counted
>   (Counted f) <*> (Counted x) = Counted (f x)

> instance Traversable Counted where
>   traverse f (Counted x) = fmap Counted $ f x

> instance (Num a) => Num (Counted a) where
>   fromInteger = Counted . fromInteger
>   (Counted u) + (Counted v) = Counted (u + v)
>   (Counted u) * (Counted v) = Counted (u * v)
>   negate (Counted u) = Counted (negate u)
>   abs (Counted u) = Counted (abs u)
>   signum (Counted u) = Counted (signum u)

> instance (Fmt.Display a) => Fmt.Display (Counted a) where
>   display (Counted a) = string "Counted" <+> display a
> 
> instance (Arb a) => Arb (Counted a) where
>   arb = fmap Counted arb
> 
> instance (Prune a) => Prune (Counted a) where
>   prune (Counted x) = map Counted $ prune x

> instance (CoArb a) => CoArb (Counted a) where
>   coarb (Counted a) = coarb a

> instance (MakeTo a) => MakeTo (Counted a) where
>   makeTo = makeToExtendWith makeTo h g
>     where
>       g :: a -> Counted a
>       g = Counted
> 
>       h :: Counted a -> a
>       h = unCounted




> newtype Self a = Self
>   { unSelf :: a
>   } deriving (Eq, Show)

> instance (Semigroup a) => Semigroup (Self a) where
>   (Self u) <> (Self v) = Self (u <> v)
> 
> instance (Monoid a) => Monoid (Self a) where
>   mempty = Self mempty

> instance (Monoid a, Eq a) => Valued (Self a) where
>   type Value (Self a) = a
> 
>   value :: Self a -> a
>   value (Self a) = a

> instance Functor Self where
>   fmap f (Self a) = Self (f a)

> instance (Fmt.Display a) => Fmt.Display (Self a) where
>   display (Self a) = string "Self" <+> display a
> 
> instance (Arb a) => Arb (Self a) where
>   arb = fmap Self arb
> 
> instance (Prune a) => Prune (Self a) where
>   prune = map Self . prune . unSelf

> instance (CoArb a) => CoArb (Self a) where
>   coarb (Self a) = coarb a

> instance (MakeTo a) => MakeTo (Self a) where
>   makeTo = makeToExtendWith makeTo h g
>     where
>       g :: a -> Self a
>       g = Self
> 
>       h :: Self a -> a
>       h = unSelf



> instance (Valued a1, Valued a2) => Valued (a1,a2) where
>   type Value (a1,a2) = (Value a1, Value a2)
> 
>   value :: (a1,a2) -> (Value a1, Value a2)
>   value (a1,a2) = (value a1, value a2)

> instance (Valued a1, Valued a2, Valued a3) => Valued (a1,a2,a3) where
>   type Value (a1,a2,a3) = (Value a1, Value a2, Value a3)
> 
>   value :: (a1,a2,a3) -> (Value a1, Value a2, Value a3)
>   value (a1,a2,a3) = (value a1, value a2, value a3)

> instance (Valued a) => Valued (Identity a) where
>   type Value (Identity a) = Value a
> 
>   value :: Identity a -> Value a
>   value = value . unIdentity

> instance (Valued (f (g a))) => Valued (Compose f g a) where
>   type Value (Compose f g a) = Value (f (g a))
> 
>   value :: Compose f g a -> Value (f (g a))
>   value = value . unCompose
