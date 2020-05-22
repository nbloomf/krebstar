> module Kreb.Optics.Combinator where

> import Prelude ((.), ($), id, Maybe(..), Monad(..))
> import Data.Void

> import Kreb.Category

> import Kreb.Optics.Class.Tambara



Viewing
=======

> newtype Viewing a b s t =
>   Viewing { getView :: s -> a }

> instance
>   ProfunctorC obj1 (MapOn obj1) obj2 (MapOn obj2) Hask (->) (Viewing a b)
>   where
>     dimapC (Map f) _ (Viewing h) = Viewing (h . f)

> instance
>   ( MonoidalActionC obj3 (MapOn obj3) (PairC obj3 obj3) () obj1 (MapOn obj1) (PairC obj3 obj1)
>   , MonoidalActionC obj3 (MapOn obj3) (PairC obj3 obj3) () obj2 (MapOn obj2) (PairC obj3 obj2)
>   ) => Tambara obj1 (MapOn obj1) obj2 (MapOn obj2) obj3 (MapOn obj3)
>     (PairC obj3 obj3) () (PairC obj3 obj1) (PairC obj3 obj2) (Viewing a b)
>   where
>     tambara (Viewing f) = Viewing (f . (\(PairC _ x) -> x))

instance (Monad m) => Tambara Any (->) Any (->) (Algebra m) (->) (,) () (,) (,) (Viewing a b) where
  tambara = tambara @Any @(->) @Any @(->) @Any @(->) @(,) @()

> (^.) :: s -> (Viewing a b a b -> Viewing a b s t) -> a
> (^.) s l = getView (l $ Viewing id) s
> infixl 8 ^.



Previewing
----------

-- | Previewing is a profunctor that can be used to implement a
-- 'preview' operation.  Previewing is a Tambara module for all the
-- optics that admit the 'preview' operator.

> newtype Previewing a b s t =
>   Previewing { getPreview :: s -> Maybe a }

> instance
>   ProfunctorC obj1 (MapOn obj1) obj2 (MapOn obj2) Hask (->) (Previewing a b)
>   where
>     dimapC (Map l) _ (Previewing f) = Previewing (f . l)

> instance
>   ( MonoidalActionC obj3 (MapOn obj3) (PairC obj3 obj3) () obj1 (MapOn obj1) (PairC obj3 obj1)
>   , MonoidalActionC obj3 (MapOn obj3) (PairC obj3 obj3) () obj2 (MapOn obj2) (PairC obj3 obj2)
>   ) => Tambara obj1 (MapOn obj1) obj2 (MapOn obj2) obj3 (MapOn obj3)
>     (PairC obj3 obj3) () (PairC obj3 obj1) (PairC obj3 obj2) (Previewing a b)
>   where
>     tambara (Previewing f) = Previewing $ \(PairC _ x) -> f x

> instance
>   ( MonoidalActionC obj3 (MapOn obj3) (ChoiceC obj3 obj3) Void obj1 (MapOn obj1) (ChoiceC obj3 obj1)
>   , MonoidalActionC obj3 (MapOn obj3) (ChoiceC obj3 obj3) Void obj2 (MapOn obj2) (ChoiceC obj3 obj2)
>   ) => Tambara obj1 (MapOn obj1) obj2 (MapOn obj2) obj3 (MapOn obj3)
>     (ChoiceC obj3 obj3) Void (ChoiceC obj3 obj1) (ChoiceC obj3 obj2) (Previewing a b)
>   where
>     tambara (Previewing f) = Previewing $
>       \x -> case x of
>         Choice1 _ -> Nothing; Choice2 a -> f a


> infixl 8 ?.
> (?.) :: s -> (Previewing a b a b -> Previewing a b s t) -> Maybe a
> (?.) s l = getPreview (l $ Previewing return) s

instance Tambara Any (->) Any (->) Any (->) Either Void Either Either (Previewing a b) where
  tambara (Previewing f) = Previewing (either (\_ -> Nothing) f)
