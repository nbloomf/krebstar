> module Kreb.Check.Arb where
> 
> import Data.Char (ord)
> 
> import Kreb.Check.Seeded



> class Arb t where
>   arb :: Seeded t
> 
> class CoArb t where
>   coarb :: t -> Seeded u -> Seeded u
> 
> class Prune t where
>   prune :: t -> [t]



Unit
----

> instance Arb () where
>   arb = return ()
> 
> instance CoArb () where
>   coarb _ = id
> 
> instance Prune () where
>   prune _ = []



Bool
----

> instance Arb Bool where
>   arb = pickFrom2
>     ( return True
>     , return False
>     )
> 
> instance CoArb Bool where
>   coarb p =
>     if p
>       then twiddle 0
>       else twiddle 1
> 
> instance Prune Bool where
>   prune _ = []



Int
---

> instance Arb Int where
>   arb = rand
> 
> instance CoArb Int where
>   coarb = twiddle
> 
> instance Prune Int where
>   prune k = case compare k 0 of
>     LT -> filter (> k) [0, -1, k`div`2, k+1]
>     EQ -> []
>     GT -> filter (< k) [0, 1, k`div`2, k-1]



Char
----

> instance Arb Char where
>   arb = freq
>     [ (4, arbAsciiChar)
>     , (1, arbUnicodeChar)
>     ]
> 
> instance Prune Char where
>   prune c = ['a']
> 
> instance CoArb Char where
>   coarb c = coarb (ord c)



Maybe
-----

> instance
>   ( Arb a
>   ) => Arb (Maybe a)
>   where
>     arb = freq
>       [ (1, return Nothing)
>       , (5, Just <$> arb)
>       ]
> 
> instance
>   ( CoArb a
>   ) => CoArb (Maybe a)
>   where
>     coarb x = case x of
>       Nothing -> twiddle 0
>       Just a  -> twiddle 1 . coarb a
> 
> instance
>   ( Prune a
>   ) => Prune (Maybe a)
>   where
>     prune x = case x of
>       Nothing -> []
>       Just a -> Nothing : (fmap Just $ prune a)



Either
------



Tuples
------

> instance
>   ( Arb a1, Arb a2
>   ) => Arb (a1, a2)
>   where
>     arb = do
>       a1 <- arb
>       a2 <- arb
>       return (a1, a2)
> 
> instance
>   ( Prune a1, Prune a2
>   ) => Prune (a1, a2)
>   where
>     prune (a1,a2) = concat
>       [ [ (u1,a2) | u1 <- prune a1 ]
>       , [ (a1,u2) | u2 <- prune a2 ]
>       ]
> 
> instance
>   ( CoArb a1, CoArb a2
>   ) => CoArb (a1,a2)
>   where
>     coarb (a1,a2) =
>       coarb a1 .
>       coarb a2

> instance
>   ( Arb a1, Arb a2, Arb a3
>   ) => Arb (a1, a2, a3)
>   where
>     arb = do
>       a1 <- arb
>       a2 <- arb
>       a3 <- arb
>       return (a1, a2, a3)
> 
> instance
>   ( Prune a1, Prune a2, Prune a3
>   ) => Prune (a1, a2, a3)
>   where
>     prune (a1,a2,a3) = concat
>       [ [ (u1,a2,a3) | u1 <- prune a1 ]
>       , [ (a1,u2,a3) | u2 <- prune a2 ]
>       , [ (a1,a2,u3) | u3 <- prune a3 ]
>       ]
> 
> instance
>   ( CoArb a1, CoArb a2, CoArb a3
>   ) => CoArb (a1,a2,a3)
>   where
>     coarb (a1,a2,a3) =
>       coarb a1 .
>       coarb a2 .
>       coarb a3



Lists
-----

> instance
>   ( Arb a
>   ) => Arb [a]
>   where
>     arb = listOf arb
> 
> instance
>   ( Prune a
>   ) => Prune [a]
>   where
>     prune z = case z of
>       [] -> []
>       x:xs -> concat
>        [ [[]]
>        , if null xs then [] else map (:[]) (x:xs)
>        , prune xs
>        , map (x:) (prune xs)
>        , map (:xs) (prune x)
>        ]
> 
> instance
>   ( CoArb a
>   ) => CoArb [a]
>   where
>     coarb z = case z of
>       []   -> twiddle 0
>       x:xs -> twiddle 1 . coarb (x,xs)







Function
--------

> instance
>   ( CoArb a, Arb b
>   ) => Arb (a -> b)
>   where
>     arb = promote $ \a -> coarb a arb
> 
> instance
>   ( Arb a, CoArb b
>   ) => CoArb (a -> b)
>   where
>     coarb f gen = do
>       xs <- arb
>       coarb (map f xs) gen













> newtype NonNegative a
>   = NonNegative a
>   deriving (Eq, Show)
> 
> instance
>   ( Num a, Arb a
>   ) => Arb (NonNegative a)
>   where
>     arb = do
>       k <- arb
>       return $ NonNegative $ abs k
> 
> instance
>   ( Num a, Prune a
>   ) => Prune (NonNegative a)
>   where
>     prune (NonNegative a) =
>       map (NonNegative . abs) $ prune a
> 
> instance
>   ( CoArb a
>   ) => CoArb (NonNegative a)
>   where
>     coarb (NonNegative a) = coarb a

> newtype Positive a
>   = Positive a
>   deriving (Eq, Show)
> 
> instance
>   ( Num a, Arb a
>   ) => Arb (Positive a)
>   where
>     arb = do
>       k <- arb
>       return $ Positive $ (+1) $ abs k
> 
> instance
>   ( Num a, Prune a
>   ) => Prune (Positive a)
>   where
>     prune (Positive a) =
>       map (Positive . (+1) . abs) $ prune a
> 
> instance
>   ( CoArb a
>   ) => CoArb (Positive a)
>   where
>     coarb (Positive a) = coarb a

