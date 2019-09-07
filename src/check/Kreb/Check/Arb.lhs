> module Kreb.Check.Arb where
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
