---
title: Shrinkable, Showable Functions
author: nbloomf
---

::: frontmatter

> module Kreb.Prop.Fun where
> 
> import Data.List (intersperse, subsequences)
> import Data.Word (Word8)
> import Data.Char (ord, chr)
> 
> import qualified Kreb.Format as Fmt
> import           Kreb.Format
> 
> import Kreb.Prop.Sample
> import Kreb.Prop.Arb

:::

 

Introduction
------------

The `Arb` class is inhabited by types of which we can generate arbitrary values, and `Prune` by types whose values can be made "smaller" in some sense. Both are used to search for counterexamples to properties we want to validate for our programs.

There's a hitch in how this works that makes it not very effective for testing higher-order functions though: the usual function type `a -> b` cannot be "shrunk" in a meaningful way, nor can it be printed as a string, which is essential for tracking down why a property fails.

In this module we'll fix this problem by defining an alternative to the `a -> b` type that _can_ be shrunk and shown. This strategy comes from the paper [Shrinking and showing functions](https://dl.acm.org/doi/10.1145/2364506.2364516) by Koen Claessen, which I highly recommend. Notably, this approach is independent of the internals of property testing, and was discovered several years after the original QuickCheck implementation. The key insight is that although an arbitrary function is (unless both the domain and codomain are finite) an infinite object, when a function witnesses a failing property tests it is only _applied_ to finitely many inputs. So for the purpose of the test failure, we only need to consider the restriction of the function to a minimal, finite support.

First we define a helper type of finite functions, which we call `To`, as a GADT with some "algebraic" function constructors. `Tabular` defines functions on finite types; `Uncurry` and `Disjoin` define functions on product and sum types; and `Extend` define functions on "subtypes".

> data To a b where
>   Tabular :: (Eq a) => [(a,c)] -> To a c
>   Trivial :: c -> To () c
>   Uncurry :: To a (To b c) -> To (a,b) c
>   Disjoin :: To a c -> To b c -> To (Either a b) c
>   Extend  :: (a -> b) -> (b -> a) -> To b c -> To a c
> 
> instance Functor (To a) where
>   fmap :: (b -> c) -> To a b -> To a c
>   fmap f z = case z of
>     Tabular ps -> Tabular $
>       map (\(a, b) -> (a, f b)) ps
>     Trivial c -> Trivial (f c)
>     Uncurry q -> Uncurry $
>       fmap (fmap f) q
>     Disjoin q1 q2 -> Disjoin
>       (fmap f q1) (fmap f q2)
>     Extend r i q -> Extend r i (fmap f q)

Every algebraic type we might define can be broken down as a combination of these. Since `To` is finite, it can be converted to a list of input/output pairs.

> mkTable :: To a c -> [(a, c)]
> mkTable z = case z of
>   Tabular ps -> ps
>   Trivial c -> [((),c)]
>   Uncurry q1 -> do
>     (x,q2) <- mkTable q1
>     (y,c) <- mkTable q2
>     return ((x,y),c)
>   Disjoin q1 q2 ->
>     [ (Left x, c) | (x,c) <- mkTable q1 ] ++
>     [ (Right y, c) | (y,c) <- mkTable q2 ]
>   Extend _ i q ->
>     [ (i x, c) | (x,c) <- mkTable q ]

Every `To` can also be converted to a normal function if we specify a default value for inputs outside the support; this is what `totalize` does.

> totalize :: c -> To a c -> (a -> c)
> totalize w z = \a -> case z of
>   Tabular ps ->
>     head $ [ c | (x,c) <- ps, x == a] ++ [w]
>   Trivial c -> c
>   Uncurry q ->
>     totalize w (fmap (\r -> totalize w r (snd a)) q) (fst a)
>   Disjoin q1 q2 -> case a of
>     Left u -> totalize w q1 u
>     Right v -> totalize w q2 v
>   Extend r _ q -> totalize w q (r a)

Notably, because `To` is finite, we can give it a useful `Display` instance:

> instance
>   ( Fmt.Display a, Fmt.Display b
>   ) => Fmt.Display (To a b)
>   where
>     display q = displayTo q Nothing
> 
> displayTo
>   :: ( Fmt.Display a, Fmt.Display b )
>   => To a b -> Maybe b -> Fmt.Doc u
> displayTo q def =
>   let
>     img (x,c) =
>       display x <+> string "->" <+> display c
>     rest = case def of
>       Nothing -> string "_ ->" <+> string "undefined"
>       Just w  -> string "_ ->" <+> display w
>   in char 'λ' <> Fmt.braceList
>     ( map img (mkTable q) ++ [ rest ] )

We can also give `To` a meaningful prune instance.

> instance
>   ( Prune c
>   ) => Prune (To a c)
>   where
>     prune = pruneTo prune
> 
> pruneTo
>   :: (c -> [c]) -> To a c -> [To a c]
> pruneTo p z = case z of
>   Tabular ps -> case ps of
>     [] -> []
>     _ -> map Tabular $ init $ subsequences ps
>   Trivial c -> do
>     c' <- p c
>     return $ Trivial c'
>   Uncurry q ->
>     map Uncurry $ pruneTo (pruneTo p) q
>   Disjoin q1 q2 ->
>     [ Disjoin q1 r | r <- pruneTo p q2 ] ++
>     [ Disjoin r q2 | r <- pruneTo p q1 ]
>   Extend r i q ->
>     map (Extend r i) $ pruneTo p q

Next we define a class, `MakeTo`. The inhabitants of `MakeTo` are types `a` such that functions _from_ `a` can be turned into `To a`s.

> class MakeTo a where
>   makeTo :: (a -> b) -> To a b

And now, if we can generate arbitrary functions from `a`, and if arbitrary functions from `a` can be turned into `To a`s, then we can generate arbitrary `To a`s.

> instance
>   ( MakeTo a, CoArb a, Arb b
>   ) => Arb (To a b)
>   where
>     arb = fmap makeTo arb



MakeTo Helpers
--------------

It's not necessarily obvious how to implement `makeTo` for a given type. To help with this we supply some basic helper functions.

`makeToDisjoin` defines `To`s on `Either`s:

> makeToDisjoinWith
>   :: ((a -> c) -> To a c)
>   -> ((b -> c) -> To b c)
>   -> (Either a b -> c)
>   -> To (Either a b) c
> makeToDisjoinWith func1 func2 f =
>   Disjoin (func1 (f . Left)) (func2 (f . Right))
> 
> makeToDisjoin
>   :: ( MakeTo a, MakeTo b )
>   => (Either a b -> c)
>   -> To (Either a b) c
> makeToDisjoin =
>   makeToDisjoinWith makeTo makeTo
> 
> instance
>   ( MakeTo a, MakeTo b
>   ) => MakeTo (Either a b)
>   where
>     makeTo = makeToDisjoin

`makeToUncurry` defines `To`s on tuples:

> makeToUncurryWith
>   :: ((a -> (b -> c)) -> To a (b -> c))
>   -> ((b -> c) -> To b c)
>   -> ((a, b) -> c)
>   -> To (a,b) c
> makeToUncurryWith func1 func2 f =
>   Uncurry $ fmap func2 (func1 $ curry f)
> 
> makeToUncurry
>   :: ( MakeTo a, MakeTo b )
>   => ((a,b) -> c)
>   -> To (a,b) c
> makeToUncurry =
>   makeToUncurryWith makeTo makeTo
> 
> instance
>   ( MakeTo a, MakeTo b
>   ) => MakeTo (a,b)
>   where
>     makeTo = makeToUncurry

And `makeToExtend` defines `To`s when we have (1) a `MakeTo` instance already and (2) a way to map to and from the existing `MakeTo` instance.

> makeToExtendWith
>   :: ((b -> c) -> To b c)
>   -> (a -> b) -> (b -> a)
>   -> (a -> c) -> To a c
> makeToExtendWith func g h f =
>   Extend g h $ func (f . h)
> 
> makeToExtend
>   :: ( MakeTo b )
>   => (a -> b) -> (b -> a)
>   -> (a -> c) -> To a c
> makeToExtend =
>   makeToExtendWith makeTo

With these in hand we can start defining `MakeTo` instance for the standard library types. Unit is trivial:

> instance MakeTo () where
>   makeTo f = Trivial $ f ()

The isomorphism `Maybe a === Either () a` gives an instance for `Maybe`:

> instance
>   ( MakeTo a
>   ) => MakeTo (Maybe a)
>   where
>     makeTo =
>       let
>         f :: Either () a -> Maybe a
>         f x = case x of
>           Left () -> Nothing
>           Right a -> Just a
> 
>         g :: Maybe a -> Either () a
>         g x = case x of
>           Nothing -> Left ()
>           Just a -> Right a
>       in makeToExtend g f

The associators on tuple types give instances for tuples with any number of entries:

> instance
>   ( MakeTo a1, MakeTo a2, MakeTo a3
>   ) => MakeTo (a1, a2, a3)
>   where
>     makeTo =
>       let
>         f :: (a1,(a2,a3)) -> (a1,a2,a3)
>         f (a1,(a2,a3)) = (a1,a2,a3)
> 
>         g :: (a1,a2,a3) -> (a1,(a2,a3))
>         g (a1,a2,a3) = (a1,(a2,a3))
>       in makeToExtend g f
> 
> instance
>   ( MakeTo a1, MakeTo a2, MakeTo a3, MakeTo a4
>   ) => MakeTo (a1, a2, a3, a4)
>   where
>     makeTo =
>       let
>         f :: (a1,(a2,a3,a4)) -> (a1,a2,a3,a4)
>         f (a1,(a2,a3,a4)) = (a1,a2,a3,a4)
> 
>         g :: (a1,a2,a3,a4) -> (a1,(a2,a3,a4))
>         g (a1,a2,a3,a4) = (a1,(a2,a3,a4))
>       in makeToExtend g f
> 
> instance
>   ( MakeTo a1, MakeTo a2, MakeTo a3, MakeTo a4, MakeTo a5
>   ) => MakeTo (a1, a2, a3, a4, a5)
>   where
>     makeTo =
>       let
>         f :: (a1,(a2,a3,a4,a5)) -> (a1,a2,a3,a4,a5)
>         f (a1,(a2,a3,a4,a5)) = (a1,a2,a3,a4,a5)
> 
>         g :: (a1,a2,a3,a4,a5) -> (a1,(a2,a3,a4,a5))
>         g (a1,a2,a3,a4,a5) = (a1,(a2,a3,a4,a5))
>       in makeToExtend g f

Small finite types are easy enough with `Tabular`.

> instance MakeTo Bool where
>   makeTo f = Tabular
>     [ (True,  f True)
>     , (False, f False)
>     ]
> 
> instance MakeTo Word8 where
>   makeTo f = Tabular
>     [ (x, f x) | x <- [minBound, maxBound] ]

The isomorphism `[a] === Either () (a, [a])` gives a (recursive!) instance for lists:

> instance
>   ( MakeTo a
>   ) => MakeTo [a]
>   where
>     makeTo = makeToExtend g h
>       where
>         -- uncons
>         g :: [a] -> Either () (a, [a])
>         g z = case z of
>           [] -> Left ()
>           x:xs -> Right (x,xs)
> 
>         -- cons
>         h :: Either () (a, [a]) -> [a]
>         h z = case z of
>           Left () -> []
>           Right (x,xs) -> x:xs

The isomorphism `Integer === [Word]` taking an integer to its base 256 representation gives an instance for `Integer`:

> instance MakeTo Integer where
>   makeTo = makeToExtend g h
>     where
>       -- to signed digits
>       g :: Integer -> Either [Word8] [Word8]
>       g n = if n < 0
>         then Left $ gNat ((abs n) - 1)
>         else Right $ gNat n
> 
>       -- from signed digits
>       h :: Either [Word8] [Word8] -> Integer
>       h z = case z of
>         Left ws -> negate (1 + hNat ws)
>         Right ws -> hNat ws 
> 
>       -- to digits
>       gNat :: Integer -> [Word8]
>       gNat n = if n <= 0
>         then []
>         else let (q,r) = quotRem n 258 in
>           (fromIntegral r :: Word8) : gNat q
> 
>       -- from digits
>       hNat :: [Word8] -> Integer
>       hNat z = case z of
>         [] -> 0
>         x:xs -> (fromIntegral x) + 256 * (hNat xs)

Now plenty of types can be mapped to and from `Integer`, and we can give a helper for defining `makeTo` on these.

> makeToIntegralWith
>   :: (a -> Integer) -> (Integer -> a)
>   -> (a -> b) -> To a b
> makeToIntegralWith g h =
>   makeToExtend g h
> 
> makeToIntegral
>   :: ( Num a, Integral a )
>   => (a -> b) -> To a b
> makeToIntegral =
>   makeToIntegralWith fromIntegral fromInteger
> 
> instance MakeTo Int where
>   makeTo = makeToIntegral
> 
> instance MakeTo Char where
>   makeTo = makeToExtend ord chr



An Alternative Function Type
----------------------------

We're nearly done with the API for shrinkable functions. To wrap up, we need a type to track a (possibly pruned) `To`, along with an equivalent "normal" function.

> data Fun a b
>   = Fun (To a b) b Pruned (a -> b)
> 
> data Pruned
>   = Pruned | NotPruned
>   deriving (Eq, Show)
> 
> instance Functor (Fun a) where
>   fmap f (Fun part def p tot) =
>     Fun (fmap f part) (f def) p (f . tot)
> 
> mkFun :: To a b -> b -> Fun a b
> mkFun part def =
>   Fun part def NotPruned (totalize def part)

Now `Fun` can be displayed, generated, and pruned:

> instance
>   ( Fmt.Display a, Fmt.Display b
>   ) => Fmt.Display (Fun a b)
>   where
>     display (Fun part def p _) =
>       case p of
>         NotPruned -> string "<func>"
>         Pruned    -> displayTo part (Just def)
> 
> instance
>   ( MakeTo a, CoArb a, Arb b
>   ) => Arb (Fun a b)
>   where
>     arb = mkFun <$> arb <*> arb
> 
> instance
>   ( MakeTo a, CoArb a, Arb b, Prune a, Prune b
>   ) => Prune (Fun a b)
>   where
>     prune (Fun part def p tot) = concat
>       [ [ mkFun p' d' | (p', d') <- prune (part, def) ]
>       , if p == Pruned
>           then []
>           else [ Fun part def Pruned tot ]
>       ]

And that's it. Anytime we want to use a function as an arbitrary parameter in a property test, we can use the `Fun` type instead, and get nicely pruned and displayed results. The only other necessary adjustment is that we have to explicitly turn the `Fun` into a "normal" function for use inside the property. The `apFun*` helpers do this.

> apFun, (@@)
>   :: Fun a b -> a -> b
> apFun (Fun _ _ _ f) = f
> (@@) = apFun
> 
> apFun2
>   :: Fun (a,b) c -> a -> b -> c
> apFun2 (Fun _ _ _ f) a b = f (a, b)
> 
> apFun3
>   :: Fun (a,b,c) d -> a -> b -> c -> d
> apFun3 (Fun _ _ _ f) a b c = f (a,b,c)
