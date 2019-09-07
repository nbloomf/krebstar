> {-# LANGUAGE
>     ScopedTypeVariables
> #-}

> module Kreb.Check.Seeded where

> import Control.Monad (ap)
> import System.Random
> import Data.Bits

> data Seed = Seed
>   { _stdgen :: StdGen
>   , _depth :: ZZ Depth
>   , _size :: ZZ Size
>   } deriving Show
> 
> seedWith
>   :: StdGen -> ZZ Depth -> ZZ Size -> Seed
> seedWith g d s = Seed
>   { _stdgen = g
>   , _depth = d
>   , _size = s
>   }

> newtype ZZ t = ZZ
>   { unZZ :: Int
>   } deriving (Eq, Ord, Show)
> 
> instance Num (ZZ t) where
>   fromInteger = ZZ . fromInteger
>   abs (ZZ a) = ZZ (abs a)
>   signum (ZZ a) = ZZ (signum a)
>   negate (ZZ a) = ZZ (negate a)
>   (ZZ a) + (ZZ b) = ZZ (a + b)
>   (ZZ a) * (ZZ b) = ZZ (a * b)

> data Depth
> data Size



> newtype Seeded a = Seeded
>   { unSeeded :: Seed -> a }
> 
> runSeeded
>   :: Seed -> Seeded a -> a
> runSeeded env (Seeded f) = f env
> 
> runSeededWith
>   :: StdGen -> ZZ Depth -> ZZ Size -> Seeded a -> a
> runSeededWith g d s =
>   runSeeded $ seedWith g d s



> instance Functor Seeded where
>   fmap f x = x >>= (return . f)
> 
> instance Applicative Seeded where
>   pure = return
>   (<*>) = ap
> 
> instance Monad Seeded where
>   return a = Seeded $ \_ -> a
> 
>   (Seeded x) >>= f = Seeded $ \env ->
>     let
>       (seed1, seed2) = split (_stdgen env)
>       Seeded y = f (x $ env { _stdgen = seed1 })
>     in y $ env { _stdgen = seed2 }

> askSize :: Seeded (ZZ Size)
> askSize = Seeded $ \env -> _size env
> 
> askDepth :: Seeded (ZZ Depth)
> askDepth = Seeded $ \env -> _depth env

> randIn
>   :: ( Random a )
>   => (a,a) -> Seeded a
> randIn bds = Seeded $ \env ->
>   fst $ randomR bds (_stdgen env)
> 
> rand
>   :: ( Random a )
>   => Seeded a
> rand = Seeded $ \env ->
>   fst $ random (_stdgen env)

> vectOf :: Int -> Seeded a -> Seeded [a]
> vectOf n x = sequence $ replicate n x

> listOf :: Seeded a -> Seeded [a]
> listOf gen = do
>   ZZ sz <- askSize
>   k <- randIn (0, sz)
>   vectOf k gen

> pickFrom2
>   :: (Seeded a, Seeded a)
>   -> Seeded a
> pickFrom2 (a0, a1) = do
>   i <- randIn (0 :: Int, 1)
>   case i of
>     0 -> a0
>     _ -> a1
> 
> pickFrom3
>   :: (Seeded a, Seeded a, Seeded a)
>   -> Seeded a
> pickFrom3 (a0, a1, a2) = do
>   i <- randIn (0 :: Int, 2)
>   case i of
>     0 -> a0
>     1 -> a1
>     _ -> a2

> freq
>   :: forall a. [(Int, Seeded a)] -> Seeded a
> freq ws = case ws of
>   [] -> error "Kreb.Check.Seeded.freq: undefined on []"
>   (k0,x0):vs -> if any (< 0) $ map fst ws
>     then error "Kreb.Check.Seeded.freq: negative weight"
>     else if all (== 0) $ map fst ws
>       then error "Kreb.Check.Seeded.freq: all weights zero"
>       else randIn (1, total) >>= thresh vs
>         where
>           total :: Int
>           total = sum $ map fst ws
> 
>           thresh
>             :: [(Int, Seeded a)] -> Int -> Seeded a
>           thresh zs n = case zs of
>             [] -> x0
>             (k,x):us -> if n <= k
>               then x
>               else thresh us (n-k)

> promote :: (a -> Seeded b) -> Seeded (a -> b)
> promote f = Seeded $ \env -> \a ->
>   let Seeded m = f a in m env



> twiddle
>   :: ( Integral n )
>   => n -> Seeded a -> Seeded a
> twiddle k (Seeded f) = Seeded $ \env ->
>   f $ env { _stdgen = twiddleInt (toInteger k) (_stdgen env) }
> 
> twiddleInt
>   :: Integer -> StdGen -> StdGen
> twiddleInt k gen =
>   let (gen1, gen2) = split gen
>   in if k >= 1
>     then gamma k gen1
>     else gamma (1-k) gen2
>   where
>     gamma :: Integer -> StdGen -> StdGen
>     gamma n =
>       let k = ilog2 n
>       in encode k . zeroes k
>       where
>         encode :: Int -> StdGen -> StdGen
>         encode m gen = if m <= -1
>           then gen
>           else let (gen1, gen2) = split gen in
>             if testBit n m
>               then encode (m-1) gen2
>               else encode (m-1) gen1
> 
>     zeroes :: Int -> StdGen -> StdGen
>     zeroes m gen = if m <= 0
>       then gen
>       else fst $ split gen
> 
>     ilog2 :: Integer -> Int
>     ilog2 1 = 0
>     ilog2 n = 1 + ilog2 (n `div` 2)