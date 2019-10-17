> {-# LANGUAGE
>     InstanceSigs
>   , GADTs
> #-}

> module Kreb.Check.Fun (
>     To(..)
> 
>   , mkTable
>   , totalize
>   , pruneTo
> 
>   , MakeTo(..)
>   , makeToExtendWith
>   , makeToIntegralWith
> 
>   , Fun(..)
>   , apFun
>   , apFun2
>   , apFun3
>   , (@@)
> ) where
> 
> import Data.List (intersperse, subsequences)
> import Data.Word (Word8)
> import Data.Char (ord, chr)
> 
> import Kreb.Check.Seeded
> import Kreb.Check.Arb



> data To a b where
>   Tabular :: (Eq a) => [(a,c)] -> To a c
>   Trivial :: c -> To () c
>   Uncurry :: To a (To b c) -> To (a,b) c
>   Disjoin :: To a c -> To b c -> To (Either a b) c
>   Extend :: (a -> b) -> (b -> a) -> To b c -> To a c

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

> showTo
>   :: ( Show a, Show b )
>   => To a b -> Maybe b -> String
> showTo q def = mconcat
>   [ "λ{ "
>   , mconcat $ intersperse ", "
>     [ show x ++ " -> " ++ show c | (x,c) <- mkTable q ]
>   , case def of
>       Nothing -> ""
>       Just w -> ", _ -> " ++ show w
>   , " }"
>   ]

> instance
>   ( Show a, Show b
>   ) => Show (To a b)
>   where
>     show q = showTo q Nothing

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





> class MakeTo a where
>   makeTo :: (a -> b) -> To a b

> instance
>   ( MakeTo a, CoArb a, Arb b
>   ) => Arb (To a b)
>   where
>     arb = fmap makeTo arb
> 
> instance
>   ( MakeTo a, CoArb a, Arb b, Prune b
>   ) => Prune (To a b)
>   where
>     prune = pruneTo prune



Base Constructors
-----------------

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





> instance MakeTo () where
>   makeTo f = Trivial $ f ()

> instance MakeTo Bool where
>   makeTo f = Tabular
>     [ (True, f True)
>     , (False, f False)
>     ]

> instance MakeTo Word8 where
>   makeTo f = Tabular
>     [ (x, f x) | x <- [minBound, maxBound] ]

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

> makeToIntegralWith
>   :: (a -> Integer) -> (Integer -> a)
>   -> (a -> b) -> To a b
> makeToIntegralWith g h =
>   makeToExtend g h

> makeToIntegral
>   :: ( Num a, Integral a )
>   => (a -> b) -> To a b
> makeToIntegral =
>   makeToIntegralWith fromIntegral fromInteger

> instance MakeTo Int where
>   makeTo = makeToIntegral

> instance MakeTo Char where
>   makeTo = makeToExtend ord chr





> data Fun a b
>   = Fun (To a b) b Pruned (a -> b)
> 
> data Pruned
>   = Pruned | NotPruned
>   deriving (Eq, Show)

> instance Functor (Fun a) where
>   fmap f (Fun part def p tot) =
>     Fun (fmap f part) (f def) p (f . tot)

> mkFun :: To a b -> b -> Fun a b
> mkFun part def =
>   Fun part def NotPruned (totalize def part)

> apFun, (@@)
>   :: Fun a b -> a -> b
> apFun (Fun _ _ _ f) = f
> (@@) = apFun

> apFun2
>   :: Fun (a,b) c -> a -> b -> c
> apFun2 (Fun _ _ _ f) a b = f (a, b)
> 
> apFun3
>   :: Fun (a,b,c) d -> a -> b -> c -> d
> apFun3 (Fun _ _ _ f) a b c = f (a,b,c)

> instance
>   ( Show a, Show b
>   ) => Show (Fun a b)
>   where
>     show (Fun part def p _) =
>       case p of
>         NotPruned -> "<func>"
>         Pruned -> showTo part (Just def)

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

> instance MakeTo (ZZ a) where
>   makeTo = makeToIntegralWith g h
>     where
>       g :: ZZ a -> Integer
>       g (ZZ k) = fromIntegral k
> 
>       h :: Integer -> ZZ a
>       h k = ZZ $ fromInteger $ abs k
