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
>  -- , Function(..)
> 
>  -- , Fun(..)
>   , apFun
>   , (@@)
> ) where
> 
> import Data.List (intersperse, subsequences)
> import Data.Word (Word8)
> 
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





> class Function a where
>   function :: (a -> b) -> To a b

> instance
>   ( Function a, CoArb a, Arb b
>   ) => Arb (To a b)
>   where
>     arb = fmap function arb
> 
> instance
>   ( Function a, CoArb a, Arb b, Prune b
>   ) => Prune (To a b)
>   where
>     prune = pruneTo prune



Base Constructors
-----------------

> functionDisjoinWith
>   :: ((a -> c) -> To a c)
>   -> ((b -> c) -> To b c)
>   -> (Either a b -> c)
>   -> To (Either a b) c
> functionDisjoinWith func1 func2 f =
>   Disjoin (func1 (f . Left)) (func2 (f . Right))
> 
> functionDisjoin
>   :: ( Function a, Function b )
>   => (Either a b -> c)
>   -> To (Either a b) c
> functionDisjoin =
>   functionDisjoinWith function function
> 
> instance
>   ( Function a, Function b
>   ) => Function (Either a b)
>   where
>     function = functionDisjoin

> functionUncurryWith
>   :: ((a -> (b -> c)) -> To a (b -> c))
>   -> ((b -> c) -> To b c)
>   -> ((a, b) -> c)
>   -> To (a,b) c
> functionUncurryWith func1 func2 f =
>   Uncurry $ fmap func2 (func1 $ curry f)
> 
> functionUncurry
>   :: ( Function a, Function b )
>   => ((a,b) -> c)
>   -> To (a,b) c
> functionUncurry =
>   functionUncurryWith function function
> 
> instance
>   ( Function a, Function b
>   ) => Function (a,b)
>   where
>     function = functionUncurry

> functionExtendWith
>   :: ((b -> c) -> To b c)
>   -> (a -> b) -> (b -> a)
>   -> (a -> c) -> To a c
> functionExtendWith func g h f =
>   Extend g h $ func (f . h)
> 
> functionExtend
>   :: ( Function b )
>   => (a -> b) -> (b -> a)
>   -> (a -> c) -> To a c
> functionExtend =
>   functionExtendWith function





> instance Function () where
>   function f = Trivial $ f ()

> instance Function Bool where
>   function f = Tabular
>     [ (True, f True)
>     , (False, f False)
>     ]

> instance Function Word8 where
>   function f = Tabular
>     [ (x, f x) | x <- [minBound, maxBound] ]

> instance
>   ( Function a
>   ) => Function [a]
>   where
>     function = functionExtend g h
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

> instance Function Integer where
>   function = functionExtend g h
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

> instance Function Int where
>   function = functionExtend fromIntegral fromInteger





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

> instance
>   ( Show a, Show b
>   ) => Show (Fun a b)
>   where
>     show (Fun part def p _) =
>       case p of
>         NotPruned -> "<func>"
>         Pruned -> showTo part (Just def)

> instance
>   ( Function a, CoArb a, Arb b
>   ) => Arb (Fun a b)
>   where
>     arb = mkFun <$> arb <*> arb
> 
> instance
>   ( Function a, CoArb a, Arb b, Prune a, Prune b
>   ) => Prune (Fun a b)
>   where
>     prune (Fun part def p tot) = concat
>       [ [ mkFun p' d' | (p', d') <- prune (part, def) ]
>       , if p == Pruned
>           then []
>           else [ Fun part def Pruned tot ]
>       ]