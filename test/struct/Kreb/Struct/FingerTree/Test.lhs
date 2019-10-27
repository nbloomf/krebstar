---
title: Finger Trees -- Tests
---



> {-# LANGUAGE
>     MultiParamTypeClasses
>   , ScopedTypeVariables
>   , FlexibleContexts
> #-}
> 
> module Kreb.Struct.FingerTree.Test (
>     test_FingerTree
>   , Tup(..)
> ) where
> 
> import Prelude hiding (reverse)
> import Data.Proxy
> import Data.Foldable
> 
> import Test.Tasty
> 
> import Kreb.Check
> import Kreb.Struct.Valued
> import Kreb.Struct.FingerTree



These instances are only needed locally. Can we get rid of them?

> instance Semigroup Bool where
>   (<>) = (&&)
> 
> instance Monoid Bool where
>   mempty = True



> test_FingerTree :: TestTree
> test_FingerTree =
>   testGroup "FingerTree"
>     [ test_FingerTree_properties
>         "Count/Char" (Proxy :: Proxy Char) (Proxy :: Proxy Count)
>     , test_FingerTree_properties
>         "Count/Char" (Proxy :: Proxy Bool) (Proxy :: Proxy Tup)
> 
>     , testGroup "Class Laws"
>       [ testGroup "Eq"
>         [ test_Eq_laws (Proxy :: Proxy (FingerTree Count Char))
>         , test_Eq_laws (Proxy :: Proxy (FingerTree Tup Bool))
>         ]
> 
>       , testGroup "Semigroup"
>         [ test_Semigroup_laws (Proxy :: Proxy (FingerTree Count Char))
>         , test_Semigroup_laws (Proxy :: Proxy (FingerTree Tup Bool))
>         ]
> 
>       , testGroup "Monoid"
>         [ test_Monoid_laws (Proxy :: Proxy (FingerTree Count Char))
>         , test_Monoid_laws (Proxy :: Proxy (FingerTree Tup Bool))
>         ]
> 
>       , testGroup "Foldable"
>         [ test_Foldable_laws (Proxy :: Proxy (FingerTree Tup))
>             (Proxy :: Proxy Bool) (Proxy :: Proxy Bool) (Proxy :: Proxy Bool)
>         , test_Foldable_laws (Proxy :: Proxy (FingerTree Count))
>             (Proxy :: Proxy Bool) (Proxy :: Proxy Bool) (Proxy :: Proxy Bool)
> 
>         , test_FoldableFunctor_laws_with
>             (fmapFT :: (Bool -> Bool) -> FingerTree Tup Bool -> FingerTree Tup Bool)
>             (fmapFT :: (Bool -> Bool) -> FingerTree Tup Bool -> FingerTree Tup Bool)
>             (fold :: FingerTree Tup Bool -> Bool)
>             (foldMap :: forall u n . (Monoid n) => (u -> n) -> FingerTree Tup u -> n)
>         , test_FoldableFunctor_laws_with
>             (fmapFT :: (Bool -> Bool) -> FingerTree Count Bool -> FingerTree Count Bool)
>             (fmapFT :: (Bool -> Bool) -> FingerTree Count Bool -> FingerTree Count Bool)
>             (fold :: FingerTree Count Bool -> Bool)
>             (foldMap :: forall u n . (Monoid n) => (u -> n) -> FingerTree Count u -> n)
>         ]
>       ]
>     ]



> test_FingerTree_properties
>   :: forall m a
>    . ( Eq a, Show a, Arb a, Prune a, MakeTo a, Eq m, CoArb a
>      , Show m, MakeTo m, CoArb m, Prune m, Valued m a, Valued m [a] )
>   => String -> Proxy a -> Proxy m -> TestTree
> test_FingerTree_properties label _ _ =
>   let title = "FingerTree (" ++ label ++ ")"
>   in testGroup title
>     [ testKreb
>         "validate a == True" $
>         \(a :: FingerTree m a) ->
>           claimTrue (validate a)
> 
>     , testKreb
>         "isSingleton (singleton a) == True" $
>         \(a :: a) ->
>           let x = singleton a :: FingerTree m a
>           in claimTrue (isSingleton x)
> 
>     , testKreb
>         "Just a == readInit (singleton a)" $
>         \(a :: a) ->
>           let x = singleton a :: FingerTree m a
>           in claimEqual
>             (Just a)
>             (readInit x)
> 
>     , testKreb
>         "Just a == readLast (singleton a)" $
>         \(a :: a) ->
>           let x = singleton a :: FingerTree m a
>           in claimEqual
>             (Just a)
>             (readLast x)
> 
>     , testKreb
>         "False == isEmpty (singleton a)" $
>         \(a :: a) ->
>           let x = singleton a :: FingerTree m a
>           in claimFalse (isEmpty x)
> 
>     , testKreb
>         "(isEmpty as) == (Nothing == uncons as)" $
>         \(as :: FingerTree m a) ->
>           claimEqual
>             (isEmpty as)
>             (Nothing == (uncons as))
> 
>     , testKreb
>         "(isEmpty as) == (Nothing == unsnoc as)" $
>         \(as :: FingerTree m a) ->
>           claimEqual
>             (isEmpty as)
>             (Nothing == (unsnoc as))
> 
>     , testKreb
>         "cons a as == (leaf a) <> as" $
>         \(a :: a) (as :: FingerTree m a) ->
>           claimEqual
>             (cons a as)
>             ((singleton a) <> as)
> 
>     , testKreb
>         "snoc a as == as <> (leaf a)" $
>         \(a :: a) (as :: FingerTree m a) ->
>           claimEqual
>             (snoc a as)
>             (as <> (singleton a))
> 
>     , testKreb
>         "(isEmpty as) || (let Just u us = uncons as in as == cons u us)" $
>         \(as :: FingerTree m a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , case uncons as of
>                 Just (u, us) -> claimEqual as (cons u us)
>                 Nothing -> reject "impossible state"
>             ]
> 
>     , testKreb
>         "(isEmpty as) || (let Just u us = unsnoc as in as == snoc u us)" $
>         \(as :: FingerTree m a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , case unsnoc as of
>                 Just (u, us) -> claimEqual as (snoc u us)
>                 Nothing -> reject "impossible state"
>             ]
> 
>     , testKreb
>         "z == uncons (cons (fst z) (snd z))" $
>         \(a :: a) (as :: FingerTree m a) ->
>           case uncons (cons a as) of
>             Nothing ->
>               reject "impossible state"
>             Just (b, bs) ->
>               (claimEqual a b) .&&. (claimEqual as bs)
> 
>     , testKreb
>         "z == unsnoc (snoc (fst z) (snd z))" $
>         \(a :: a) (as :: FingerTree m a) ->
>           case unsnoc (snoc a as) of
>             Nothing ->
>               reject "impossible state"
>             Just (b, bs) ->
>               (claimEqual a b) .&&. (claimEqual as bs)
> 
>     , testKreb
>         "as == fromList (toList as)" $
>         \(as :: FingerTree m a) ->
>           claimEqual (as) (fromList (toList as))
> 
>     , testKreb
>         "xs == toList (fromList xs)" $
>         \(xs :: [a]) ->
>           let as = fromList xs :: FingerTree m a
>           in claimEqual (xs) (toList as)
> 
>     , testKreb
>         "toList mempty == mempty" $
>         claimEqual
>           (mempty :: [a])
>           (toList (mempty :: FingerTree m a))
> 
>     , testKreb
>         "toList (as <> bs) == toList as ++ toList bs" $
>         \(as :: FingerTree m a) (bs :: FingerTree m a) ->
>           claimEqual
>             (toList (as <> bs))
>             (toList as ++ toList bs)
> 
>     , testKreb
>         "fromList mempty == mempty" $
>         claimEqual
>           (mempty :: FingerTree m a)
>           (fromList mempty)
> 
>     , testKreb
>         "fromList (xs ++ ys) == fromList xs <> fromList ys" $
>         \(xs :: [a]) (ys :: [a]) ->
>           claimEqual
>             (fromList (xs ++ ys) :: FingerTree m a)
>             (fromList xs <> fromList ys)
> 
>     , testKreb
>         "value as == foldMap value (toList as)" $
>         \(as :: FingerTree m a) ->
>           claimEqual
>             (value as)
>             (foldMap (value :: a -> m) (toList as))
> 
>     , testKreb
>         "as == reverse (reverse as)" $
>         \(as :: FingerTree m a) ->
>           claimEqual
>             (as)
>             (reverse (reverse as))
> 
>     , testKreb
>         "reverse mempty == mempty" $
>         claimEqual
>           (mempty :: FingerTree m a)
>           (reverse mempty)
> 
>     , testKreb
>         "reverse (as <> bs) == reverse bs <> reverse as" $
>         \(as :: FingerTree m a) (bs :: FingerTree m a) ->
>           claimEqual
>             (reverse (as <> bs))
>             ((reverse bs) <> (reverse as))
> 
>     , testKreb
>         "toList (cons a as) == a : (toList x)" $
>         \(a :: a) (as :: FingerTree m a) ->
>           claimEqual
>             (toList (cons a as))
>             (a : toList as)
> 
>     , testKreb
>         "toList (snoc a as) == (toList as) ++ [a]" $
>         \(a :: a) (as :: FingerTree m a) ->
>           claimEqual
>             (toList (snoc a as))
>             ((toList as) ++ [a])
> 
>     , testKreb
>         "fromList (a : as) == cons a (fromList as)" $
>         \(a :: a) (as :: [a]) ->
>           claimEqual
>             (fromList (a : as) :: FingerTree m a)
>             (cons a (fromList as))
> 
>     , testKreb
>         "fromList (as ++ [a]) == snoc a (fromList as)" $
>         \(a :: a) (as :: [a]) ->
>           claimEqual
>             (fromList (as ++ [a]) :: FingerTree m a)
>             (snoc a (fromList as))
> 
>     , testKreb
>         "reverse (cons a as) == snoc a (reverse as)" $
>         \(a :: a) (as :: FingerTree m a) ->
>           claimEqual
>             (reverse (cons a as))
>             (snoc a (reverse as))
> 
>     , testKreb
>         "reverse (snoc a as) == cons a (reverse as)" $
>         \(a :: a) (as :: FingerTree m a) ->
>           claimEqual
>             (reverse (snoc a as))
>             (cons a (reverse as))
> 
>     , testKreb
>         "splitting property: concat" $
>         \(p :: Fun m Bool) (as :: FingerTree m a) ->
>           claimAny
>             [ claimTrue (apFun p mempty)
>             , claimFalse (apFun p (value as))
>             , case split (apFun p) as of
>                 Nothing -> reject "expected successful split"
>                 Just (us, x, vs) -> claimEqual as (us <> cons x vs)
>             ]
> 
>     , testKreb
>         "splitting property: prefix" $
>         \(p :: Fun m Bool) (as :: FingerTree m a) ->
>           claimAny
>             [ claimTrue (apFun p mempty)
>             , claimFalse (apFun p (value as))
>             , case split (apFun p) as of
>                 Nothing -> reject "expected successful split"
>                 Just (us, _, _) -> claimFalse (apFun p (value us))
>             ]
> 
>     , testKreb
>         "splitting property: found" $
>         \(p :: Fun m Bool) (as :: FingerTree m a) ->
>           claimAny
>             [ claimTrue (apFun p mempty)
>             , claimFalse (apFun p (value as))
>             , case split (apFun p) as of
>                 Nothing -> reject "expected successful split"
>                 Just (us, x, _) -> claimTrue (apFun p (value us <> value x))
>             ]
> 
>     , testKreb
>         "fmapFT id as == as" $
>         \(as :: FingerTree m a) ->
>           claimEqual
>             (as)
>             (fmapFT id as)
> 
>     , testKreb
>         "fmapFT (g . f) as == fmapFT g (fmap f as)" $
>         \(as :: FingerTree m a) (f :: Fun a a) (g :: Fun a a) ->
>           claimEqual
>             (fmapFT ((apFun g) . (apFun f)) as :: FingerTree m a)
>             (fmapFT (apFun g) (fmapFT (apFun f) as :: FingerTree m a))
> 
>     , testKreb
>         "inflateWith id (fmapFT (:[]) as) == as" $
>         \(as :: FingerTree m a) ->
>           claimEqual
>             (as)
>             (inflateWith id (fmapFT (:[]) as :: FingerTree m [a]))
> 
>     , testKreb
>         "inflateWith id (singleton xs) == fromList xs" $
>         \(xs :: [a]) ->
>           let as = singleton xs :: FingerTree m [a]
>           in claimEqual
>             (inflateWith id as :: FingerTree m a)
>             (fromList xs)
> 
>     , testKreb
>         "(isEmpty as) || (readInit (as <> bs) == readInit as)" $
>         \(as :: FingerTree m a) (bs :: FingerTree m a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (readInit as)
>                 (readInit (as <> bs))
>             ]
> 
>     , testKreb
>         "(isEmpty bs) || (readLast (as <> bs) == readLast bs)" $
>         \(as :: FingerTree m a) (bs :: FingerTree m a) ->
>           claimAny
>             [ claimTrue (isEmpty bs)
>             , claimEqual
>                 (readLast bs)
>                 (readLast (as <> bs))
>             ]
> 
>     , testKreb
>         "readInit (mempty <> bs) == readInit bs" $
>         \(bs :: FingerTree m a) ->
>           claimEqual
>             (readInit bs)
>             (readInit (mempty <> bs))
> 
>     , testKreb
>         "readLast (as <> mempty) == readLast as" $
>         \(as :: FingerTree m a) ->
>           claimEqual
>             (readLast as)
>             (readLast (as <> mempty))
>     ]
