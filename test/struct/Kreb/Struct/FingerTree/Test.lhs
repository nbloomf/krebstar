---
title: Kreb.Struct.FingerTree.Test
---



Contents
--------

* [Introduction](#introduction)
    * [A note on strategy](#a-note-on-strategy)
* [Generators](#generators)
* [Properties](#properties)
    * [Count is a Monoid](#count-is-a-monoid)
    * [Equality on finger trees is an equivalence](#equality-on-finger-trees-is-an-equivalence)
    * [Finger trees and lists are interconvertible](#finger-trees-and-lists-are-interconvertible)
    * [Functor laws for fmapFT](#functor-laws-for-fmapft)
    * [Fold is lawful on finger trees](#fold-is-lawful-on-finger-trees)
    * [Cons and uncons are mutual inverses](#cons-and-uncons-are-mutual-inverses)
    * [Finger trees form a monoid](#finger-trees-form-a-monoid)
    * [toList is a monoid homomorphism](#tolist-is-a-monoid-homomorphism)
    * [fromList is a monoid homomorphism](#fromlist-is-a-monoid-homomorphism)
    * [Reverse is a monoid involution](#reverse-is-a-monoid-involution)
    * [Leaf is cons and snoc](#leaf-is-cons-and-snoc)
    * [Splitting properties](#splitting-properties)
    * [Break properties](#break-properties)
* [Test Suite](#test-suite)
* [Test Helpers](#test-helpers)



Introduction
============

In [Ned.Data.FingerTree](src/Ned/Data/FingerTree.html) we developed a module of code for creating and manipulating finger trees. Now to test it.

Throughout this project we'll be using the QuickCheck library to write and execute generative property tests. These differ from traditional unit tests in that rather than specifying a prescribed list of input/output/effect triples for a program, we specify boolean _properties_ that the program should satisfy and bombard it with randomly generated data looking for a counterexample. If you've never used these before, you're in for a treat; generative property testing is shockingly effective at rooting out bugs and improving confidence in our code.

> {-# LANGUAGE
>     MultiParamTypeClasses
>   , ScopedTypeVariables
> #-}
> 
> module Kreb.Struct.FingerTree.Test (
>     test_FingerTree
> ) where
> 
> import Data.Proxy
> import Data.Foldable
> 
> import Test.Tasty
> 
> import Kreb.Check hiding (ZZ(..))
> import Kreb.Struct.FingerTree





Dummy Types
-----------

> data ZZ
>   = ZZ Int
>   deriving (Eq, Show)
> 
> pZZ :: Proxy ZZ
> pZZ = Proxy
> 
> instance Semigroup ZZ where
>   (ZZ a) <> (ZZ b) = ZZ (a + b)
> 
> instance Monoid ZZ where
>   mempty = ZZ 0
> 
> instance Valued Count ZZ where
>   value _ = Count 1
> 
> instance Arb ZZ where
>   arb = ZZ <$> arb
> 
> instance CoArb ZZ where
>   coarb (ZZ k) = coarb k
> 
> instance MakeTo ZZ where
>   makeTo = makeToIntegralWith g h
>     where
>       g :: ZZ -> Integer
>       g (ZZ k) = fromIntegral k
> 
>       h :: Integer -> ZZ
>       h k = ZZ $ fromInteger $ abs k

> data Tup
>   = Tup Int Int
>   deriving (Eq, Show)
> 
> pTup :: Proxy Tup
> pTup = Proxy
> 
> instance Semigroup Tup where
>   (Tup a1 b1) <> (Tup a2 b2) =
>     if a2 > 0
>       then Tup (a1+a2) b2
>       else Tup a1 (b1+b2)
> 
> instance Monoid Tup where
>   mempty = Tup 0 0
> 
> instance Valued Tup Bool where
>   value p = if p
>     then Tup 0 1
>     else Tup 1 0
> 
> instance Valued Tup ZZ where
>   value (ZZ k) = if 0 == rem k 2
>     then Tup 0 1
>     else Tup 1 0
> 
> instance Arb Tup where
>   arb = do
>     NonNegative a <- arb
>     NonNegative b <- arb
>     return $ Tup a b
> 
> instance CoArb Tup where
>   coarb (Tup a b) =
>     coarb (a,b)
> 
> instance MakeTo Tup where
>   makeTo = makeToExtendWith makeTo g h
>     where
>       g :: Tup -> (Int, Int)
>       g (Tup a b) = (a,b)
> 
>       h :: (Int, Int) -> Tup
>       h (a,b) = Tup a b



Generators
==========

> pCount :: Proxy Count
> pCount = Proxy
> 
> pChar :: Proxy Char
> pChar = Proxy
> 
> pBool :: Proxy Bool
> pBool = Proxy



Properties
==========

Count is a Monoid
-----------------

Recall that a _monoid_ is a type with an associative binary operation and a neutral element. Algebraic structures like this, and lawful class instances in general, are tailor-made for property testing, which we can demonstrate on the `Count` type. This particular example looks a little superfluous -- surely we don't need to test the monoid laws for such a simple type. But this example will be a template for tests on more complicated types. More generally it looks like overkill to have so much infrastructure for testing this one module, but the idea is that the structure of these tests will (hopefully) scale nicely to the thornier code.

> test_Count_Monoid :: TestTree
> test_Count_Monoid =
>   testGroup "Count is a monoid"
>     [ test_Semigroup_laws (Proxy :: Proxy Count)
>     , test_Monoid_laws (Proxy :: Proxy Count)
>     ]



Equality on finger trees is an equivalence
------------------------------------------

Since we rolled our own `Eq` instance for finger trees, it makes sense to check that it really is an equivalence relation

> test_FingerTree_Eq :: TestTree
> test_FingerTree_Eq =
>   testGroup "Eq laws for FingerTree"
>     [ test_Eq_laws (Proxy :: Proxy (FingerTree Count Char))
>     , test_Eq_laws (Proxy :: Proxy (FingerTree Tup Bool))
>     ]



Finger trees form a monoid
--------------------------

Concatenation and mempty form a monoid structure on finger trees.

> test_FingerTree_Monoid :: TestTree
> test_FingerTree_Monoid =
>   testGroup "Monoid laws for concat/mempty"
>     [ test_Semigroup_laws (Proxy :: Proxy (FingerTree Count Char))
>     , test_Semigroup_laws (Proxy :: Proxy (FingerTree Tup Bool))
> 
>     , test_Monoid_laws (Proxy :: Proxy (FingerTree Count Char))
>     , test_Monoid_laws (Proxy :: Proxy (FingerTree Tup Bool))
>     ]






Test Suite
==========

> test_FingerTree :: TestTree
> test_FingerTree =
>   testGroup "FingerTree"
>     [ test_Count_Monoid
>     , test_FingerTree_Eq
>     , test_FingerTree_Monoid
>   --  , test_FingerTree_List_convert
>   --  , test_FingerTree_fmapFT
>   --  , test_FingerTree_Foldable
>   --  , test_FingerTree_cons_snoc_inverses
>   --  , test_FingerTree_toList_homomorphism
>   --  , test_FingerTree_fromList_homomorphism
>   --  , test_FingerTree_reverse
>   --  , test_FingerTree_leaf
>   --  , test_FingerTree_split
>   --  , test_FingerTree_break
>     ]








> {-

Finger trees and lists are interconvertible
-------------------------------------------

We have two functions, `toList` (from the `Foldable` class) and `fromListFT`, which convert between finger trees and lists. These should be mutual inverses.

> test_FingerTree_List_convert
>   :: TestTree
> test_FingerTree_List_convert =
>   testGroup "FingerTree and List are interconvertible"
>     [ test_FingerTree_toList_fromList
>     , test_FingerTree_fromList_toList
>     ]

Converting a list to a finger tree and back is the identity:

> prop_FingerTree_toList_fromList
>    , cprop_FingerTree_toList_fromList
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> [a] -> Property
> 
> prop_FingerTree_toList_fromList _ _ xs =
>   let t = fromListFT xs :: FingerTree m a in
>   property $ toList t == xs
> 
> cprop_FingerTree_toList_fromList pa pm xs =
>   cover 50 (length xs > 30) "length xs > 50" $
>   cover 90 (length xs > 0) "length xs > 0" $
>   prop_FingerTree_toList_fromList pa pm xs
> 
> test_FingerTree_toList_fromList :: TestTree
> test_FingerTree_toList_fromList =
>   testGroup "toList . fromList == id (Char, Count)"
>     [ testProperty "Char/Count" $
>         cprop_FingerTree_toList_fromList pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTree_toList_fromList pBool pTup
> 
>     , testCases (prop_FingerTree_toList_fromList pChar pCount)
>       [ ("\"\"",    [])
>       , ("\"abc\"", ['a', 'b', 'c'])
>       ]
>     ]

Converting a finger tree to a list and back is the identity:

> prop_FingerTree_fromList_toList
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTree m a -> Bool
> prop_FingerTree_fromList_toList _ _ xs =
>   ((fromListFT $ toList xs) == xs)
> 
> cprop_FingerTree_fromList_toList
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTree m a -> Property
> cprop_FingerTree_fromList_toList pa pm xs =
>   cover 90 (notEmptyFT xs) "notEmptyFT xs" $
>   cover 30 (depthFT xs > 3) "depthFT xs > 3" $
>   prop_FingerTree_fromList_toList pa pm xs
> 
> test_FingerTree_fromList_toList :: TestTree
> test_FingerTree_fromList_toList =
>   testGroup "fromList . toList == id (Char, Count)"
>     [ testProperty "Char/Count" $
>         cprop_FingerTree_fromList_toList pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTree_fromList_toList pBool pTup
> 
>     , testCases (prop_FingerTree_fromList_toList pChar pCount)
>       [ ( "mempty"
>         , mempty
>         )
> 
>       , ( "'a':'b':'c':mempty"
>         , cons 'a' (cons 'b' (cons 'c' mempty))
>         )
>       ]
>     ]



Functor laws for fmapFT
-----------------------

Recall that `FingerTree m` can't be a bona fide instance of the functor class due to the weirdness of the `Valued` constraint; essentially the data type held by the tree is crucially related to `m`, so the type signature of the real `fmap` doesn't work out. But we do have a function that behaves like fmap.

> test_FingerTree_fmapFT :: TestTree
> test_FingerTree_fmapFT =
>   testGroup "Functor Laws for FingerTree"
>     [ test_FingerTree_fmapFT_identity
>     , localOption (QuickCheckTests 500) $
>         test_FingerTree_fmapFT_composite
>     ]

`fmapFT` preserves the identity:

> prop_FingerTree_fmapFT_identity
>    , cprop_FingerTree_fmapFT_identity
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTree m a
>   -> Property
> 
> prop_FingerTree_fmapFT_identity _ _ xs =
>   property $
>     xs == (fmapFT id xs)
> 
> cprop_FingerTree_fmapFT_identity pa pm xs =
>   cover 90 (notEmptyFT xs) "notEmptyFT xs" $
>   cover 30 (depthFT xs > 3) "depthFT xs > 3" $
>   prop_FingerTree_fmapFT_identity pa pm xs
> 
> test_FingerTree_fmapFT_identity :: TestTree
> test_FingerTree_fmapFT_identity =
>   testGroup "fmapFT id == id"
>     [ testProperty "Char/Count" $
>         cprop_FingerTree_fmapFT_identity pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTree_fmapFT_identity pBool pTup
> 
>     , testCases
>       (prop_FingerTree_fmapFT_identity pChar pCount)
>       [ ( "mempty", mempty )
>       , ( "leaf",   leaf 'a' )
>       ]
>     ]

And `fmapFT` preserves composites:

> prop_FingerTree_fmapFT_composite
>    , cprop_FingerTree_fmapFT_composite
>   :: forall a1 m1 a2 m2 a3 m3
>    . ( Valued m1 a1, Valued m2 a2, Valued m3 a3, Eq a3 )
>   => Proxy a1 -> Proxy m1 -> Proxy a2
>   -> Proxy m2 -> Proxy a3 -> Proxy m3
>   -> Fun a1 a2 -> Fun a2 a3 -> FingerTree m1 a1
>   -> Property
> 
> prop_FingerTree_fmapFT_composite _ _ _ _ _ _ f g xs =
>   property $
>     ((fmapFT ((applyFun g) . (applyFun f)) xs) :: FingerTree m3 a3)
>       == (fmapFT (applyFun g)
>           (fmapFT (applyFun f) xs :: FingerTree m2 a2))
> 
> cprop_FingerTree_fmapFT_composite pa pm pb pn pc po f g xs =
>   cover 90 (notEmptyFT xs) "notEmptyFT xs" $
>   cover 30 (depthFT xs > 3) "depthFT xs > 3" $
>   prop_FingerTree_fmapFT_composite pa pm pb pn pc po f g xs
> 
> test_FingerTree_fmapFT_composite :: TestTree
> test_FingerTree_fmapFT_composite =
>   testGroup "fmapFT (f . g) == fmapFT f . fmapFT g"
>     [ testProperty "Char/Count" $
>         cprop_FingerTree_fmapFT_composite
>           pChar pCount pChar pCount pChar pCount
> 
>     , testProperty "Bool/Tup" $
>         cprop_FingerTree_fmapFT_composite
>           pBool pTup pBool pTup pBool pTup
>     ]



Fold is lawful on fingertrees
-----------------------------

We get a lot of mileage out of the `Foldable` instance for `FingerTree`, so it makes sense to explicitly check that the laws for this class are not not satisifed.

> test_FingerTree_Foldable :: TestTree
> test_FingerTree_Foldable =
>   testGroup "Foldable laws for FingerTree"
>     [ test_FingerTree_foldMap_id
>     , localOption (QuickCheckTests 1000)
>         $ test_FingerTree_foldMap_factor
>     , localOption (QuickCheckTests 1000)
>         $ test_FingerTree_foldMap_compose
>     , test_FingerTree_fold_value
>     ]

`fold` is a special case of `foldMap`:

> prop_FingerTree_foldMap_id
>    , cprop_FingerTree_foldMap_id
>   :: forall a m
>    . ( Eq a, Valued m a, Monoid a )
>   => Proxy a -> Proxy m
>   -> FingerTree m a
>   -> Property
> 
> prop_FingerTree_foldMap_id _ _ xs =
>   property $ fold xs == foldMap id xs
> 
> cprop_FingerTree_foldMap_id pa pm xs =
>   cover 70 (notEmptyFT xs) "xs nonempty" $
>   cover 30 (depthFT xs > 2) "depth xs > 2" $
>   prop_FingerTree_foldMap_id pa pm xs
> 
> test_FingerTree_foldMap_id :: TestTree
> test_FingerTree_foldMap_id =
>   testGroup "fold == foldMap id"
>     [ testProperty "ZZ/Count" $
>         cprop_FingerTree_foldMap_id pZZ pCount
> 
>     , testProperty "ZZ/Tup" $
>         cprop_FingerTree_foldMap_id pZZ pTup
>     ]

`foldMap f` factors as `fold . fmap f`:

> prop_FingerTree_foldMap_factor
>    , cprop_FingerTree_foldMap_factor
>   :: forall a b m
>    . ( Eq b, Valued m a, Monoid b, Valued m b )
>   => Proxy a -> Proxy b -> Proxy m
>   -> Fun a b -> FingerTree m a
>   -> Property
> 
> prop_FingerTree_foldMap_factor _ _ _ f xs =
>   property $
>     (foldMap (applyFun f) xs)
>       == (fold (fmapFT (applyFun f) xs :: FingerTree m b))
> 
> cprop_FingerTree_foldMap_factor pa pb pm f xs =
>   cover 70 (notEmptyFT xs) "xs nonempty" $
>   cover 30 (depthFT xs > 2) "depth xs > 2" $
>   prop_FingerTree_foldMap_factor pa pb pm f xs
> 
> test_FingerTree_foldMap_factor :: TestTree
> test_FingerTree_foldMap_factor =
>   testGroup "foldMap f = fold . fmap f"
>     [ testProperty "ZZ/Count" $
>         cprop_FingerTree_foldMap_factor pZZ pZZ pCount
> 
>     , testProperty "ZZ/Tup" $
>         cprop_FingerTree_foldMap_factor pZZ pZZ pTup
>     ]

`foldMap` distributes over composition:

> prop_FingerTree_foldMap_compose
>    , cprop_FingerTree_foldMap_compose
>   :: forall a b c m
>    . ( Eq b, Valued m a, Monoid b, Valued m b
>      , Valued m c, Monoid c, Eq c )
>   => Proxy a -> Proxy b -> Proxy c -> Proxy m
>   -> Fun a b -> Fun b c -> FingerTree m a
>   -> Property
> 
> prop_FingerTree_foldMap_compose _ _ _ _ f g xs =
>   property $
>     (foldMap (applyFun g . applyFun f) xs)
>       == (foldMap (applyFun g)
>           ((fmapFT (applyFun f) xs) :: FingerTree m b))
> 
> cprop_FingerTree_foldMap_compose pa pb pc pm f g xs =
>   cover 70 (notEmptyFT xs) "xs nonempty" $
>   cover 30 (depthFT xs > 2) "depth xs > 2" $
>   prop_FingerTree_foldMap_compose pa pb pc pm f g xs
> 
> test_FingerTree_foldMap_compose :: TestTree
> test_FingerTree_foldMap_compose =
>   testGroup "foldMap (g . f) == foldMap g . fmap f"
>     [ testProperty "ZZ/Count" $
>         cprop_FingerTree_foldMap_compose pZZ pZZ pZZ pCount
> 
>     , testProperty "ZZ/Tup" $
>         cprop_FingerTree_foldMap_compose pZZ pZZ pZZ pTup
>     ]

`fold` is compatible with `value`:

> prop_FingerTree_fold_value
>    , cprop_FingerTree_fold_value
>   :: forall a m
>    . ( Eq a, Valued m a, Eq m )
>   => Proxy a -> Proxy m
>   -> FingerTree m a
>   -> Property
> 
> prop_FingerTree_fold_value _ _ xs =
>   property $
>     (value xs :: m) == fold (map value $ toList xs)
> 
> cprop_FingerTree_fold_value pa pm xs =
>   cover 70 (notEmptyFT xs) "xs nonempty" $
>   cover 30 (depthFT xs > 2) "depth xs > 2" $
>   prop_FingerTree_fold_value pa pm xs
> 
> test_FingerTree_fold_value :: TestTree
> test_FingerTree_fold_value =
>   testGroup "value == fold . fmapFT value"
>     [ testProperty "Char/Count" $
>         cprop_FingerTree_fold_value pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTree_fold_value pBool pTup
> 
>     , testCases
>       (prop_FingerTree_fold_value pChar pCount)
>       [ ( "mempty", mempty )
>       ]
>     ]



Cons and uncons are mutual inverses
-----------------------------------

The `cons` and `uncons` functions are inverses of each other (sort of); likewise `snoc` and `unsnoc`.

> test_FingerTree_cons_snoc_inverses :: TestTree
> test_FingerTree_cons_snoc_inverses =
>   testGroup "cons/snoc and uncons/unsnoc are mutual inverses"
>     [ test_FingerTree_cons_uncons
>     , test_FingerTree_uncons_cons
>     , test_FingerTree_snoc_unsnoc
>     , test_FingerTree_unsnoc_snoc
>     , test_FingerTree_uncons_mempty
>     , test_FingerTree_unsnoc_mempty
>     ]

`uncons` followed by `cons`:

> prop_FingerTree_cons_uncons
>    , cprop_FingerTree_cons_uncons
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTree m a
>   -> Property
> 
> prop_FingerTree_cons_uncons _ _ xs =
>   property $
>     case uncons xs of
>       Nothing -> True
>       Just (a,zs) -> xs == cons a zs
> 
> cprop_FingerTree_cons_uncons pa pm xs =
>   cover 50 (notEmptyFT xs) "xs nonempty" $
>   checkOneDepth xs $
>   prop_FingerTree_cons_uncons pa pm xs
> 
> test_FingerTree_cons_uncons :: TestTree
> test_FingerTree_cons_uncons =
>   testGroup "cons . uncons == id"
>     [ testProperty "Char/Count" $
>         cprop_FingerTree_cons_uncons pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTree_cons_uncons pBool pTup
> 
>     , testCases
>       (prop_FingerTree_cons_uncons pChar pCount)
>       [ ( "mempty", mempty )
>       , ( "leaf",   leaf 'a' )
>       ]
>     ]

`cons` followed by `uncons`:

> prop_FingerTree_uncons_cons
>    , cprop_FingerTree_uncons_cons
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> FingerTree m a
>   -> Property
> 
> prop_FingerTree_uncons_cons _ _ a xs =
>   property $
>     case uncons (cons a xs) of
>       Nothing -> False
>       Just (b,ys) -> (a == b) && (xs == ys)
> 
> cprop_FingerTree_uncons_cons pa pm a xs =
>   cover 50 (notEmptyFT xs) "xs nonempty" $
>   checkOneDepth xs $
>   prop_FingerTree_uncons_cons pa pm a xs
> 
> test_FingerTree_uncons_cons :: TestTree
> test_FingerTree_uncons_cons =
>   testGroup "uncons . cons == id"
>     [ testProperty "Char/Count" $
>         cprop_FingerTree_uncons_cons pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTree_uncons_cons pBool pTup
> 
>     , testCases
>       (uncurry (prop_FingerTree_uncons_cons pChar pCount))
>       [ ( "empty", ( 'a', mempty ) )
>       , ( "leaf",  ( 'a', leaf 'b' ) )
>       ]
>     ]

`unsnoc` followed by `snoc`:

> prop_FingerTree_snoc_unsnoc
>    , cprop_FingerTree_snoc_unsnoc
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTree m a -> Property
> 
> prop_FingerTree_snoc_unsnoc _ _ xs =
>   property $
>     case unsnoc xs of
>       Nothing -> True
>       Just (a,zs) -> xs == snoc a zs
> 
> cprop_FingerTree_snoc_unsnoc pa pm xs =
>   cover 50 (notEmptyFT xs) "xs nonempty" $
>   checkOneDepth xs $
>   prop_FingerTree_snoc_unsnoc pa pm xs
> 
> test_FingerTree_snoc_unsnoc :: TestTree
> test_FingerTree_snoc_unsnoc =
>   testGroup "snoc . unsnoc == id"
>     [ testProperty "Char/Count" $
>         cprop_FingerTree_snoc_unsnoc pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTree_snoc_unsnoc pBool pTup
> 
>     , testCases
>       (prop_FingerTree_snoc_unsnoc pChar pCount)
>       [ ( "mempty", mempty )
>       , ( "leaf",   leaf 'a' )
>       ]
>     ]

`snoc` followed by `unsnoc`:

> prop_FingerTree_unsnoc_snoc
>    , cprop_FingerTree_unsnoc_snoc
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> FingerTree m a
>   -> Property
> 
> prop_FingerTree_unsnoc_snoc _ _ a xs =
>   property $
>     case unsnoc (snoc a xs) of
>       Nothing -> False
>       Just (b,ys) -> (a == b) && (xs == ys)
> 
> cprop_FingerTree_unsnoc_snoc pa pm a xs =
>   cover 50 (notEmptyFT xs) "xs nonempty" $
>   checkOneDepth xs $
>   prop_FingerTree_unsnoc_snoc pa pm a xs
> 
> test_FingerTree_unsnoc_snoc :: TestTree
> test_FingerTree_unsnoc_snoc =
>   testGroup "unsnoc . snoc == id"
>     [ testProperty "Char/Count" $
>         cprop_FingerTree_unsnoc_snoc pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTree_unsnoc_snoc pBool pTup
> 
>     , testCases
>       (uncurry $ prop_FingerTree_unsnoc_snoc pChar pCount)
>       [ ( "empty", ( 'a', mempty ) )
>       , ( "leaf",  ( 'a', leaf 'b' ) )
>       ]
>     ]

`uncons` is only `Nothing` on empty inputs:

> prop_FingerTree_uncons_mempty
>    , cprop_FingerTree_uncons_mempty
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTree m a
>   -> Property
> 
> prop_FingerTree_uncons_mempty _ _ xs =
>   property $
>     (Nothing == uncons xs) == (xs == mempty)
> 
> cprop_FingerTree_uncons_mempty pa pm xs =
>   cover 50 (notEmptyFT xs) "xs nonempty" $
>   cover 2 (isEmptyFT xs) "xs empty" $
>   checkOneDepth xs $
>   prop_FingerTree_uncons_mempty pa pm xs
> 
> test_FingerTree_uncons_mempty :: TestTree
> test_FingerTree_uncons_mempty =
>   testGroup "uncons == Nothing"
>     [ testProperty "Char/Count" $
>         cprop_FingerTree_uncons_mempty pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTree_uncons_mempty pBool pTup
> 
>     , testCases
>       (prop_FingerTree_uncons_mempty pChar pCount)
>       [ ( "mempty", mempty )
>       , ( "leaf",   leaf 'a' )
>       ]
>     ]

`unsnoc` is only `Nothing` on empty inputs:

> prop_FingerTree_unsnoc_mempty
>    , cprop_FingerTree_unsnoc_mempty
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTree m a
>   -> Property
> 
> prop_FingerTree_unsnoc_mempty _ _ xs =
>   property $
>     (Nothing == unsnoc xs) == (xs == mempty)
> 
> cprop_FingerTree_unsnoc_mempty pa pm xs =
>   cover 50 (notEmptyFT xs) "xs nonempty" $
>   cover 2 (isEmptyFT xs) "xs empty" $
>   checkOneDepth xs $
>   prop_FingerTree_unsnoc_mempty pa pm xs
> 
> test_FingerTree_unsnoc_mempty :: TestTree
> test_FingerTree_unsnoc_mempty =
>   testGroup "unsnoc == Nothing"
>     [ testProperty "Char/Count" $
>         cprop_FingerTree_unsnoc_mempty pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTree_unsnoc_mempty pBool pTup
> 
>     , testCases
>       (prop_FingerTree_unsnoc_mempty pChar pCount)
>       [ ( "mempty", mempty )
>       , ( "leaf",   leaf 'a' )
>       ]
>     ]



toList is a monoid homomorphism
-------------------------------

We have two monoids, lists and finger trees, and a function `toList` between them. When this happens it's natural to ask if the function preserves the monoid structure -- and indeed it does.

> test_FingerTree_toList_homomorphism :: TestTree
> test_FingerTree_toList_homomorphism =
>   testGroup "toList is a monoid hom"
>     [ test_FingerTree_toList_hom_identity
>     , test_FingerTree_toList_hom_product
>     , test_FingerTree_toList_hom_cons
>     , test_FingerTree_toList_hom_snoc
>     ]

`toList` preserves the identity:

> prop_FingerTree_toList_hom_identity
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> Property
> prop_FingerTree_toList_hom_identity _ _ =
>   let x = mempty :: FingerTree m a in
>   property $ (toList x) == []
> 
> test_FingerTree_toList_hom_identity :: TestTree
> test_FingerTree_toList_hom_identity =
>   testGroup "toList mempty == mempty"
>     [ testProperty "Char/Count" $
>         prop_FingerTree_toList_hom_identity pChar pCount
> 
>     , testProperty "Bool/Tup" $
>         prop_FingerTree_toList_hom_identity pBool pTup
>     ]

`toList` preserves products:

> prop_FingerTree_toList_hom_product
>    , cprop_FingerTree_toList_hom_product
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTree m a -> FingerTree m a
>   -> Property
> 
> prop_FingerTree_toList_hom_product _ _ xs ys =
>   property $
>     (toList $ xs <> ys)
>       == (toList xs ++ toList ys)
> 
> cprop_FingerTree_toList_hom_product pa pm xs ys =
>   cover 50 ((notEmptyFT xs) && (notEmptyFT ys))
>     "xs, ys nonempty" $
>   checkPairDepth xs ys $
>   prop_FingerTree_toList_hom_product pa pm xs ys
> 
> test_FingerTree_toList_hom_product :: TestTree
> test_FingerTree_toList_hom_product =
>   testGroup "toList (a <> b) == toList a <> toList b"
>     [ testProperty "Char/Count" $
>         cprop_FingerTree_toList_hom_product pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTree_toList_hom_product pBool pTup
> 
>     , testCases
>       (uncurry $ prop_FingerTree_toList_hom_product pChar pCount)
>       [ ( "both empty", (mempty, mempty) )
>       ]
>     ]

In fact both lists and finger trees are a bit more than just monoids -- they are monoids being acted on by a set from the left and the right. This is `cons` and `snoc`. We can verify that `toList` preserves these actions as well. First for `cons`:

> prop_FingerTree_toList_hom_cons
>    , cprop_FingerTree_toList_hom_cons
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> FingerTree m a -> Property
> 
> prop_FingerTree_toList_hom_cons _ _ a xs =
>   property $
>     (toList $ cons a xs) == (a : toList xs)
> 
> cprop_FingerTree_toList_hom_cons pa pm a xs =
>   checkOneDepth xs $
>   prop_FingerTree_toList_hom_cons pa pm a xs
> 
> test_FingerTree_toList_hom_cons :: TestTree
> test_FingerTree_toList_hom_cons =
>   testGroup "toList preserves cons"
>     [ testProperty "Char/Count" $
>         cprop_FingerTree_toList_hom_cons pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTree_toList_hom_cons pBool pTup
> 
>     , testCases
>       (uncurry $ prop_FingerTree_toList_hom_cons pChar pCount)
>       [ ( "mempty", ( 'a', mempty ) )
>       , ( "leaf",   ( 'a', leaf 'b' ) )
>       ]
>     ]

And then for `snoc`:

> prop_FingerTree_toList_hom_snoc
>    , cprop_FingerTree_toList_hom_snoc
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> FingerTree m a -> Property

> prop_FingerTree_toList_hom_snoc _ _ a xs =
>   property $
>     (toList $ snoc a xs) == (toList xs ++ [a])
> 
> cprop_FingerTree_toList_hom_snoc pa pm a xs =
>   checkOneDepth xs $
>   prop_FingerTree_toList_hom_snoc pa pm a xs
> 
> test_FingerTree_toList_hom_snoc :: TestTree
> test_FingerTree_toList_hom_snoc =
>   testGroup "toList preserves snoc"
>     [ testProperty "Char/Count" $
>         cprop_FingerTree_toList_hom_snoc pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTree_toList_hom_snoc pBool pTup
> 
>     , testCases
>       (uncurry $ prop_FingerTree_toList_hom_snoc pChar pCount)
>       [ ( "mempty", ( 'a', mempty ) )
>       , ( "leaf",   ( 'a', leaf 'b' ) )
>       ]
>     ]



fromList is a monoid homomorphism
---------------------------------

Like `toList`, `fromList` is also a monoid (and set action) homomorphism.

> test_FingerTree_fromList_homomorphism :: TestTree
> test_FingerTree_fromList_homomorphism =
>   testGroup "fromList is a monoid hom"
>     [ test_FingerTree_fromList_hom_identity
>     , test_FingerTree_fromList_hom_product
>     , test_FingerTree_fromList_hom_cons
>     , test_FingerTree_fromList_hom_snoc
>     ]

`fromList` preserves the identity:

> prop_FingerTree_fromList_hom_identity
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> Property
> 
> prop_FingerTree_fromList_hom_identity _ _ =
>   property $
>     let x = fromListFT [] :: FingerTree m a in
>     x == mempty
> 
> test_FingerTree_fromList_hom_identity :: TestTree
> test_FingerTree_fromList_hom_identity =
>   testGroup "fromList mempty == mempty"
>     [ testProperty "Char/Count" $
>         prop_FingerTree_fromList_hom_identity pChar pCount
>     , testProperty "Bool/Tup" $
>         prop_FingerTree_fromList_hom_identity pBool pTup
>     ]

And it preserves products:

> prop_FingerTree_fromList_hom_product
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> [a] -> [a]
>   -> Property
> 
> prop_FingerTree_fromList_hom_product _ _ xs ys =
>   property $
>     let zs = fromListFT (xs ++ ys) :: FingerTree m a in
>     zs == (fromListFT xs) <> (fromListFT ys)
> 
> cprop_FingerTree_fromList_hom_product pa pm xs ys =
>   cover 20 ((length xs > 5) && (length ys > 5)) "length xs,ys > 5" $
>   cover 1 ((xs == []) && (ys == [])) "both nonempty" $
>   prop_FingerTree_fromList_hom_product pa pm xs ys
> 
> test_FingerTree_fromList_hom_product :: TestTree
> test_FingerTree_fromList_hom_product =
>   testGroup "fromList (xs <> ys) == fromList xs <> fromList ys"
>     [ testProperty "Char/Count" $
>         cprop_FingerTree_fromList_hom_product pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTree_fromList_hom_product pBool pTup
> 
>     , testCases
>       (uncurry $ prop_FingerTree_fromList_hom_product pChar pCount)
>       [ ( "empty",     ([], []) )
>       , ( "singleton", (['a'], ['b']) )
>       ]
>     ]

`fromList` preserves `cons`:

> prop_FingerTree_fromList_hom_cons
>    , cprop_FingerTree_fromList_hom_cons
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> [a]
>   -> Property
> 
> prop_FingerTree_fromList_hom_cons _ _ a xs =
>   property $
>     let zs = fromListFT xs :: FingerTree m a in
>     (fromListFT $ a : xs) == (cons a zs)
> 
> cprop_FingerTree_fromList_hom_cons pa pm a xs =
>   cover 40 (length xs > 5) "length xs > 5" $
>   cover 1 (xs == []) "xs empty" $
>   prop_FingerTree_fromList_hom_cons pa pm a xs
> 
> test_FingerTree_fromList_hom_cons :: TestTree
> test_FingerTree_fromList_hom_cons =
>   testGroup "fromList (a : xs) == cons a (fromList xs)"
>     [ testProperty "Char/Count" $
>         cprop_FingerTree_fromList_hom_cons pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTree_fromList_hom_cons pBool pTup
> 
>     , testCases
>       (uncurry $ prop_FingerTree_fromList_hom_cons pChar pCount)
>       [ ( "mempty",    ('a', []) )
>       , ( "singleton", ('a', ['b']) )
>       ]
>     ]

And `fromList` preserves `snoc`:

> prop_FingerTree_fromList_hom_snoc
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> [a]
>   -> Property
> 
> prop_FingerTree_fromList_hom_snoc _ _ a xs =
>   property $
>     let zs = fromListFT xs :: FingerTree m a in
>     (fromListFT $ xs ++ [a]) == (snoc a zs)
> 
> cprop_FingerTree_fromList_hom_snoc pa pm a xs =
>   cover 40 (length xs > 5) "length xs > 5" $
>   cover 1 (xs == []) "xs empty" $
>   prop_FingerTree_fromList_hom_snoc pa pm a xs
> 
> test_FingerTree_fromList_hom_snoc :: TestTree
> test_FingerTree_fromList_hom_snoc =
>   testGroup "fromList (a : xs) == snoc a (fromList xs)"
>     [ testProperty "Char/Count" $
>         cprop_FingerTree_fromList_hom_snoc pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTree_fromList_hom_snoc pBool pTup
> 
>     , testCases
>       (uncurry $ prop_FingerTree_fromList_hom_snoc pChar pCount)
>       [ ( "mempty",    ('a', []) )
>       , ( "singleton", ('a', ['b']) )
>       ]
>     ]



Reverse is a monoid involution
------------------------------

`reverseFT` is also _almost_ a homomorphism; it's an antihomomorphism.

> test_FingerTree_reverse :: TestTree
> test_FingerTree_reverse =
>   testGroup "Reverse is a monoid involution"
>     [ test_FingerTree_reverse_involution
>     , test_FingerTree_reverse_identity
>     , test_FingerTree_reverse_product
>     , test_FingerTree_reverse_cons
>     , test_FingerTree_reverse_snoc
>     ]

`reverse` is an involution:

> prop_FingerTree_reverse_involution
>    , cprop_FingerTree_reverse_involution
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTree m a
>   -> Property
> 
> prop_FingerTree_reverse_involution _ _ xs =
>   property $
>     xs == (reverseFT (reverseFT xs))
> 
> cprop_FingerTree_reverse_involution pa pm xs =
>   checkOneDepth xs $
>   prop_FingerTree_reverse_involution pa pm xs
> 
> test_FingerTree_reverse_involution :: TestTree
> test_FingerTree_reverse_involution =
>   testGroup "reverse . reverse == id"
>     [ testProperty "Char/Count" $
>         cprop_FingerTree_reverse_involution pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTree_reverse_involution pBool pTup
> 
>     , testCases
>       (prop_FingerTree_reverse_involution pChar pCount)
>       [ ( "mempty", mempty )
>       , ( "leaf",   leaf 'a' )
>       ]
>     ]

`reverse` preserves the identity:

> prop_FingerTree_reverse_identity
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> Property
> 
> prop_FingerTree_reverse_identity _ _ =
>   property $
>     (mempty :: FingerTree m a)
>       == reverseFT mempty
> 
> test_FingerTree_reverse_identity :: TestTree
> test_FingerTree_reverse_identity =
>   testGroup "reverse mempty == mempty"
>     [ testProperty "Char/Count" $
>         prop_FingerTree_reverse_identity pChar pCount
>     , testProperty "Bool/Tup" $
>         prop_FingerTree_reverse_identity pBool pTup
>     ]

`reverse` antipreserves products:

> prop_FingerTree_reverse_product
>    , cprop_FingerTree_reverse_product
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTree m a -> FingerTree m a
>   -> Property
> 
> prop_FingerTree_reverse_product _ _ xs ys =
>   property $
>     (reverseFT (xs <> ys))
>       == ((reverseFT ys) <> (reverseFT xs))
> 
> cprop_FingerTree_reverse_product pa pm xs ys =
>   checkPairDepth xs ys $
>   prop_FingerTree_reverse_product pa pm xs ys
> 
> test_FingerTree_reverse_product :: TestTree
> test_FingerTree_reverse_product =
>   testGroup "reverse (xs <> ys) == reverse ys <> reverse xs"
>     [ testProperty "Char/Count" $
>         cprop_FingerTree_reverse_product pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTree_reverse_product pBool pTup
> 
>     , testCases
>       (uncurry $ prop_FingerTree_reverse_product pChar pCount)
>       [ ( "mempty", (mempty, mempty) )
>       , ( "leaf",   (leaf 'a', leaf 'b') )
>       ]
>     ]

`reverse` turns `cons` into `snoc`:

> prop_FingerTree_reverse_cons
>    , cprop_FingerTree_reverse_cons
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> FingerTree m a
>   -> Property
> 
> prop_FingerTree_reverse_cons _ _ a xs =
>   property $
>     (reverseFT (cons a xs))
>       == (snoc a (reverseFT xs))
> 
> cprop_FingerTree_reverse_cons pa pm a xs =
>   checkOneDepth xs $
>   prop_FingerTree_reverse_cons pa pm a xs
> 
> test_FingerTree_reverse_cons :: TestTree
> test_FingerTree_reverse_cons =
>   testGroup "reverse (cons a xs) == snoc a (reverse xs)"
>     [ testProperty "Char/Count" $
>         cprop_FingerTree_reverse_cons pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTree_reverse_cons pBool pTup
> 
>     , testCases
>       (uncurry $ prop_FingerTree_reverse_cons pChar pCount)
>       [ ( "mempty", ('a', mempty) )
>       , ( "leaf",   ('a', leaf 'b') )
>       ]
>     ]

And turns `snoc` into `cons`:

> prop_FingerTree_reverse_snoc
>    , cprop_FingerTree_reverse_snoc
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> FingerTree m a
>   -> Property
> 
> prop_FingerTree_reverse_snoc _ _ a xs =
>   property $
>     (reverseFT (snoc a xs))
>       == (cons a (reverseFT xs))
> 
> cprop_FingerTree_reverse_snoc pa pm a xs =
>   checkOneDepth xs $
>   prop_FingerTree_reverse_snoc pa pm a xs
> 
> test_FingerTree_reverse_snoc :: TestTree
> test_FingerTree_reverse_snoc =
>   testGroup "reverse (snoc a xs) == cons a (reverse xs)"
>     [ testProperty "Char/Count" $
>         cprop_FingerTree_reverse_snoc pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTree_reverse_snoc pBool pTup
> 
>     , testCases
>       (uncurry $ prop_FingerTree_reverse_snoc pChar pCount)
>       [ ( "mempty", ('a', mempty) )
>       , ( "leaf",   ('a', leaf 'b') )
>       ]
>     ]



Leaf is cons and snoc
---------------------

This is a grab bag of properties of `leaf`.

> test_FingerTree_leaf :: TestTree
> test_FingerTree_leaf =
>   testGroup "Leaf properties"
>     [ test_FingerTree_cons_cat_singleton
>     , test_FingerTree_snoc_cat_singleton
>     , test_FingerTree_leaf_not_empty
>     , test_FingerTree_leaf_depth
>     ]

`leaf` interacts with `cons`:

> prop_FingerTree_cons_cat_singleton
>    , cprop_FingerTree_cons_cat_singleton
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> FingerTree m a
>   -> Property
> 
> prop_FingerTree_cons_cat_singleton _ _ a xs =
>   property $
>     (cons a xs) == ((leaf a) <> xs)
> 
> cprop_FingerTree_cons_cat_singleton pa pm a xs =
>   checkOneDepth xs $
>   prop_FingerTree_cons_cat_singleton pa pm a xs
> 
> test_FingerTree_cons_cat_singleton :: TestTree
> test_FingerTree_cons_cat_singleton =
>   testGroup "cons a xs == leaf a <> xs"
>     [ testProperty "Char/Count" $
>         cprop_FingerTree_cons_cat_singleton pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTree_cons_cat_singleton pBool pTup
> 
>     , testCases
>       (uncurry $ prop_FingerTree_cons_cat_singleton pChar pCount)
>       [ ( "mempty", ('a', mempty) )
>       , ( "leaf",   ('a', leaf 'b') )
>       ]
>     ]

And with `snoc`:

> prop_FingerTree_snoc_cat_singleton
>    , cprop_FingerTree_snoc_cat_singleton
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> FingerTree m a
>   -> Property
> 
> prop_FingerTree_snoc_cat_singleton _ _ a xs =
>   property $
>     (snoc a xs) == (xs <> (leaf a))
> 
> cprop_FingerTree_snoc_cat_singleton pa pm a xs =
>   checkOneDepth xs $
>   prop_FingerTree_snoc_cat_singleton pa pm a xs
> 
> test_FingerTree_snoc_cat_singleton :: TestTree
> test_FingerTree_snoc_cat_singleton =
>   testGroup "snoc a xs == leaf a <> xs"
>     [ testProperty "Char/Count" $
>         cprop_FingerTree_snoc_cat_singleton pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTree_snoc_cat_singleton pBool pTup
> 
>     , testCases
>       (uncurry $ prop_FingerTree_snoc_cat_singleton pChar pCount)
>       [ ( "mempty", ('a', mempty) )
>       , ( "leaf",   ('a', leaf 'b') )
>       ]
>     ]

`leaf` is not empty:

> prop_FingerTree_leaf_not_empty
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a
>   -> Property
> 
> prop_FingerTree_leaf_not_empty _ _ a =
>   property $
>     notEmptyFT (leaf a :: FingerTree m a)
> 
> test_FingerTree_leaf_not_empty :: TestTree
> test_FingerTree_leaf_not_empty =
>   testGroup "notEmpty (leaf x)"
>     [ testProperty "Char/Count" $
>         prop_FingerTree_leaf_not_empty pChar pCount
>     , testProperty "Bool/Tup" $
>         prop_FingerTree_leaf_not_empty pBool pTup
>     ]

And `leaf` has depth 1:

> prop_FingerTree_leaf_depth
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a
>   -> Property
> 
> prop_FingerTree_leaf_depth _ _ a =
>   property $
>     let x = leaf a :: FingerTree m a in
>     depthFT x == 1
> 
> test_FingerTree_leaf_depth :: TestTree
> test_FingerTree_leaf_depth =
>   testGroup "depth (leaf x) == 1"
>     [ testProperty "Char/Count" $
>         prop_FingerTree_leaf_depth pChar pCount
>     , testProperty "Bool/Tup" $
>         prop_FingerTree_leaf_depth pBool pTup
>     ]



Splitting properties
--------------------

The `split` operation is supposed to satisfy some nice properties, which we can turn into tests.

> test_FingerTree_split :: TestTree
> test_FingerTree_split =
>   testGroup "Splitting properties"
>     [ test_FingerTree_split_concat
>     , test_FingerTree_split_value_left
>     , test_FingerTree_split_value_mid
>     , test_FingerTree_split_example
>     ]

If the result of a split succeeds, the results should concatenate to the original finger tree.

> prop_FingerTree_split_concat
>    , cprop_FingerTree_split_concat
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> Fun m Bool -> FingerTree m a
>   -> Property
> 
> prop_FingerTree_split_concat _ _ p xs =
>   (False == applyFun p mempty) ==>
>   (True == applyFun p (value xs)) ==>
>   case splitFT (applyFun p) xs of
>     Nothing -> False
>     Just (as, x, bs) ->
>       xs == mconcat [ as, leaf x, bs ]
> 
> cprop_FingerTree_split_concat pa pm p xs =
>   cover 30 (depthFT xs > 2) "depth xs > 2" $
>   cover 5 (depthFT xs == 1) "depth xs == 1" $
>   prop_FingerTree_split_concat pa pm p xs
> 
> test_FingerTree_split_concat :: TestTree
> test_FingerTree_split_concat =
>   testGroup "Split concat property"
>     [ testProperty "Char/Count" $
>         cprop_FingerTree_split_concat pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTree_split_concat pBool pTup
>     ]

If the split succeeds, the predicate on the value of the left portion should be false.

> prop_FingerTree_split_value_left
>    , cprop_FingerTree_split_value_left
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> Fun m Bool -> FingerTree m a
>   -> Property
> 
> prop_FingerTree_split_value_left _ _ p xs =
>   (False == applyFun p mempty) ==>
>   (True == applyFun p (value xs)) ==>
>   case splitFT (applyFun p) xs of
>     Nothing -> False
>     Just (as, x, bs) ->
>       not $ applyFun p $ value as
> 
> cprop_FingerTree_split_value_left pa pm p xs =
>   cover 30 (depthFT xs > 2) "depth xs > 2" $
>   cover 5 (depthFT xs == 1) "depth xs == 1" $
>   prop_FingerTree_split_value_left pa pm p xs
> 
> test_FingerTree_split_value_left :: TestTree
> test_FingerTree_split_value_left =
>   testGroup "p (value as) == False"
>     [ testProperty "Char/Count" $
>         cprop_FingerTree_split_value_left pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTree_split_value_left pBool pTup
>     ]

If the split succeeds, the predicate on the value of the middle portion (with that of the left portion prepended) should be true.

> prop_FingerTree_split_value_mid
>    , cprop_FingerTree_split_value_mid
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> Fun m Bool -> FingerTree m a
>   -> Property
> 
> prop_FingerTree_split_value_mid _ _ p xs =
>   (False == applyFun p mempty) ==>
>   (True == applyFun p (value xs)) ==>
>   case splitFT (applyFun p) xs of
>     Nothing -> False
>     Just (as, x, bs) ->
>       applyFun p (value as <> value x)
> 
> cprop_FingerTree_split_value_mid pa pm p xs =
>   cover 30 (depthFT xs > 2) "depth xs > 2" $
>   cover 5 (depthFT xs == 1) "depth xs == 1" $
>   prop_FingerTree_split_value_mid pa pm p xs
> 
> test_FingerTree_split_value_mid :: TestTree
> test_FingerTree_split_value_mid =
>   testGroup "p (value as <> value x) == True"
>     [ testProperty "Char/Count" $
>         cprop_FingerTree_split_value_mid pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTree_split_value_mid pBool pTup
>     ]

For good measure, we'll also validate some specific cases to check our understanding of how splitting works.

> prop_FingerTree_split_example
>   :: forall m a
>    . ( Eq a, Valued m a, Show a )
>   => Proxy a -> Proxy m
>   -> (m -> Bool) -> FingerTree m a
>   -> Maybe (FingerTree m a, a, FingerTree m a)
>   -> Property
> 
> prop_FingerTree_split_example _ _ p xs y =
>   property $
>     y === splitFT p xs
> 
> test_FingerTree_split_example :: TestTree
> test_FingerTree_split_example =
>   testGroup "split cases"
>     [ testCases
>       (uncurry3 $ prop_FingerTree_split_example pChar pCount)
>       [ ( "k > 3"
>         , ( \(Count k) -> k > 3
>           , fromListFT ['a', 'b', 'c', 'd', 'e']
>           , Just (fromListFT ['a', 'b', 'c'], 'd', fromListFT ['e'])
>           )
>         )
> 
>       , ( "k >= 2"
>         , ( \(Count k) -> k >= 2
>           , fromListFT ['a', 'b', 'c', 'd']
>           , Just (fromListFT ['a'], 'b', fromListFT ['c', 'd'])
>           )
>         )
> 
>       , ( "k > length xs"
>         , ( \(Count k) -> k >= 7
>           , fromListFT ['a', 'b', 'c', 'd']
>           , Nothing
>           )
>         )
>       ]
> 
>     , testCases
>       (uncurry3 $ prop_FingerTree_split_example pBool pTup)
>       [ ( "x > 1"
>         , ( \(Tup x y) -> x > 1
>           , fromListFT [False, True, True, False, True]
>           , Just
>             ( fromListFT [False, True, True]
>             , False
>             , fromListFT [True]
>             )
>           )
>         )
>       ]
>     ]



Break properties
----------------

Grab bag of properties for the `break`-related functions.

> test_FingerTree_break :: TestTree
> test_FingerTree_break =
>   testGroup "Break properties"
>     [ test_FingerTree_break_prefix_concat
>     ]

`breakPrefix` is a cat-factorization:

> prop_FingerTree_break_prefix_concat
>    , cprop_FingerTree_break_prefix_concat
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> Fun m Bool -> FingerTree m a
>   -> Property
> 
> prop_FingerTree_break_prefix_concat _ _ p xs =
>   property $
>     let (as,bs) = breakPrefixWhileValueFT (applyFun p) xs
>     in xs == as <> bs
> 
> cprop_FingerTree_break_prefix_concat pa pm p xs =
>   checkOneDepth xs $
>   prop_FingerTree_break_prefix_concat pa pm p xs
> 
> test_FingerTree_break_prefix_concat :: TestTree
> test_FingerTree_break_prefix_concat =
>   testGroup "Concat property"
>     [ testProperty "Char/Count" $
>         cprop_FingerTree_break_prefix_concat pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTree_break_prefix_concat pBool pTup
>     ]







Test Helpers
============

> checkOneDepth
>   :: ( Valued m a, Testable prop )
>   => FingerTree m a
>   -> prop -> Property
> checkOneDepth xs prop =
>   cover 1 (depthFT xs == 0) "depth xs == 0" $
>   cover 1 (depthFT xs == 1) "depth xs == 1" $
>   cover 30 (depthFT xs >= 2) "depth xs >= 2" $
>   prop

> checkPairDepth
>   :: ( Valued m a, Testable prop )
>   => FingerTree m a -> FingerTree m a
>   -> prop -> Property
> checkPairDepth xs ys prop =
>   cover 0.2 ((depthFT xs == 0) && (depthFT ys == 0))
>     "depth xs == 0, depth ys == 0" $
>   cover 0.2 ((depthFT xs == 0) && (depthFT ys == 1))
>     "depth xs == 0, depth ys == 1" $
>   cover 0.2 ((depthFT xs == 0) && (depthFT ys >= 2))
>     "depth xs == 0, depth ys >= 2" $
>   cover 0.2 ((depthFT xs == 1) && (depthFT ys == 0))
>     "depth xs == 1, depth ys == 0" $
>   cover 0.2 ((depthFT xs == 1) && (depthFT ys == 1))
>     "depth xs == 1, depth ys == 1" $
>   cover 0.2 ((depthFT xs == 1) && (depthFT ys >= 2))
>     "depth xs == 1, depth ys >= 2" $
>   cover 0.2 ((depthFT xs >= 2) && (depthFT ys == 0))
>     "depth xs >= 2, depth ys == 0" $
>   cover 0.2 ((depthFT xs >= 2) && (depthFT ys == 1))
>     "depth xs >= 2, depth ys == 1" $
>   cover 30 ((depthFT xs >= 2) && (depthFT ys >= 2))
>     "depth xs >= 2, depth ys >= 2" $
>   prop

> -}
