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
>   , ZZ(..)
>   , Tup(..)
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
> instance Prune Tup where
>   prune (Tup a b) =
>     [ Tup a c | c <- prune b ] ++
>     [ Tup c b | c <- prune a ]
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

> instance Semigroup Bool where
>   (<>) = (&&)
> 
> instance Monoid Bool where
>   mempty = True



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



Reverse is a monoid involution
------------------------------

`reverseFT` is also _almost_ a homomorphism; it's an antihomomorphism.

> test_FingerTree_reverse :: TestTree
> test_FingerTree_reverse =
>   let
>     rev
>       :: ( Valued m a )
>       => Proxy (FingerTree m a)
>       -> FingerTree m a -> FingerTree m a 
>     rev _ = reverseFT
>   in testGroup "Reverse is a monoid involution"
>     [ testKreb "involution (Count/Char)" $
>         check_prop_involutive
>           (rev (Proxy :: Proxy (FingerTree Count Char)))
>     , testKreb "involution (Tup/Bool)" $
>         check_prop_involutive
>           (rev (Proxy :: Proxy (FingerTree Count Char)))
> 
>     , test_Monoid_antihomomorphism
>         (rev (Proxy :: Proxy (FingerTree Count Char)))
>     , test_Monoid_antihomomorphism
>         (rev (Proxy :: Proxy (FingerTree Tup Bool)))
>     ]



Functor laws for fmapFT
-----------------------

Recall that `FingerTree m` can't be a bona fide instance of the functor class due to the weirdness of the `Valued` constraint; essentially the data type held by the tree is crucially related to `m`, so the type signature of the real `fmap` doesn't work out. But we do have a function that behaves like fmap.

> test_FingerTree_fmapFT :: TestTree
> test_FingerTree_fmapFT =
>   testGroup "Functor Laws for FingerTree"
>     [ testGroup "Count/Char"
>       [ testKreb "fmap id == id" $
>         (check_prop_fmap_id fmapFT :: FingerTree Count Char -> Check)
>       , testKreb "fmap (g . f) = fmap g . fmap f" $
>           \(g :: Fun Char Char) (f :: Fun Char Char) (x :: FingerTree Count Char) ->
>             check_prop_fmap_comp fmapFT fmapFT fmapFT (apFun g) (apFun f) x
>       ]
> 
>     , testGroup "Tup/Bool"
>       [ testKreb "fmap id == id" $
>         (check_prop_fmap_id fmapFT :: FingerTree Tup Bool -> Check)
>       , testKreb "fmap (g . f) = fmap g . fmap f" $
>           \(g :: Fun Bool Bool) (f :: Fun Bool Bool) (x :: FingerTree Tup Bool) ->
>             check_prop_fmap_comp fmapFT fmapFT fmapFT (apFun g) (apFun f) x
>       ]
>     ]



Finger trees and lists are interconvertible
-------------------------------------------

We have two functions, `toList` (from the `Foldable` class) and `fromListFT`, which convert between finger trees and lists. These should be mutual inverses.

> test_FingerTree_List_convert
>   :: TestTree
> test_FingerTree_List_convert =
>   testGroup "FingerTree and List are interconvertible"
>     [ testGroup "Count/Char"
>       [ testKreb "toList . fromList == id" $
>           check_prop_function_inverse
>             (toList :: FingerTree Count Char -> [Char])
>             (fromListFT :: [Char] -> FingerTree Count Char)
>       , testKreb "fromList . toList == id" $
>           check_prop_function_inverse
>             (fromListFT :: [Char] -> FingerTree Count Char)
>             (toList :: FingerTree Count Char -> [Char])
>       ]
> 
>     , testGroup "Tup/Bool"
>       [ testKreb "toList . fromList == id" $
>           check_prop_function_inverse
>             (toList :: FingerTree Tup Bool -> [Bool])
>             (fromListFT :: [Bool] -> FingerTree Tup Bool)
>       , testKreb "fromList . toList == id" $
>           check_prop_function_inverse
>             (fromListFT :: [Bool] -> FingerTree Tup Bool)
>             (toList :: FingerTree Tup Bool -> [Bool])
>       ]
>     ]



> test_FingerTree_cons_snoc_action
>   :: TestTree
> test_FingerTree_cons_snoc_action =
>   testGroup "FingerTree cons/snoc actions"
>     [ testGroup "Count/Char"
>       [ testKreb "toList/cons" $
>           check_prop_left_affine_func
>             (cons :: Char -> FingerTree Count Char -> FingerTree Count Char)
>             ((:) :: Char -> [Char] -> [Char])
>             toList
>       , testKreb "toList/snoc" $
>           check_prop_right_affine_func
>             (flip snoc :: FingerTree Count Char -> Char -> FingerTree Count Char)
>             ((\xs x -> xs ++ [x]) :: [Char] -> Char -> [Char])
>             toList
>       , testKreb "fromList/cons" $
>           check_prop_left_affine_func
>             ((:) :: Char -> [Char] -> [Char])
>             (cons :: Char -> FingerTree Count Char -> FingerTree Count Char)
>             fromListFT
>       , testKreb "fromList/snoc" $
>           check_prop_right_affine_func
>             ((\xs x -> xs ++ [x]) :: [Char] -> Char -> [Char])
>             (flip snoc :: FingerTree Count Char -> Char -> FingerTree Count Char)
>             fromListFT
>       , testKreb "reverse/cons" $
>           check_prop_right_left_affine_func
>             (flip snoc :: FingerTree Count Char -> Char -> FingerTree Count Char)
>             (cons :: Char -> FingerTree Count Char -> FingerTree Count Char)
>             reverseFT
>       , testKreb "reverse/snoc" $
>           check_prop_left_right_affine_func
>             (cons :: Char -> FingerTree Count Char -> FingerTree Count Char)
>             (flip snoc :: FingerTree Count Char -> Char -> FingerTree Count Char)
>             reverseFT
>       ]
> 
>     , testGroup "Tup/Bool"
>       [ testKreb "toList/cons" $
>           check_prop_left_affine_func
>             (cons :: Bool -> FingerTree Tup Bool -> FingerTree Tup Bool)
>             ((:) :: Bool -> [Bool] -> [Bool])
>             toList
>       , testKreb "toList/snoc" $
>           check_prop_right_affine_func
>             (flip snoc :: FingerTree Tup Bool -> Bool -> FingerTree Tup Bool)
>             ((\xs x -> xs ++ [x]) :: [Bool] -> Bool -> [Bool])
>             toList
>       , testKreb "fromList/cons" $
>           check_prop_left_affine_func
>             ((:) :: Bool -> [Bool] -> [Bool])
>             (cons :: Bool -> FingerTree Tup Bool -> FingerTree Tup Bool)
>             fromListFT
>       , testKreb "fromList/snoc" $
>           check_prop_right_affine_func
>             ((\xs x -> xs ++ [x]) :: [Bool] -> Bool -> [Bool])
>             (flip snoc :: FingerTree Tup Bool -> Bool -> FingerTree Tup Bool)
>             fromListFT
>       , testKreb "reverse/cons" $
>           check_prop_right_left_affine_func
>             (flip snoc :: FingerTree Tup Bool -> Bool -> FingerTree Tup Bool)
>             (cons :: Bool -> FingerTree Tup Bool -> FingerTree Tup Bool)
>             reverseFT
>       , testKreb "reverse/snoc" $
>           check_prop_left_right_affine_func
>             (cons :: Bool -> FingerTree Tup Bool -> FingerTree Tup Bool)
>             (flip snoc :: FingerTree Tup Bool -> Bool -> FingerTree Tup Bool)
>             reverseFT
>       ]
>     ]



toList is a monoid homomorphism
-------------------------------

> test_FingerTree_toList :: TestTree
> test_FingerTree_toList =
>   testGroup "Reverse is a monoid involution"
>     [ test_Monoid_homomorphism
>         (toList :: FingerTree Count Char -> [Char])
>     , test_Monoid_homomorphism
>         (toList :: FingerTree Tup Bool -> [Bool])
>     ]



fromListFT is a monoid homomorphism
-----------------------------------

> test_FingerTree_fromListFT :: TestTree
> test_FingerTree_fromListFT =
>   testGroup "Reverse is a monoid involution"
>     [ test_Monoid_homomorphism
>         (fromListFT :: [Char] -> FingerTree Count Char)
>     , test_Monoid_homomorphism
>         (fromListFT :: [Bool] -> FingerTree Tup Bool)
>     ]



Fold is lawful on fingertrees
-----------------------------

We get a lot of mileage out of the `Foldable` instance for `FingerTree`, so it makes sense to explicitly check that the laws for this class are not not satisifed.

> test_FingerTree_Foldable :: TestTree
> test_FingerTree_Foldable =
>   testGroup "Foldable laws for FingerTree"
>     [ test_Foldable_laws
>         (Proxy :: Proxy (FingerTree Tup)) (Proxy :: Proxy Bool)
>         (Proxy :: Proxy Bool) (Proxy :: Proxy Bool)
> 
>     , test_FingerTree_fold_value
>     ]

> check_FingerTree_fold_value
>   :: forall a m
>    . ( Eq a, Valued m a, Eq m )
>   => Proxy a -> Proxy m
>   -> FingerTree m a -> Check
> check_FingerTree_fold_value _ _ xs =
>   check $
>     (value xs :: m) == fold (map value $ toList xs)
> 
> test_FingerTree_fold_value :: TestTree
> test_FingerTree_fold_value =
>   testGroup "value == fold . fmapFT value"
>     [ testKreb "Char/Count" $
>         check_FingerTree_fold_value pChar pCount
>     , testKreb "Bool/Tup" $
>         check_FingerTree_fold_value pBool pTup
>     ]



Cons and uncons are mutual inverses
-----------------------------------

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

> check_FingerTree_cons_uncons
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTree m a -> Check
> check_FingerTree_cons_uncons _ _ xs =
>   check $ case uncons xs of
>     Nothing -> True
>     Just (a,zs) -> xs == cons a zs
> 
> test_FingerTree_cons_uncons :: TestTree
> test_FingerTree_cons_uncons =
>   testGroup "cons . uncons == id"
>     [ testKreb "Char/Count" $
>         check_FingerTree_cons_uncons pChar pCount
>     , testKreb "Bool/Tup" $
>         check_FingerTree_cons_uncons pBool pTup
>     ]

> check_FingerTree_uncons_cons
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> FingerTree m a -> Check
> check_FingerTree_uncons_cons _ _ a xs =
>   check $ case uncons (cons a xs) of
>     Nothing -> False
>     Just (b,ys) -> (a == b) && (xs == ys)
> 
> test_FingerTree_uncons_cons :: TestTree
> test_FingerTree_uncons_cons =
>   testGroup "uncons . cons == id"
>     [ testKreb "Char/Count" $
>         check_FingerTree_uncons_cons pChar pCount
>     , testKreb "Bool/Tup" $
>         check_FingerTree_uncons_cons pBool pTup
>     ]

> check_FingerTree_snoc_unsnoc
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTree m a -> Check
> check_FingerTree_snoc_unsnoc _ _ xs =
>   check $ case unsnoc xs of
>     Nothing -> True
>     Just (a,zs) -> xs == snoc a zs
> 
> test_FingerTree_snoc_unsnoc :: TestTree
> test_FingerTree_snoc_unsnoc =
>   testGroup "snoc . unsnoc == id"
>     [ testKreb "Char/Count" $
>         check_FingerTree_snoc_unsnoc pChar pCount
>     , testKreb "Bool/Tup" $
>         check_FingerTree_snoc_unsnoc pBool pTup
>     ]

> check_FingerTree_unsnoc_snoc
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> FingerTree m a -> Check
> check_FingerTree_unsnoc_snoc _ _ a xs =
>   check $
>     case unsnoc (snoc a xs) of
>       Nothing -> False
>       Just (b,ys) -> (a == b) && (xs == ys)
> 
> test_FingerTree_unsnoc_snoc :: TestTree
> test_FingerTree_unsnoc_snoc =
>   testGroup "unsnoc . snoc == id"
>     [ testKreb "Char/Count" $
>         check_FingerTree_unsnoc_snoc pChar pCount
>     , testKreb "Bool/Tup" $
>         check_FingerTree_unsnoc_snoc pBool pTup
>     ]

> check_FingerTree_uncons_mempty
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTree m a -> Check
> check_FingerTree_uncons_mempty _ _ xs =
>   check $
>     (Nothing == uncons xs) == (xs == mempty)
> 
> test_FingerTree_uncons_mempty :: TestTree
> test_FingerTree_uncons_mempty =
>   testGroup "uncons == Nothing"
>     [ testKreb "Char/Count" $
>         check_FingerTree_uncons_mempty pChar pCount
>     , testKreb "Bool/Tup" $
>         check_FingerTree_uncons_mempty pBool pTup
>     ]

> check_FingerTree_unsnoc_mempty
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTree m a -> Check
> check_FingerTree_unsnoc_mempty _ _ xs =
>   check $
>     (Nothing == unsnoc xs) == (xs == mempty)
> 
> test_FingerTree_unsnoc_mempty :: TestTree
> test_FingerTree_unsnoc_mempty =
>   testGroup "unsnoc == Nothing"
>     [ testKreb "Char/Count" $
>         check_FingerTree_unsnoc_mempty pChar pCount
>     , testKreb "Bool/Tup" $
>         check_FingerTree_unsnoc_mempty pBool pTup
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

> check_FingerTree_cons_cat_singleton
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> FingerTree m a -> Check
> check_FingerTree_cons_cat_singleton _ _ a xs =
>   check $
>     (cons a xs) == ((leaf a) <> xs)
> 
> test_FingerTree_cons_cat_singleton :: TestTree
> test_FingerTree_cons_cat_singleton =
>   testGroup "cons a xs == leaf a <> xs"
>     [ testKreb "Char/Count" $
>         check_FingerTree_cons_cat_singleton pChar pCount
>     , testKreb "Bool/Tup" $
>         check_FingerTree_cons_cat_singleton pBool pTup
>     ]

And with `snoc`:

> check_FingerTree_snoc_cat_singleton
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> FingerTree m a -> Check
> check_FingerTree_snoc_cat_singleton _ _ a xs =
>   check $
>     (snoc a xs) == (xs <> (leaf a))
> 
> test_FingerTree_snoc_cat_singleton :: TestTree
> test_FingerTree_snoc_cat_singleton =
>   testGroup "snoc a xs == leaf a <> xs"
>     [ testKreb "Char/Count" $
>         check_FingerTree_snoc_cat_singleton pChar pCount
>     , testKreb "Bool/Tup" $
>         check_FingerTree_snoc_cat_singleton pBool pTup
>     ]

`leaf` is not empty:

> check_FingerTree_leaf_not_empty
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> Check
> check_FingerTree_leaf_not_empty _ _ a =
>   check $
>     notEmptyFT (leaf a :: FingerTree m a)
> 
> test_FingerTree_leaf_not_empty :: TestTree
> test_FingerTree_leaf_not_empty =
>   testGroup "notEmpty (leaf x)"
>     [ testKreb "Char/Count" $
>         check_FingerTree_leaf_not_empty pChar pCount
>     , testKreb "Bool/Tup" $
>         check_FingerTree_leaf_not_empty pBool pTup
>     ]

And `leaf` has depth 1:

> check_FingerTree_leaf_depth
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> Check
> check_FingerTree_leaf_depth _ _ a =
>   check $
>     let x = leaf a :: FingerTree m a in
>     depthFT x == 1
> 
> test_FingerTree_leaf_depth :: TestTree
> test_FingerTree_leaf_depth =
>   testGroup "depth (leaf x) == 1"
>     [ testKreb "Char/Count" $
>         check_FingerTree_leaf_depth pChar pCount
>     , testKreb "Bool/Tup" $
>         check_FingerTree_leaf_depth pBool pTup
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

> check_FingerTree_split_concat
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> Fun m Bool -> FingerTree m a
>   -> Bool
> check_FingerTree_split_concat _ _ p xs =
>   if (False == apFun p mempty) && (True == apFun p (value xs))
>     then case splitFT (apFun p) xs of
>       Nothing -> False
>       Just (as, x, bs) ->
>         xs == mconcat [ as, leaf x, bs ]
>     else True
> 
> test_FingerTree_split_concat :: TestTree
> test_FingerTree_split_concat =
>   testGroup "Split concat property"
>     [ testKreb "Char/Count" $
>         check_FingerTree_split_concat pChar pCount
>     , testKreb "Bool/Tup" $
>         check_FingerTree_split_concat pBool pTup
>     ]

If the split succeeds, the predicate on the value of the left portion should be false.

> check_FingerTree_split_value_left
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> Fun m Bool -> FingerTree m a
>   -> Bool
> check_FingerTree_split_value_left _ _ p xs =
>   if (False == apFun p mempty) && (True == apFun p (value xs))
>     then case splitFT (apFun p) xs of
>       Nothing -> False
>       Just (as, x, bs) ->
>         not $ apFun p $ value as
>     else True
> 
> test_FingerTree_split_value_left :: TestTree
> test_FingerTree_split_value_left =
>   testGroup "p (value as) == False"
>     [ testKreb "Char/Count" $
>         check_FingerTree_split_value_left pChar pCount
>     , testKreb "Bool/Tup" $
>         check_FingerTree_split_value_left pBool pTup
>     ]

If the split succeeds, the predicate on the value of the middle portion (with that of the left portion prepended) should be true.

> check_FingerTree_split_value_mid
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> Fun m Bool -> FingerTree m a
>   -> Bool
> check_FingerTree_split_value_mid _ _ p xs =
>   if (False == apFun p mempty) && (True == apFun p (value xs))
>     then case splitFT (apFun p) xs of
>       Nothing -> False
>       Just (as, x, bs) ->
>         apFun p (value as <> value x)
>     else True
> 
> test_FingerTree_split_value_mid :: TestTree
> test_FingerTree_split_value_mid =
>   testGroup "p (value as <> value x) == True"
>     [ testKreb "Char/Count" $
>         check_FingerTree_split_value_mid pChar pCount
>     , testKreb "Bool/Tup" $
>         check_FingerTree_split_value_mid pBool pTup
>     ]

For good measure, we'll also validate some specific cases to check our understanding of how splitting works.

> check_FingerTree_split_example
>   :: forall m a
>    . ( Eq a, Valued m a, Show a )
>   => Proxy a -> Proxy m
>   -> ( (m -> Bool)
>      , FingerTree m a
>      , Maybe (FingerTree m a, a, FingerTree m a) )
>   -> Check
> check_FingerTree_split_example _ _ (p, xs, y) =
>   check $
>     y == splitFT p xs
> 
> test_FingerTree_split_example :: TestTree
> test_FingerTree_split_example =
>   testGroup "split cases"
>     [ testKrebCases "Count/Char"
>       (check_FingerTree_split_example pChar pCount)
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
>     , testKrebCases "Tup/Bool"
>       (check_FingerTree_split_example pBool pTup)
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

> check_FingerTree_break_prefix_concat
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> Fun m Bool -> FingerTree m a
>   -> Check
> check_FingerTree_break_prefix_concat _ _ p xs =
>   check $
>     let (as,bs) = breakPrefixWhileValueFT (apFun p) xs
>     in xs == as <> bs
> 
> test_FingerTree_break_prefix_concat :: TestTree
> test_FingerTree_break_prefix_concat =
>   testGroup "Concat property"
>     [ testKreb "Char/Count" $
>         check_FingerTree_break_prefix_concat pChar pCount
>     , testKreb "Bool/Tup" $
>         check_FingerTree_break_prefix_concat pBool pTup
>     ]





Test Suite
==========

> test_FingerTree :: TestTree
> test_FingerTree =
>   testGroup "FingerTree"
>     [ test_Count_Monoid
>     , test_FingerTree_Eq
>     , test_FingerTree_Monoid
>     , test_FingerTree_reverse
>     , test_FingerTree_fmapFT
>     , test_FingerTree_List_convert
>     , test_FingerTree_cons_snoc_action
>     , test_FingerTree_toList
>     , test_FingerTree_fromListFT
>     , test_FingerTree_Foldable
>     , test_FingerTree_cons_snoc_inverses
>     , test_FingerTree_leaf
>     , test_FingerTree_split
>     , test_FingerTree_break
>     ]
