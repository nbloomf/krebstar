---
title: Kreb.Struct.RunLengthEncoding.Test
---



Contents
--------

* [Introduction](#introduction)
* [Generators](#generators)
    * [Test Helpers](#test-helpers)
* [Tests](#tests)
    * [RunSize is a Monoid](#runsize-is-a-monoid)
    * [Run is a functor](#run-is-a-functor)
    * [RunLengthEncoding is a functor](#runlengthencoding-is-a-functor)
    * [RunLengthEncoding is a Monoid](#runlengthencoding-is-a-monoid)
    * [Foldable laws for RunLengthEncoding](#foldable-laws-for-runlengthencoding)
    * [Conversion](#runlengthencoding-conversion)
    * [Test Suite](#test-suite)



Introduction
============

> {-# LANGUAGE
>     MultiParamTypeClasses
>   , ScopedTypeVariables
>   , FlexibleContexts
> #-}
> 
> module Kreb.Struct.RunLengthEncoding.Test (
>     test_RunLengthEncoding
> ) where
> 
> import Data.Proxy
> import Data.Foldable
> 
> import Test.QuickCheck
> import Test.Tasty
> import Test.Tasty.QuickCheck
> 
> import Kreb.Struct.FingerTree
> import Kreb.Struct.RunLengthEncoding
> 
> import Kreb.Struct.FingerTree.Test
> import Kreb.Struct.FingerTreeZip.Test



Generators
==========

> instance
>   Arbitrary RunSize
>   where
>     arbitrary = do
>       Positive s <- arbitrary
>       Positive l <- arbitrary
>       return $ RunSize s l

> instance
>   ( Arbitrary a, Eq a
>   ) => Arbitrary (Run a)
>   where
>     arbitrary = do
>       Positive k <- arbitrary
>       a <- arbitrary
>       return (mkRun k a)

> instance
>   ( Arbitrary a, Eq a
>   ) => Arbitrary (RunLengthEncoding a)
>   where
>     arbitrary = do
>       let
>         run = do
>           Positive k <- arbitrary
>           a <- arbitrary
>           return (k,a)
>       fromFreqList <$> listOf run



Test Helpers
------------

> instance Valued Count Bool where
>   value _ = Count 1

> pInt :: Proxy Int
> pInt = Proxy



Tests
=====



RunSize is a monoid
-------------------

> test_RunSize_Monoid :: TestTree
> test_RunSize_Monoid =
>   testGroup "RunSize is a monoid"
>     [ testProperty "xs == mempty <> xs"
>         prop_RunSize_Monoid_neutral_left
>     , testProperty "xs == xs <> mempty"
>         prop_RunSize_Monoid_neutral_right
>     , testProperty "a <> (b <> c) == (a <> b) <> c"
>         prop_RunSize_Monoid_associative
>     ]

We have a left identity:

> prop_RunSize_Monoid_neutral_left
>   :: RunSize
>   -> Property
> 
> prop_RunSize_Monoid_neutral_left xs =
>   property $
>     xs == (mempty <> xs)

We have a right identity:

> prop_RunSize_Monoid_neutral_right
>   :: RunSize
>   -> Property
> 
> prop_RunSize_Monoid_neutral_right xs =
>   property $
>     xs == (xs <> mempty)

And the product on `RunSize` is associative.

> prop_RunSize_Monoid_associative
>   :: RunSize -> RunSize -> RunSize
>   -> Property
> 
> prop_RunSize_Monoid_associative a b c =
>   property $
>     (a <> (b <> c))
>       == ((a <> b) <> c)



Run is a functor
----------------

`Run` has a functor instance, which for good measure we should validate.

> test_Run_Functor :: TestTree
> test_Run_Functor =
>   testGroup "Functor laws for Run"
>     [ test_Run_Functor_identity
>     , localOption (QuickCheckTests 100) $
>         test_Run_Functor_composite
>     ]

`fmap` preserves the identity:

> prop_Run_Functor_identity
>   :: forall a
>    . ( Eq a )
>   => Proxy a
>   -> Run a
>   -> Property
> 
> prop_Run_Functor_identity _ x =
>   property $
>     x == fmap id x
> 
> test_Run_Functor_identity :: TestTree
> test_Run_Functor_identity =
>   testGroup "fmap id == id"
>     [ testProperty "Char" $
>         prop_Run_Functor_identity pChar
>     , testProperty "Bool" $
>         prop_Run_Functor_identity pBool
>     ]

And `fmap` preserves composition:

> prop_Run_Functor_composite
>   :: forall a b c
>    . ( Eq c )
>   => Proxy a -> Proxy b -> Proxy c
>   -> Fun a b -> Fun b c -> Run a
>   -> Property
> 
> prop_Run_Functor_composite _ _ _ f g x =
>   property $
>     (fmap (applyFun g . applyFun f) x)
>       == (fmap (applyFun g) $ fmap (applyFun f) x)
> 
> test_Run_Functor_composite :: TestTree
> test_Run_Functor_composite =
>   testGroup "fmap (f . g) == fmap f . fmap g"
>     [ testProperty "Char/Char/Char" $
>         prop_Run_Functor_composite pChar pChar pChar
>     , testProperty "Bool/Bool/Bool" $
>         prop_Run_Functor_composite pBool pBool pBool
>     ]



RunLengthEncoding is a functor
------------------------------

`RunLengthEncoding` also has a functor instance, which for good measure we should validate.

> test_RunLengthEncoding_Functor :: TestTree
> test_RunLengthEncoding_Functor =
>   testGroup "Functor laws for RunLengthEncoding"
>     [ test_RunLengthEncoding_Functor_identity
>     , test_RunLengthEncoding_Functor_composite
>     ]

`fmap` preserves the identity:

> prop_RunLengthEncoding_Functor_identity
>   :: forall a
>    . ( Eq a )
>   => Proxy a
>   -> RunLengthEncoding a
>   -> Property
> 
> prop_RunLengthEncoding_Functor_identity _ x =
>   property $
>     x == fmap id x
> 
> test_RunLengthEncoding_Functor_identity :: TestTree
> test_RunLengthEncoding_Functor_identity =
>   testGroup "fmap id == id"
>     [ testProperty "Char" $
>         prop_RunLengthEncoding_Functor_identity pChar
>     , testProperty "Bool" $
>         prop_RunLengthEncoding_Functor_identity pBool
>     ]

And `fmap` preserves composition:

> prop_RunLengthEncoding_Functor_composite
>   :: forall a b c
>    . ( Eq c )
>   => Proxy a -> Proxy b -> Proxy c
>   -> Fun a b -> Fun b c -> RunLengthEncoding a
>   -> Property
> 
> prop_RunLengthEncoding_Functor_composite _ _ _ f g x =
>   property $
>     (fmap (applyFun g . applyFun f) x)
>       == (fmap (applyFun g) $ fmap (applyFun f) x)
> 
> test_RunLengthEncoding_Functor_composite :: TestTree
> test_RunLengthEncoding_Functor_composite =
>   testGroup "fmap (f . g) == fmap f . fmap g"
>     [ testProperty "Char/Char/Char" $
>         prop_RunLengthEncoding_Functor_composite pChar pChar pChar
>     , testProperty "Bool/Bool/Bool" $
>         prop_RunLengthEncoding_Functor_composite pBool pBool pBool
>     ]



RunLengthEncoding is a Monoid
-----------------------------

> test_RunLengthEncoding_Monoid :: TestTree
> test_RunLengthEncoding_Monoid =
>   testGroup "Monoid laws for RunLengthEncoding"
>     [ test_RunLengthEncoding_Monoid_neutral_left
>     , test_RunLengthEncoding_Monoid_neutral_right
>     , test_RunLengthEncoding_Monoid_associative
>     ]

We have a left identity:

> prop_RunLengthEncoding_Monoid_neutral_left
>    , cprop_RunLengthEncoding_Monoid_neutral_left
>   :: forall a
>    . ( Eq a )
>   => Proxy a
>   -> RunLengthEncoding a
>   -> Property
> 
> prop_RunLengthEncoding_Monoid_neutral_left _ xs =
>   property $
>     xs == (mempty <> xs)
> 
> cprop_RunLengthEncoding_Monoid_neutral_left pa xs =
>   let zs = unRLE xs in
>   cover 50 (notEmptyFT zs) "xs not empty" $
>   cover 20 (depthFT zs > 2) "depth xs > 2" $
>   prop_RunLengthEncoding_Monoid_neutral_left pa xs
> 
> test_RunLengthEncoding_Monoid_neutral_left :: TestTree
> test_RunLengthEncoding_Monoid_neutral_left =
>   testGroup "xs == mempty <> xs"
>     [ testProperty "Char" $
>         cprop_RunLengthEncoding_Monoid_neutral_left pChar
>     , testProperty "Bool" $
>         cprop_RunLengthEncoding_Monoid_neutral_left pBool
>     ]

We have a right identity:

> prop_RunLengthEncoding_Monoid_neutral_right
>    , cprop_RunLengthEncoding_Monoid_neutral_right
>   :: forall a
>    . ( Eq a )
>   => Proxy a
>   -> RunLengthEncoding a
>   -> Property
> 
> prop_RunLengthEncoding_Monoid_neutral_right _ xs =
>   property $
>     xs == (xs <> mempty)
> 
> cprop_RunLengthEncoding_Monoid_neutral_right pa xs =
>   let zs = unRLE xs in
>   cover 50 (notEmptyFT zs) "xs not empty" $
>   cover 20 (depthFT zs > 2) "depth xs > 2" $
>   prop_RunLengthEncoding_Monoid_neutral_right pa xs
> 
> test_RunLengthEncoding_Monoid_neutral_right :: TestTree
> test_RunLengthEncoding_Monoid_neutral_right =
>   testGroup "xs == mempty <> xs"
>     [ testProperty "Char" $
>         cprop_RunLengthEncoding_Monoid_neutral_right pChar
>     , testProperty "Bool" $
>         cprop_RunLengthEncoding_Monoid_neutral_right pBool
>     ]

And concatenation of run length encodings is associative:

> prop_RunLengthEncoding_Monoid_associative
>    , cprop_RunLengthEncoding_Monoid_associative
>   :: forall a
>    . ( Eq a )
>   => Proxy a
>   -> RunLengthEncoding a
>   -> RunLengthEncoding a
>   -> RunLengthEncoding a
>   -> Property
> 
> prop_RunLengthEncoding_Monoid_associative _ a b c =
>   property $
>     (a <> (b <> c)) == ((a <> b) <> c)
> 
> cprop_RunLengthEncoding_Monoid_associative pa a b c =
>   let zs = unRLE a in
>   cover 50 (notEmptyFT zs) "xs not empty" $
>   cover 20 (depthFT zs > 2) "depth xs > 2" $
>   prop_RunLengthEncoding_Monoid_associative pa a b c
> 
> test_RunLengthEncoding_Monoid_associative :: TestTree
> test_RunLengthEncoding_Monoid_associative =
>   testGroup "a <> (b <> c) == (a <> b) <> c"
>     [ testProperty "Char" $
>         cprop_RunLengthEncoding_Monoid_associative pChar
>     , testProperty "Bool" $
>         cprop_RunLengthEncoding_Monoid_associative pBool
>     ]



Foldable laws for RunLengthEncoding
-----------------------------------

`RunLengthEncoding` has a `Foldable` instance, which we can validate.

> test_RunLengthEncoding_Foldable :: TestTree
> test_RunLengthEncoding_Foldable =
>   testGroup "Foldable laws"
>     [ test_RunLengthEncoding_Foldable_foldMap_id
>     , test_RunLengthEncoding_Foldable_fold_fmap
>     , test_RunLengthEncoding_Foldable_foldMap_fold
>     ]

`fold` is a `foldMap`:

> prop_RunLengthEncoding_Foldable_foldMap_id
>   :: forall a
>    . ( Eq a, Monoid a )
>   => Proxy a
>   -> RunLengthEncoding a
>   -> Property
> 
> prop_RunLengthEncoding_Foldable_foldMap_id _ xs =
>   property $
>     (fold xs)
>       == (foldMap id xs)
> 
> test_RunLengthEncoding_Foldable_foldMap_id :: TestTree
> test_RunLengthEncoding_Foldable_foldMap_id =
>   testGroup "fold == foldMap id"
>     [ testProperty "ZZ" $
>         prop_RunLengthEncoding_Foldable_foldMap_id pZZ
>     , testProperty "Tup" $
>         prop_RunLengthEncoding_Foldable_foldMap_id pTup
>     ]

`foldMap` decomposes as `fold` and `fmap`:

> prop_RunLengthEncoding_Foldable_fold_fmap
>   :: forall a b
>    . ( Eq b, Monoid b )
>   => Proxy a -> Proxy b
>   -> Fun a b -> RunLengthEncoding a
>   -> Property
> 
> prop_RunLengthEncoding_Foldable_fold_fmap _ _ f xs =
>   property $
>     (foldMap (applyFun f) xs)
>       == (fold $ fmap (applyFun f) xs)
> 
> test_RunLengthEncoding_Foldable_fold_fmap :: TestTree
> test_RunLengthEncoding_Foldable_fold_fmap =
>   testGroup "foldMap f == fold . fmap f"
>     [ testProperty "Char/ZZ" $
>         prop_RunLengthEncoding_Foldable_fold_fmap pChar pZZ
>     , testProperty "Char/Tup" $
>         prop_RunLengthEncoding_Foldable_fold_fmap pChar pTup
>     ]

And `foldMap` distributes over composition (sort of):

> prop_RunLengthEncoding_Foldable_foldMap_fold
>   :: forall a b c
>    . ( Eq c, Monoid c )
>   => Proxy a -> Proxy b -> Proxy c
>   -> Fun a b -> Fun b c -> RunLengthEncoding a
>   -> Property
> 
> prop_RunLengthEncoding_Foldable_foldMap_fold _ _ _ f g xs =
>   property $
>     (foldMap (applyFun g) $ fmap (applyFun f) xs)
>       == (foldMap (applyFun g . applyFun f) xs)
> 
> test_RunLengthEncoding_Foldable_foldMap_fold :: TestTree
> test_RunLengthEncoding_Foldable_foldMap_fold =
>   testGroup "foldMap f . fmap g == foldMap (f . g)"
>     [ testProperty "Char/Char/ZZ" $
>         prop_RunLengthEncoding_Foldable_foldMap_fold pChar pChar pZZ
>     , testProperty "Bool/Bool/Tup" $
>         prop_RunLengthEncoding_Foldable_foldMap_fold pBool pBool pTup
>     ]



RunLengthEncoding conversion
----------------------------

We should also verify that `fromFreqList` is a left inverse for `toFreqList`. The converse is not true, because for instance `toFreqList` will always combine adjacent runs on the same value.

> test_RunLengthEncoding_conversion :: TestTree
> test_RunLengthEncoding_conversion =
>   testGroup "Conversion"
>     [ test_RunLengthEncoding_fromFreqList_toFreqList
>     , test_RunLengthEncoding_fromFreqList_examples
>     ]

> prop_RunLengthEncoding_fromFreqList_toFreqList
>   :: forall a
>    . ( Eq a )
>   => Proxy a
>   -> RunLengthEncoding a
>   -> Property
> 
> prop_RunLengthEncoding_fromFreqList_toFreqList _ xs =
>   property $
>     xs == fromFreqList (toFreqList xs)
> 
> test_RunLengthEncoding_fromFreqList_toFreqList :: TestTree
> test_RunLengthEncoding_fromFreqList_toFreqList =
>   testGroup "fromFreqList . toFreqList == id"
>     [ testProperty "Char" $
>         prop_RunLengthEncoding_fromFreqList_toFreqList pChar
>     , testProperty "Bool" $
>         prop_RunLengthEncoding_fromFreqList_toFreqList pBool
>     ]

For fun we can also test some specific examples for `fromFreqList` to check our intuition.

> prop_RunLengthEncoding_fromFreqList_examples
>   :: forall a
>    . ( Eq a )
>   => Proxy a
>   -> [(Int, a)]
>   -> RunLengthEncoding a
>   -> Property
> 
> prop_RunLengthEncoding_fromFreqList_examples _ xs ys =
>   property $
>     ys == fromFreqList xs
> 
> test_RunLengthEncoding_fromFreqList_examples :: TestTree
> test_RunLengthEncoding_fromFreqList_examples =
>   testGroup "fromFreqList examples"
>     [ testCases
>       (uncurry $ prop_RunLengthEncoding_fromFreqList_examples pChar)
>       [ ( "all distinct"
>         , ( [(2, 'a'),(1, 'b'),(3, 'c')]
>           , fromFreqList [(2, 'a'), (1, 'b'), (3, 'c')]
>           )
>         )
> 
>       , ( "adjacent duplicates"
>         , ( [(2, 'a'), (3, 'a'), (1, 'b')]
>           , fromFreqList [(5, 'a'), (1, 'b')]
>           )
>         )
>       ]
>     ]



Test Suite
----------

> test_RunLengthEncoding :: TestTree
> test_RunLengthEncoding =
>   testGroup "RunLengthEncoding"
>     [ test_RunSize_Monoid
>     , test_Run_Functor
>     , test_RunLengthEncoding_Functor
>     , test_RunLengthEncoding_Monoid
>     , localOption (QuickCheckTests 100)
>         $ test_RunLengthEncoding_Foldable
>     , test_RunLengthEncoding_conversion
>     ]
