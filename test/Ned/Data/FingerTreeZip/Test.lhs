---
title: Ned.Data.FingerTreeZip.Test
---



Contents
--------

* [Introduction](#introduction)
* [Generators](#generators)
* [FingerTreeZip tests](#fingertreezip-tests)
    * [Conversion properties](#conversion-properties-for-fingertreezip)
    * [Init properties](#init-properties-for-fingertreezip)
    * [Last properties](#last-properties-for-fingertreezip)
    * [Head properties](#head-properties-for-fingertreezip)
    * [Commuting properties](#commuting-properties-for-fingertreezip)
    * [Split and integrate](#split-and-integrate)
    * [FTZ test suite](#ftz-test-suite)
* [Tape tests](#tape-tests)
    * [Conversion properties for Tape](#conversion-properties-for-tape)
    * [Init properties](#init-properties-for-tape)
    * [Last properties](#last-properties-for-tape)
    * [Head properties](#head-properties-for-tape)
    * [Commuting properties](#commuting-properties-for-tape)
    * [Tape test suite](#tape-test-suite)



Introduction
============

This test module is a little different. The `FingerTreeZip` module exports some "normal" code, which we can test in the usual way. But it also imports a large class, `Tape`, with no instances. The methods of `Tape` do satisfy several properties, and we'd like to avoid having to rewrite tests for these for each new instance. So in addition to a suite of concrete tests this module exports a parameterized suite of tests for instances of `Tape`. In general individual instances will have other properties that should be tested, but the parameterized family lets us avoid some duplication.

> {-# LANGUAGE
>     ScopedTypeVariables
>   , KindSignatures
> #-}
> 
> module Ned.Data.FingerTreeZip.Test (
>     test_FingerTreeZip
>   , test_Tape
> ) where
> 
> import Data.Proxy
> 
> import Test.QuickCheck
> import Test.Tasty
> import Test.Tasty.QuickCheck
> 
> import Ned.Data.FingerTree
> import Ned.Data.FingerTreeZip
> 
> import Ned.Data.FingerTree.Test



Generators
==========

> instance
>   ( Arbitrary a, Valued m a
>   ) => Arbitrary (FingerTreeZip m a)
>   where
>     arbitrary =
>       FingerTreeZip <$> arbitrary



FingerTreeZip tests
===================

The FingerTreeZip module exports a lot of functions. To help wrap our heads around properties these functions should satisfy, we'll organize them around interactions among specific subsets of the functions.

Conversion properties for FingerTreeZip
---------------------------------------

Testing functions which convert between zipped finger trees and lists:

> test_FingerTreeZip_conversion :: TestTree
> test_FingerTreeZip_conversion =
>   testGroup "Conversion properties"
>     [ test_FingerTreeZip_unTape_mkTape
>     , test_FingerTreeZip_mkTape_unTape
>     ]

`unTape` is a left inverse for `mkTape`:

> prop_FingerTreeZip_unTape_mkTape
>    , cprop_FingerTreeZip_unTape_mkTape
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> [a]
>   -> Property
> 
> prop_FingerTreeZip_unTape_mkTape _ _ xs =
>   property $
>     let z = mkTapeFTZ xs :: FingerTreeZip m a
>     in xs == unTapeFTZ z
> 
> cprop_FingerTreeZip_unTape_mkTape pa pm xs =
>   cover 1 (xs == []) "as empty" $
>   cover 30 (length xs > 10) "length xs > 10" $
>   prop_FingerTreeZip_unTape_mkTape pa pm xs
> 
> test_FingerTreeZip_unTape_mkTape :: TestTree
> test_FingerTreeZip_unTape_mkTape =
>   testGroup "unTape . mkTape == id"
>     [ testProperty "Char/Count" $
>         cprop_FingerTreeZip_unTape_mkTape pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTreeZip_unTape_mkTape pBool pTup
>     ]

`mkTape` is a left inverse for `unTape` (almost):

> prop_FingerTreeZip_mkTape_unTape
>    , cprop_FingerTreeZip_mkTape_unTape
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTreeZip m a
>   -> Property
> 
> prop_FingerTreeZip_mkTape_unTape _ _ xs =
>   property $
>     (initMoveFTZ xs)
>       == mkTapeFTZ (unTapeFTZ xs)
> 
> cprop_FingerTreeZip_mkTape_unTape pa pm xs =
>   cover 80 (not $ isEmptyFTZ xs) "as not empty" $
>   cover 30 (depthFTZ xs > 2) "depth xs > 2" $
>   prop_FingerTreeZip_mkTape_unTape pa pm xs
> 
> test_FingerTreeZip_mkTape_unTape :: TestTree
> test_FingerTreeZip_mkTape_unTape =
>   testGroup "mkTape . unTape == initMove"
>     [ testProperty "Char/Count" $
>         cprop_FingerTreeZip_mkTape_unTape pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTreeZip_mkTape_unTape pBool pTup
>     ]



Init properties for FingerTreeZip
---------------------------------

First we consider functions that interact with the beginning of a stream.

> test_FingerTreeZip_init :: TestTree
> test_FingerTreeZip_init =
>   testGroup "Init related properties"
>     [ test_FingerTreeZip_isAtInit_mkTapeFocus
>     , test_FingerTreeZip_initAlter
>     , test_FingerTreeZip_initAlter_examples
>     , test_FingerTreeZip_initRead_initInsert
>     , test_FingerTreeZip_initRead_examples
>     , test_FingerTreeZip_initDelete_initInsert
>     , test_FingerTreeZip_initDelete_examples
>     , test_FingerTreeZip_initMove_isAtInit
>     , test_FingerTreeZip_initMove_examples
>     , test_FingerTreeZip_initMove_idempotent
>     ]

For nonempty zipped finger trees, the `isAtInit` predicate is true if and only if the initial segment is empty.

> prop_FingerTreeZip_isAtInit_mkTapeFocus
>    , cprop_FingerTreeZip_isAtInit_mkTapeFocus
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> [a] -> a -> [a]
>   -> Property
> 
> prop_FingerTreeZip_isAtInit_mkTapeFocus _ _ as x bs =
>   let w = mkTapeFocusFTZ as x bs :: FingerTreeZip m a in
>   property $
>     (as == []) == (isAtInitFTZ w)
> 
> cprop_FingerTreeZip_isAtInit_mkTapeFocus pa pm as x bs =
>   cover 1 (as == []) "as empty" $
>   cover 1 (bs == []) "bs empty" $
>   prop_FingerTreeZip_isAtInit_mkTapeFocus pa pm as x bs
> 
> test_FingerTreeZip_isAtInit_mkTapeFocus :: TestTree
> test_FingerTreeZip_isAtInit_mkTapeFocus =
>   testGroup "isAtInit mkTapeFocus"
>     [ testProperty "Char/Count" $
>         cprop_FingerTreeZip_isAtInit_mkTapeFocus pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTreeZip_isAtInit_mkTapeFocus pBool pTup
> 
>     , testCases
>       (uncurry3 $ prop_FingerTreeZip_isAtInit_mkTapeFocus pChar pCount)
>       [ ( "as empty"
>         , ( [], 'a', ['b', 'c'] )
>         )
> 
>       , ( "bs empty"
>         , ( ['a', 'b'], 'c', [] )
>         )
> 
>       , ( "singleton"
>         , ( [], 'a', [] )
>         )
>       ]
>     ]

`initAlter` interacts predictably with `cons`.

> prop_FingerTreeZip_initAlter
>    , cprop_FingerTreeZip_initAlter
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> a -> FingerTreeZip m a
>   -> Property
> 
> prop_FingerTreeZip_initAlter _ _ u v xs =
>   let swop x = if x == u then v else x in
>   property $
>     (initInsertFTZ v xs)
>       == (initAlterFTZ swop $ initInsertFTZ u xs)
> 
> cprop_FingerTreeZip_initAlter pa pm u v xs =
>   cover 1 (isEmptyFTZ xs) "xs empty" $
>   cover 20 (depthFTZ xs > 2) "depth xs > 2" $
>   cover 40 (u /= v) "u /= v" $
>   prop_FingerTreeZip_initAlter pa pm u v xs
> 
> test_FingerTreeZip_initAlter :: TestTree
> test_FingerTreeZip_initAlter =
>   testGroup "initAlter cons"
>     [ testProperty "Char/Count" $
>         cprop_FingerTreeZip_initAlter pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTreeZip_initAlter pBool pTup
> 
>     , testCases
>       (uncurry3 $ prop_FingerTreeZip_initAlter pChar pCount)
>       [ ( "empty tail"
>         , ( 'a'
>           , 'b'
>           , emptyFTZ
>           )
>         )
>       ]
>     ]

We also check some concrete input/output pairs for `initAlter`.

> prop_FingerTreeZip_initAlter_examples
>   :: forall m a
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> (a -> a) -> FingerTreeZip m a
>   -> FingerTreeZip m a
>   -> Property
> 
> prop_FingerTreeZip_initAlter_examples _ _ f xs ys =
>   property $
>     ys == (initAlterFTZ f xs)
> 
> test_FingerTreeZip_initAlter_examples :: TestTree
> test_FingerTreeZip_initAlter_examples =
>   testGroup "initAlter examples"
>     [ testCases
>       (uncurry3 $ prop_FingerTreeZip_initAlter_examples pChar pCount)
>       [ ( "nonempty prefix"
>         , ( \c -> if c == 'a' then 'z' else c
>           , mkTapeFocusFTZ ['a', 'b'] 'c' ['d']
>           , mkTapeFocusFTZ ['z', 'b'] 'c' ['d']
>           )
>         )
> 
>       , ( "empty prefix"
>         , ( \c -> if c == 'a' then 'z' else c
>           , mkTapeFocusFTZ [] 'a' ['b', 'c', 'd']
>           , mkTapeFocusFTZ [] 'z' ['b', 'c', 'd']
>           )
>         )
> 
>       , ( "map fixes head"
>         , ( \c -> if c == 'b' then 'z' else c
>           , mkTapeFocusFTZ ['a', 'b'] 'c' ['d']
>           , mkTapeFocusFTZ ['a', 'b'] 'c' ['d']
>           )
>         )
> 
>       , ( "empty"
>         , ( undefined
>           , emptyFTZ
>           , emptyFTZ
>           )
>         )
>       ]
>     ]

`initRead` interacts predictably with `cons`.

> prop_FingerTreeZip_initRead_initInsert
>    , cprop_FingerTreeZip_initRead_initInsert
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> FingerTreeZip m a
>   -> Property
> 
> prop_FingerTreeZip_initRead_initInsert _ _ a xs =
>   property $
>     (Just a)
>       == (initReadFTZ (initInsertFTZ a xs))
> 
> cprop_FingerTreeZip_initRead_initInsert pa pm a xs =
>   cover 1 (isEmptyFTZ xs) "xs empty" $
>   cover 30 (depthFTZ xs > 2) "depth xs > 2" $
>   prop_FingerTreeZip_initRead_initInsert pa pm a xs
> 
> test_FingerTreeZip_initRead_initInsert :: TestTree
> test_FingerTreeZip_initRead_initInsert =
>   testGroup "initRead cons"
>     [ testProperty "Char/Count" $
>         cprop_FingerTreeZip_initRead_initInsert pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTreeZip_initRead_initInsert pBool pTup
> 
>     , testCases
>       (uncurry $ prop_FingerTreeZip_initRead_initInsert pChar pCount)
>       [ ( "empty tail"
>         , ( 'a', emptyFTZ )
>         )
>       ]
>     ]

Some concrete examples for `initRead`:

> prop_FingerTreeZip_initRead_examples
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTreeZip m a -> Maybe a
>   -> Property
> 
> prop_FingerTreeZip_initRead_examples _ _ xs z =
>   property $
>     z == initReadFTZ xs
> 
> test_FingerTreeZip_initRead_examples :: TestTree
> test_FingerTreeZip_initRead_examples =
>   testGroup "initRead examples"
>     [ testCases
>       (uncurry $ prop_FingerTreeZip_initRead_examples pChar pCount)
>       [ ( "empty"
>         , ( emptyFTZ
>           , Nothing
>           )
>         )
> 
>       , ( "not empty, not at head"
>         , ( mkTapeFocusFTZ ['a'] 'b' ['c', 'd']
>           , Just 'a'
>           )
>         )
> 
>       , ( "not empty, at head"
>         , ( mkTapeFocusFTZ [] 'a' ['b', 'c', 'd']
>           , Just 'a'
>           )
>         )
>       ]
>     ]

`initDelete` interacts predictably with `initInsert`.

> prop_FingerTreeZip_initDelete_initInsert
>    , cprop_FingerTreeZip_initDelete_initInsert
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> FingerTreeZip m a
>   -> Property
> 
> prop_FingerTreeZip_initDelete_initInsert _ _ a xs =
>   property $
>     xs == (initDeleteFTZ $ initInsertFTZ a xs)
> 
> cprop_FingerTreeZip_initDelete_initInsert pa pm a xs =
>   cover 1 (isEmptyFTZ xs) "xs empty" $
>   cover 30 (depthFTZ xs > 2) "depth xs > 2" $
>   prop_FingerTreeZip_initDelete_initInsert pa pm a xs
> 
> test_FingerTreeZip_initDelete_initInsert :: TestTree
> test_FingerTreeZip_initDelete_initInsert =
>   testGroup "initDelete cons"
>     [ testProperty "Char/Count" $
>         cprop_FingerTreeZip_initDelete_initInsert pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTreeZip_initDelete_initInsert pBool pTup
>     ]

And some examples for `initDelete`:

> prop_FingerTreeZip_initDelete_examples
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTreeZip m a -> FingerTreeZip m a
>   -> Property
> 
> prop_FingerTreeZip_initDelete_examples _ _ xs ys =
>   property $
>     ys == (initDeleteFTZ xs)
> 
> test_FingerTreeZip_initDelete_examples :: TestTree
> test_FingerTreeZip_initDelete_examples =
>   testGroup "initDelete examples"
>     [ testCases
>       (uncurry $ prop_FingerTreeZip_initDelete_examples pChar pCount)
>       [ ( "empty"
>         , ( emptyFTZ
>           , emptyFTZ
>           )
>         )
> 
>       , ( "not empty, not at init (1)"
>         , ( mkTapeFocusFTZ ['a'] 'b' ['c', 'd']
>           , mkTapeFocusFTZ [] 'b' ['c', 'd']
>           )
>         )
> 
>       , ( "not empty, not at init (2)"
>         , ( mkTapeFocusFTZ ['a', 'b'] 'c' ['d', 'e']
>           , mkTapeFocusFTZ ['b'] 'c' ['d', 'e']
>           )
>         )
> 
>       , ( "not empty, at init"
>         , ( mkTapeFocusFTZ [] 'a' ['b', 'c', 'd', 'e']
>           , mkTapeFocusFTZ [] 'b' ['c', 'd', 'e']
>           )
>         )
> 
>       , ( "singleton"
>         , ( mkTapeFocusFTZ [] 'a' []
>           , emptyFTZ
>           )
>         )
>       ]
>     ]

After moving to the beginning of the stream, we are at the beginning of the stream.

> prop_FingerTreeZip_initMove_isAtInit
>    , cprop_FingerTreeZip_initMove_isAtInit
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTreeZip m a
>   -> Property
> 
> prop_FingerTreeZip_initMove_isAtInit _ _ xs =
>   property $
>     isAtInitFTZ (initMoveFTZ xs)
> 
> cprop_FingerTreeZip_initMove_isAtInit pa pm xs =
>   cover 30 (not $ isAtInitFTZ xs) "not at init" $
>   cover 20 (depthFTZ xs > 2) "depth xs > 2" $
>   prop_FingerTreeZip_initMove_isAtInit pa pm xs
> 
> test_FingerTreeZip_initMove_isAtInit :: TestTree
> test_FingerTreeZip_initMove_isAtInit =
>   testGroup "initMove isAtInit"
>     [ testProperty "Char/Count" $
>         cprop_FingerTreeZip_initMove_isAtInit pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTreeZip_initMove_isAtInit pBool pTup
>     ]

We can also test `initMove` on some specific cases.

> prop_FingerTreeZip_initMove_examples
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTreeZip m a
>   -> FingerTreeZip m a
>   -> Property
> 
> prop_FingerTreeZip_initMove_examples _ _ xs ys =
>   property $
>     ys == (initMoveFTZ xs)
> 
> test_FingerTreeZip_initMove_examples :: TestTree
> test_FingerTreeZip_initMove_examples =
>   testGroup "initMove examples"
>     [ testCases
>       (uncurry $ prop_FingerTreeZip_initMove_examples pChar pCount)
>       [ ( "empty"
>         , ( mkTapeFTZ []
>           , mkTapeFTZ []
>           )
>         )
> 
>       , ( "not empty, not at front"
>         , ( mkTapeFocusFTZ ['a', 'b'] 'c' ['d']
>           , mkTapeFocusFTZ [] 'a' ['b', 'c', 'd']
>           )
>         )
> 
>       , ( "not empty, at front"
>         , ( mkTapeFocusFTZ [] 'a' ['b', 'c', 'd']
>           , mkTapeFocusFTZ [] 'a' ['b', 'c', 'd']
>           )
>         )
>       ]
>     ]

`initMove` is idempotent:

> prop_FingerTreeZip_initMove_idempotent
>    , cprop_FingerTreeZip_initMove_idempotent
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTreeZip m a
>   -> Property
> 
> prop_FingerTreeZip_initMove_idempotent _ _ xs =
>   property $
>     (initMoveFTZ xs)
>       == (initMoveFTZ $ initMoveFTZ xs)
> 
> cprop_FingerTreeZip_initMove_idempotent pa pm xs =
>   cover 30 (not $ isAtInitFTZ xs) "not at init" $
>   cover 20 (depthFTZ xs > 2) "depth xs > 2" $
>   cover 40 (not $ isEmptyFTZ xs) "xs not empty" $
>   prop_FingerTreeZip_initMove_idempotent pa pm xs
> 
> test_FingerTreeZip_initMove_idempotent :: TestTree
> test_FingerTreeZip_initMove_idempotent =
>   testGroup "initMove is idempotent"
>     [ testProperty "Char/Count" $
>         cprop_FingerTreeZip_initMove_idempotent pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTreeZip_initMove_idempotent pBool pTup
>     ]



Last properties for FingerTreeZip
---------------------------------

Next we consider functions that interact with the end of a stream.

> test_FingerTreeZip_last :: TestTree
> test_FingerTreeZip_last =
>   testGroup "Last related properties"
>     [ test_FingerTreeZip_lastAlter
>     , test_FingerTreeZip_lastAlter_examples
>     , test_FingerTreeZip_lastRead_lastInsert
>     , test_FingerTreeZip_lastRead_examples
>     , test_FingerTreeZip_lastDelete_lastInsert
>     , test_FingerTreeZip_lastDelete_examples
>     , test_FingerTreeZip_lastMove_isAtLast
>     , test_FingerTreeZip_lastMove_examples
>     , test_FingerTreeZip_lastMove_idempotent
>     ]

`lastAlter` interacts predictably with `cons`.

> prop_FingerTreeZip_lastAlter
>    , cprop_FingerTreeZip_lastAlter
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> a -> FingerTreeZip m a
>   -> Property
> 
> prop_FingerTreeZip_lastAlter _ _ u v xs =
>   let swop x = if x == u then v else x in
>   property $
>     (lastInsertFTZ v xs)
>       == (lastAlterFTZ swop $ lastInsertFTZ u xs)
> 
> cprop_FingerTreeZip_lastAlter pa pm u v xs =
>   cover 1 (isEmptyFTZ xs) "xs empty" $
>   cover 20 (depthFTZ xs > 2) "depth xs > 2" $
>   cover 40 (u /= v) "u /= v" $
>   prop_FingerTreeZip_lastAlter pa pm u v xs
> 
> test_FingerTreeZip_lastAlter :: TestTree
> test_FingerTreeZip_lastAlter =
>   testGroup "lastAlter cons"
>     [ testProperty "Char/Count" $
>         cprop_FingerTreeZip_lastAlter pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTreeZip_lastAlter pBool pTup
> 
>     , testCases
>       (uncurry3 $ prop_FingerTreeZip_lastAlter pChar pCount)
>       [ ( "empty tail"
>         , ( 'a'
>           , 'b'
>           , emptyFTZ
>           )
>         )
>       ]
>     ]

We also check some concrete input/output pairs for `lastAlter`.

> prop_FingerTreeZip_lastAlter_examples
>   :: forall m a
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> (a -> a) -> FingerTreeZip m a
>   -> FingerTreeZip m a
>   -> Property
> 
> prop_FingerTreeZip_lastAlter_examples _ _ f xs ys =
>   property $
>     ys == (lastAlterFTZ f xs)
> 
> test_FingerTreeZip_lastAlter_examples :: TestTree
> test_FingerTreeZip_lastAlter_examples =
>   testGroup "lastAlter examples"
>     [ testCases
>       (uncurry3 $ prop_FingerTreeZip_lastAlter_examples pChar pCount)
>       [ ( "nonempty prefix"
>         , ( \c -> if c == 'd' then 'z' else c
>           , mkTapeFocusFTZ ['a', 'b'] 'c' ['d']
>           , mkTapeFocusFTZ ['a', 'b'] 'c' ['z']
>           )
>         )
> 
>       , ( "empty prefix"
>         , ( \c -> if c == 'd' then 'z' else c
>           , mkTapeFocusFTZ [] 'a' ['b', 'c', 'd']
>           , mkTapeFocusFTZ [] 'a' ['b', 'c', 'z']
>           )
>         )
> 
>       , ( "map fixes last"
>         , ( \c -> if c == 'b' then 'z' else c
>           , mkTapeFocusFTZ ['a', 'b'] 'c' ['d']
>           , mkTapeFocusFTZ ['a', 'b'] 'c' ['d']
>           )
>         )
> 
>       , ( "empty"
>         , ( undefined
>           , emptyFTZ
>           , emptyFTZ
>           )
>         )
>       ]
>     ]

`lastRead` interacts predictably with `cons`.

> prop_FingerTreeZip_lastRead_lastInsert
>    , cprop_FingerTreeZip_lastRead_lastInsert
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> FingerTreeZip m a
>   -> Property
> 
> prop_FingerTreeZip_lastRead_lastInsert _ _ a xs =
>   property $
>     (Just a)
>       == (lastReadFTZ (lastInsertFTZ a xs))
> 
> cprop_FingerTreeZip_lastRead_lastInsert pa pm a xs =
>   cover 1 (isEmptyFTZ xs) "xs empty" $
>   cover 30 (depthFTZ xs > 2) "depth xs > 2" $
>   prop_FingerTreeZip_lastRead_lastInsert pa pm a xs
> 
> test_FingerTreeZip_lastRead_lastInsert :: TestTree
> test_FingerTreeZip_lastRead_lastInsert =
>   testGroup "lastRead cons"
>     [ testProperty "Char/Count" $
>         cprop_FingerTreeZip_lastRead_lastInsert pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTreeZip_lastRead_lastInsert pBool pTup
> 
>     , testCases
>       (uncurry $ prop_FingerTreeZip_lastRead_lastInsert pChar pCount)
>       [ ( "empty tail"
>         , ( 'a', emptyFTZ )
>         )
>       ]
>     ]

Some concrete examples for `lastRead`:

> prop_FingerTreeZip_lastRead_examples
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTreeZip m a -> Maybe a
>   -> Property
> 
> prop_FingerTreeZip_lastRead_examples _ _ xs z =
>   property $
>     z == lastReadFTZ xs
> 
> test_FingerTreeZip_lastRead_examples :: TestTree
> test_FingerTreeZip_lastRead_examples =
>   testGroup "lastRead examples"
>     [ testCases
>       (uncurry $ prop_FingerTreeZip_lastRead_examples pChar pCount)
>       [ ( "empty"
>         , ( emptyFTZ
>           , Nothing
>           )
>         )
> 
>       , ( "not empty, not at end"
>         , ( mkTapeFocusFTZ ['a'] 'b' ['c', 'd']
>           , Just 'd'
>           )
>         )
> 
>       , ( "not empty, at end"
>         , ( mkTapeFocusFTZ ['a', 'b', 'c'] 'd' []
>           , Just 'd'
>           )
>         )
>       ]
>     ]

`lastDelete` interacts predictably with `lastInsert`.

> prop_FingerTreeZip_lastDelete_lastInsert
>    , cprop_FingerTreeZip_lastDelete_lastInsert
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> FingerTreeZip m a
>   -> Property
> 
> prop_FingerTreeZip_lastDelete_lastInsert _ _ a xs =
>   property $
>     xs == (lastDeleteFTZ $ lastInsertFTZ a xs)
> 
> cprop_FingerTreeZip_lastDelete_lastInsert pa pm a xs =
>   cover 1 (isEmptyFTZ xs) "xs empty" $
>   cover 30 (depthFTZ xs > 2) "depth xs > 2" $
>   prop_FingerTreeZip_lastDelete_lastInsert pa pm a xs
> 
> test_FingerTreeZip_lastDelete_lastInsert :: TestTree
> test_FingerTreeZip_lastDelete_lastInsert =
>   testGroup "lastDelete cons"
>     [ testProperty "Char/Count" $
>         cprop_FingerTreeZip_lastDelete_lastInsert pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTreeZip_lastDelete_lastInsert pBool pTup
>     ]

And some examples for `lastDelete`:

> prop_FingerTreeZip_lastDelete_examples
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTreeZip m a -> FingerTreeZip m a
>   -> Property
> 
> prop_FingerTreeZip_lastDelete_examples _ _ xs ys =
>   property $
>     ys == (lastDeleteFTZ xs)
> 
> test_FingerTreeZip_lastDelete_examples :: TestTree
> test_FingerTreeZip_lastDelete_examples =
>   testGroup "lastDelete examples"
>     [ testCases
>       (uncurry $ prop_FingerTreeZip_lastDelete_examples pChar pCount)
>       [ ( "empty"
>         , ( emptyFTZ
>           , emptyFTZ
>           )
>         )
> 
>       , ( "not empty, not at last (1)"
>         , ( mkTapeFocusFTZ ['a'] 'b' ['c', 'd']
>           , mkTapeFocusFTZ ['a'] 'b' ['c']
>           )
>         )
> 
>       , ( "not empty, not at last (2)"
>         , ( mkTapeFocusFTZ ['a', 'b'] 'c' ['d', 'e']
>           , mkTapeFocusFTZ ['a', 'b'] 'c' ['d']
>           )
>         )
> 
>       , ( "not empty, at last"
>         , ( mkTapeFocusFTZ ['a', 'b', 'c'] 'd' []
>           , mkTapeFocusFTZ ['a', 'b'] 'c' []
>           )
>         )
> 
>       , ( "singleton"
>         , ( mkTapeFocusFTZ [] 'a' []
>           , emptyFTZ
>           )
>         )
>       ]
>     ]

After moving to the beginning of the stream, we are at the beginning of the stream.

> prop_FingerTreeZip_lastMove_isAtLast
>    , cprop_FingerTreeZip_lastMove_isAtLast
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTreeZip m a
>   -> Property
> 
> prop_FingerTreeZip_lastMove_isAtLast _ _ xs =
>   property $
>     isAtLastFTZ (lastMoveFTZ xs)
> 
> cprop_FingerTreeZip_lastMove_isAtLast pa pm xs =
>   cover 30 (not $ isAtLastFTZ xs) "not at last" $
>   cover 20 (depthFTZ xs > 2) "depth xs > 2" $
>   prop_FingerTreeZip_lastMove_isAtLast pa pm xs
> 
> test_FingerTreeZip_lastMove_isAtLast :: TestTree
> test_FingerTreeZip_lastMove_isAtLast =
>   testGroup "lastMove isAtLast"
>     [ testProperty "Char/Count" $
>         cprop_FingerTreeZip_lastMove_isAtLast pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTreeZip_lastMove_isAtLast pBool pTup
>     ]

We can also test `lastMove` on some specific cases.

> prop_FingerTreeZip_lastMove_examples
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTreeZip m a
>   -> FingerTreeZip m a
>   -> Property
> 
> prop_FingerTreeZip_lastMove_examples _ _ xs ys =
>   property $
>     ys == (lastMoveFTZ xs)
> 
> test_FingerTreeZip_lastMove_examples :: TestTree
> test_FingerTreeZip_lastMove_examples =
>   testGroup "lastMove examples"
>     [ testCases
>       (uncurry $ prop_FingerTreeZip_lastMove_examples pChar pCount)
>       [ ( "empty"
>         , ( mkTapeFTZ []
>           , mkTapeFTZ []
>           )
>         )
> 
>       , ( "not empty, not at front"
>         , ( mkTapeFocusFTZ ['a', 'b'] 'c' ['d']
>           , mkTapeFocusFTZ ['a', 'b', 'c'] 'd' []
>           )
>         )
> 
>       , ( "not empty, at end"
>         , ( mkTapeFocusFTZ ['a', 'b', 'c'] 'd' []
>           , mkTapeFocusFTZ ['a', 'b', 'c'] 'd' []
>           )
>         )
>       ]
>     ]

`lastMove` is idempotent:

> prop_FingerTreeZip_lastMove_idempotent
>    , cprop_FingerTreeZip_lastMove_idempotent
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTreeZip m a
>   -> Property
> 
> prop_FingerTreeZip_lastMove_idempotent _ _ xs =
>   property $
>     (lastMoveFTZ xs)
>       == (lastMoveFTZ $ lastMoveFTZ xs)
> 
> cprop_FingerTreeZip_lastMove_idempotent pa pm xs =
>   cover 30 (not $ isAtLastFTZ xs) "not at last" $
>   cover 20 (depthFTZ xs > 2) "depth xs > 2" $
>   cover 40 (not $ isEmptyFTZ xs) "xs not empty" $
>   prop_FingerTreeZip_lastMove_idempotent pa pm xs
> 
> test_FingerTreeZip_lastMove_idempotent :: TestTree
> test_FingerTreeZip_lastMove_idempotent =
>   testGroup "lastMove is idempotent"
>     [ testProperty "Char/Count" $
>         cprop_FingerTreeZip_lastMove_idempotent pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTreeZip_lastMove_idempotent pBool pTup
>     ]



Head properties for FingerTreeZip
---------------------------------

Now test for functions which manipulate the read head.

> test_FingerTreeZip_head :: TestTree
> test_FingerTreeZip_head =
>   testGroup "Head related properties"
>     [ test_FingerTreeZip_headMoveR_headMoveL
>     , test_FingerTreeZip_headMoveL_headMoveR
>     , test_FingerTreeZip_headMoveR_examples
>     , test_FingerTreeZip_headMoveL_examples
>     , test_FingerTreeZip_headDeleteL_headInsertL
>     , test_FingerTreeZip_headDeleteR_headInsertR
>     , test_FingerTreeZip_headDeleteL_examples
>     , test_FingerTreeZip_headDeleteR_examples
>     , test_FingerTreeZip_headInsertL_examples
>     , test_FingerTreeZip_headInsertR_examples
>     ]

`headMoveR` is a left inverse of `headMoveL` (almost):

> prop_FingerTreeZip_headMoveR_headMoveL
>    , cprop_FingerTreeZip_headMoveR_headMoveL
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTreeZip m a
>   -> Property
> 
> prop_FingerTreeZip_headMoveR_headMoveL _ _ xs =
>   property $
>     if isAtInitFTZ xs
>       then True
>       else xs == headMoveRFTZ (headMoveLFTZ xs)
> 
> cprop_FingerTreeZip_headMoveR_headMoveL pa pm xs =
>   cover 30 (not $ isAtInitFTZ xs) "xs not at init" $
>   cover 30 (depthFTZ xs > 2) "depth xs > 2" $
>   prop_FingerTreeZip_headMoveR_headMoveL pa pm xs
> 
> test_FingerTreeZip_headMoveR_headMoveL :: TestTree
> test_FingerTreeZip_headMoveR_headMoveL =
>   testGroup "headMoveR . headMoveL == id"
>     [ testProperty "Char/Count" $
>         cprop_FingerTreeZip_headMoveR_headMoveL pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTreeZip_headMoveR_headMoveL pBool pTup
>     ]

`headMoveL` is a left inverse of `headMoveR` (almost):

> prop_FingerTreeZip_headMoveL_headMoveR
>    , cprop_FingerTreeZip_headMoveL_headMoveR
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTreeZip m a
>   -> Property
> 
> prop_FingerTreeZip_headMoveL_headMoveR _ _ xs =
>   property $
>     if isAtLastFTZ xs
>       then True
>       else xs == headMoveLFTZ (headMoveRFTZ xs)
> 
> cprop_FingerTreeZip_headMoveL_headMoveR pa pm xs =
>   cover 30 (not $ isAtInitFTZ xs) "xs not at init" $
>   cover 30 (depthFTZ xs > 2) "depth xs > 2" $
>   prop_FingerTreeZip_headMoveL_headMoveR pa pm xs
> 
> test_FingerTreeZip_headMoveL_headMoveR :: TestTree
> test_FingerTreeZip_headMoveL_headMoveR =
>   testGroup "headMoveL . headMoveR == id"
>     [ testProperty "Char/Count" $
>         cprop_FingerTreeZip_headMoveL_headMoveR pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTreeZip_headMoveL_headMoveR pBool pTup
>     ]

`headMoveR` examples:

> prop_FingerTreeZip_headMoveR_examples
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTreeZip m a
>   -> FingerTreeZip m a
>   -> Property
> 
> prop_FingerTreeZip_headMoveR_examples _ _ xs ys =
>   property $
>     ys == headMoveRFTZ xs
> 
> test_FingerTreeZip_headMoveR_examples :: TestTree
> test_FingerTreeZip_headMoveR_examples =
>   testGroup "headMoveR examples"
>     [ testCases
>       (uncurry $ prop_FingerTreeZip_headMoveR_examples pChar pCount)
>       [ ( "in middle"
>         , ( mkTapeFocusFTZ ['a'] 'b' ['c', 'd']
>           , mkTapeFocusFTZ ['a', 'b'] 'c' ['d']
>           )
>         )
> 
>       , ( "at last"
>         , ( mkTapeFocusFTZ ['a', 'b'] 'c' []
>           , mkTapeFocusFTZ ['a', 'b'] 'c' []
>           )
>         )
>       ]
>     ]

`headMoveL` examples:

> prop_FingerTreeZip_headMoveL_examples
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTreeZip m a
>   -> FingerTreeZip m a
>   -> Property
> 
> prop_FingerTreeZip_headMoveL_examples _ _ xs ys =
>   property $
>     ys == headMoveLFTZ xs
> 
> test_FingerTreeZip_headMoveL_examples :: TestTree
> test_FingerTreeZip_headMoveL_examples =
>   testGroup "headMoveL examples"
>     [ testCases
>       (uncurry $ prop_FingerTreeZip_headMoveL_examples pChar pCount)
>       [ ( "in middle"
>         , ( mkTapeFocusFTZ ['a', 'b'] 'c' ['d']
>           , mkTapeFocusFTZ ['a'] 'b' ['c', 'd']
>           )
>         )
> 
>       , ( "at last"
>         , ( mkTapeFocusFTZ [] 'a' ['b', 'c']
>           , mkTapeFocusFTZ [] 'a' ['b', 'c']
>           )
>         )
>       ]
>     ]

`headDeleteL` is a left inverse of `headInsertL`:

> prop_FingerTreeZip_headDeleteL_headInsertL
>    , cprop_FingerTreeZip_headDeleteL_headInsertL
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> FingerTreeZip m a
>   -> Property
> 
> prop_FingerTreeZip_headDeleteL_headInsertL _ _ a xs =
>   property $
>     xs == headDeleteLFTZ (headInsertLFTZ a xs)
> 
> cprop_FingerTreeZip_headDeleteL_headInsertL pa pm a xs =
>   cover 50 (not $ isEmptyFTZ xs) "xs nonempty" $
>   cover 30 (depthFTZ xs > 2) "depth xs > 2" $
>   prop_FingerTreeZip_headDeleteL_headInsertL pa pm a xs
> 
> test_FingerTreeZip_headDeleteL_headInsertL :: TestTree
> test_FingerTreeZip_headDeleteL_headInsertL =
>   testGroup "headDeleteL . headInsertL a == id"
>     [ testProperty "Char/Count" $
>         cprop_FingerTreeZip_headDeleteL_headInsertL pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTreeZip_headDeleteL_headInsertL pBool pTup
>     ]

`headDeleteR` is a left inverse of `headInsertR`:

> prop_FingerTreeZip_headDeleteR_headInsertR
>    , cprop_FingerTreeZip_headDeleteR_headInsertR
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> FingerTreeZip m a
>   -> Property
> 
> prop_FingerTreeZip_headDeleteR_headInsertR _ _ a xs =
>   property $
>     xs == headDeleteRFTZ (headInsertRFTZ a xs)
> 
> cprop_FingerTreeZip_headDeleteR_headInsertR pa pm a xs =
>   cover 50 (not $ isEmptyFTZ xs) "xs nonempty" $
>   cover 30 (depthFTZ xs > 2) "depth xs > 2" $
>   prop_FingerTreeZip_headDeleteR_headInsertR pa pm a xs
> 
> test_FingerTreeZip_headDeleteR_headInsertR :: TestTree
> test_FingerTreeZip_headDeleteR_headInsertR =
>   testGroup "headDeleteR . headInsertR a == id"
>     [ testProperty "Char/Count" $
>         cprop_FingerTreeZip_headDeleteR_headInsertR pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTreeZip_headDeleteR_headInsertR pBool pTup
>     ]

Examples of `headDeleteL`:

> prop_FingerTreeZip_headDeleteL_examples
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTreeZip m a
>   -> FingerTreeZip m a
>   -> Property
> 
> prop_FingerTreeZip_headDeleteL_examples _ _ xs ys =
>   property $
>     ys == headDeleteLFTZ xs
> 
> test_FingerTreeZip_headDeleteL_examples :: TestTree
> test_FingerTreeZip_headDeleteL_examples =
>   testGroup "headDeleteL examples"
>     [ testCases
>       (uncurry $ prop_FingerTreeZip_headDeleteL_examples pChar pCount)
>       [ ( "inner"
>         , ( mkTapeFocusFTZ ['a', 'b'] 'c' ['d', 'e']
>           , mkTapeFocusFTZ ['a'] 'c' ['d', 'e']
>           )
>         )
> 
>       , ( "at init"
>         , ( mkTapeFocusFTZ [] 'a' ['b', 'c', 'd', 'e']
>           , mkTapeFocusFTZ [] 'b' ['c', 'd', 'e']
>           )
>         )
>       ]
>     ]

Examples of `headDeleteR`:

> prop_FingerTreeZip_headDeleteR_examples
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTreeZip m a
>   -> FingerTreeZip m a
>   -> Property
> 
> prop_FingerTreeZip_headDeleteR_examples _ _ xs ys =
>   property $
>     ys == headDeleteRFTZ xs
> 
> test_FingerTreeZip_headDeleteR_examples :: TestTree
> test_FingerTreeZip_headDeleteR_examples =
>   testGroup "headDeleteR examples"
>     [ testCases
>       (uncurry $ prop_FingerTreeZip_headDeleteR_examples pChar pCount)
>       [ ( "inner"
>         , ( mkTapeFocusFTZ ['a', 'b'] 'c' ['d', 'e']
>           , mkTapeFocusFTZ ['a', 'b'] 'c' ['e']
>           )
>         )
> 
>       , ( "at last"
>         , ( mkTapeFocusFTZ ['a', 'b', 'c', 'd'] 'e' []
>           , mkTapeFocusFTZ ['a', 'b', 'c'] 'd' []
>           )
>         )
>       ]
>     ]

Examples of `headInsertL`:

> prop_FingerTreeZip_headInsertL_examples
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> FingerTreeZip m a
>   -> FingerTreeZip m a
>   -> Property
> 
> prop_FingerTreeZip_headInsertL_examples _ _ a xs ys =
>   property $
>     ys == (headInsertLFTZ a xs)
> 
> test_FingerTreeZip_headInsertL_examples :: TestTree
> test_FingerTreeZip_headInsertL_examples =
>   testGroup "headInsertL examples"
>     [ testCases
>       (uncurry3 $ prop_FingerTreeZip_headInsertL_examples pChar pCount)
>       [ ( "middle"
>         , ( 'z'
>           , mkTapeFocusFTZ ['a'] 'b' ['c']
>           , mkTapeFocusFTZ ['a', 'z'] 'b' ['c']
>           )
>         )
> 
>       , ( "init"
>         , ( 'z'
>           , mkTapeFocusFTZ [] 'a' ['b', 'c']
>           , mkTapeFocusFTZ ['z'] 'a' ['b', 'c']
>           )
>         )
> 
>       , ( "last"
>         , ( 'z'
>           , mkTapeFocusFTZ ['a', 'b'] 'c' []
>           , mkTapeFocusFTZ ['a', 'b', 'z'] 'c' []
>           )
>         )
>       ]
>     ]

Examples of `headInsertR`:

> prop_FingerTreeZip_headInsertR_examples
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> FingerTreeZip m a
>   -> FingerTreeZip m a
>   -> Property
> 
> prop_FingerTreeZip_headInsertR_examples _ _ a xs ys =
>   property $
>     ys == (headInsertRFTZ a xs)
> 
> test_FingerTreeZip_headInsertR_examples :: TestTree
> test_FingerTreeZip_headInsertR_examples =
>   testGroup "headInsertR examples"
>     [ testCases
>       (uncurry3 $ prop_FingerTreeZip_headInsertR_examples pChar pCount)
>       [ ( "middle"
>         , ( 'z'
>           , mkTapeFocusFTZ ['a'] 'b' ['c']
>           , mkTapeFocusFTZ ['a'] 'b' ['z', 'c']
>           )
>         )
> 
>       , ( "init"
>         , ( 'z'
>           , mkTapeFocusFTZ [] 'a' ['b', 'c']
>           , mkTapeFocusFTZ [] 'a' ['z', 'b', 'c']
>           )
>         )
> 
>       , ( "last"
>         , ( 'z'
>           , mkTapeFocusFTZ ['a', 'b'] 'c' []
>           , mkTapeFocusFTZ ['a', 'b'] 'c' ['z']
>           )
>         )
>       ]
>     ]



Commuting actions for FingerTreeZip
-----------------------------------

Some pairs of actions commute with each other; these make good tests.

> test_FingerTreeZip_commuting_actions :: TestTree
> test_FingerTreeZip_commuting_actions =
>   testGroup "Commuting Actions"
>     [ test_FingerTreeZip_initInsert_lastInsert
>     , test_FingerTreeZip_initDelete_lastDelete
>     ]

`initInsert` and `lastInsert` commute:

> prop_FingerTreeZip_initInsert_lastInsert
>    , cprop_FingerTreeZip_initInsert_lastInsert
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> a -> FingerTreeZip m a
>   -> Property
> 
> prop_FingerTreeZip_initInsert_lastInsert _ _ u v xs =
>   property $
>     if isEmptyFTZ xs
>       then True
>       else (initInsertFTZ u $ lastInsertFTZ v xs)
>             == (lastInsertFTZ v $ initInsertFTZ u xs)
> 
> cprop_FingerTreeZip_initInsert_lastInsert pa pm u v xs =
>   cover 50 (not $ isEmptyFTZ xs) "xs nonempty" $
>   cover 30 (depthFTZ xs > 2) "depth xs > 2" $
>   prop_FingerTreeZip_initInsert_lastInsert pa pm u v xs
> 
> test_FingerTreeZip_initInsert_lastInsert :: TestTree
> test_FingerTreeZip_initInsert_lastInsert =
>   testGroup "initInsert and lastInsert commute"
>     [ testProperty "Char/Count" $
>         cprop_FingerTreeZip_initInsert_lastInsert pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTreeZip_initInsert_lastInsert pBool pTup
>     ]

`initDelete` and `lastDelete` commute:

> prop_FingerTreeZip_initDelete_lastDelete
>    , cprop_FingerTreeZip_initDelete_lastDelete
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> a -> FingerTreeZip m a
>   -> Property
> 
> prop_FingerTreeZip_initDelete_lastDelete _ _ u v xs =
>   property $
>     (initDeleteFTZ $ lastDeleteFTZ xs)
>       == (lastDeleteFTZ $ initDeleteFTZ xs)
> 
> cprop_FingerTreeZip_initDelete_lastDelete pa pm u v xs =
>   cover 50 (not $ isEmptyFTZ xs) "xs nonempty" $
>   cover 30 (depthFTZ xs > 2) "depth xs > 2" $
>   prop_FingerTreeZip_initDelete_lastDelete pa pm u v xs
> 
> test_FingerTreeZip_initDelete_lastDelete :: TestTree
> test_FingerTreeZip_initDelete_lastDelete =
>   testGroup "initDelete and lastDelete commute"
>     [ testProperty "Char/Count" $
>         cprop_FingerTreeZip_initDelete_lastDelete pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTreeZip_initDelete_lastDelete pBool pTup
>     ]



Split and integrate
-------------------

> prop_FingerTreeZip_split_integrate
>    , cprop_FingerTreeZip_split_integrate
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> Fun m Bool -> FingerTree m a
>   -> Property
> 
> prop_FingerTreeZip_split_integrate _ _ p xs =
>   (applyFun p mempty == False) ==>
>   (applyFun p (value xs) == True) ==>
>   xs == integrateFTZ (splitFTZ (applyFun p) xs)
> 
> cprop_FingerTreeZip_split_integrate pa pm p xs =
>   cover 30 (depthFT xs > 2) "depth xs > 2" $
>   cover 20 (depthFT xs <= 2) "depth xs <= 2" $
>   prop_FingerTreeZip_split_integrate pa pm p xs
> 
> test_FingerTreeZip_split_integrate :: TestTree
> test_FingerTreeZip_split_integrate =
>   testGroup "Split then integrate"
>     [ testProperty "Char/Count" $
>         cprop_FingerTreeZip_split_integrate pChar pCount
>     , testProperty "Bool/Tup" $
>         cprop_FingerTreeZip_split_integrate pBool pTup
>     ]



FTZ test suite
--------------

> test_FingerTreeZip :: TestTree
> test_FingerTreeZip =
>   testGroup "FingerTreeZip"
>     [ test_FingerTreeZip_conversion
>     , test_FingerTreeZip_init
>     , test_FingerTreeZip_last
>     , test_FingerTreeZip_head
>     , test_FingerTreeZip_commuting_actions
>     , test_FingerTreeZip_split_integrate
>     ]



Tape tests
==========

As far as possible the tests for `Tape` will be copies of the tests for `FingerTreeZip` with the types generalized. It's unfortunate that we have to do this, but necessary because `FingerTreeZip` cannot itself be an instance of `Tape`.

Conversion properties for Tape
------------------------------

Test converting between lists and tapes:

> test_Tape_conversion
>   :: ( Tape t a
>      , Eq a, Show a, Arbitrary a
>      , Eq (t a), Show (t a), Arbitrary (t a) )
>   => Proxy a -> Proxy t
>   -> TestTree
> test_Tape_conversion pa pt =
>   testGroup "Conversion properties"
>     [ testProperty "unTape . mkTape == id" $
>         cprop_Tape_mkTape_unTape pa pt
>     , testProperty "mkTape . unTape == initMove" $
>         cprop_Tape_unTape_mkTape pa pt
>     ]

`unTape` is a left inverse of `mkTape`:

> prop_Tape_unTape_mkTape
>    , cprop_Tape_unTape_mkTape
>   :: forall a t
>    . ( Tape t a, Eq a, Eq (t a) )
>   => Proxy a -> Proxy t
>   -> [a]
>   -> Property
> 
> prop_Tape_unTape_mkTape _ _ xs =
>   property $
>     let z = mkTape xs :: t a
>     in xs == unTape z
> 
> cprop_Tape_unTape_mkTape pa pt xs =
>   cover 1 (xs == []) "xs empty" $
>   cover 30 (length xs > 10) "length xs > 10" $
>   prop_Tape_unTape_mkTape pa pt xs

`mkTape` is a left inverse of `unTape` (almost):

> prop_Tape_mkTape_unTape
>    , cprop_Tape_mkTape_unTape
>   :: forall a t
>    . ( Tape t a, Eq a, Eq (t a) )
>   => Proxy a -> Proxy t
>   -> t a
>   -> Property
> 
> prop_Tape_mkTape_unTape _ _ xs =
>   property $
>     (initMove xs) == mkTape (unTape xs)
> 
> cprop_Tape_mkTape_unTape pa pt xs =
>   cover 1 (not $ isEmpty xs) "xs not empty" $
>   cover 30 (not $ isAtInit xs) "xs not at init" $
>   prop_Tape_mkTape_unTape pa pt xs



Init properties for Tape
------------------------

Tests manipulating the beginning of the list:

> test_Tape_init
>   :: ( Tape t a
>      , Eq a, Show a, Arbitrary a
>      , Eq (t a), Show (t a), Arbitrary (t a) )
>   => Proxy a -> Proxy t
>   -> TestTree
> test_Tape_init pa pt =
>   testGroup "Init related properties"
>     [ testProperty "isAtInit mkTapeFocus" $
>         cprop_Tape_isAtInit_mkTapeFocus pa pt
>     , testProperty "initAlter" $
>         cprop_Tape_initAlter pa pt
>     , testProperty "initRead initInsert" $
>         cprop_Tape_initRead_initInsert pa pt
>     , testProperty "initDelete initInsert" $
>         cprop_Tape_initDelete_initInsert pa pt
>     , testProperty "initMove isAtInit" $
>         cprop_Tape_initMove_isAtInit pa pt
>     , testProperty "initMove idempotent" $
>         cprop_Tape_initMove_idempotent pa pt
>     ]

`isAtInit` returns true if and only if the segment before the read head is empty.

> prop_Tape_isAtInit_mkTapeFocus
>    , cprop_Tape_isAtInit_mkTapeFocus
>   :: forall a (t :: * -> *)
>    . ( Eq a, Tape t a )
>   => Proxy a -> Proxy t
>   -> [a] -> a -> [a]
>   -> Property
> 
> prop_Tape_isAtInit_mkTapeFocus _ _ as x bs =
>   let w = mkTapeFocus as x bs :: t a in
>   property $
>     (as == []) == (isAtInit w)
> 
> cprop_Tape_isAtInit_mkTapeFocus pa pt as x bs =
>   cover 1 (as == []) "as empty" $
>   cover 1 (bs == []) "bs empty" $
>   prop_Tape_isAtInit_mkTapeFocus pa pt as x bs

`initAlter` behaves predictably with `initInsert`.

> prop_Tape_initAlter
>    , cprop_Tape_initAlter
>   :: forall a t
>    . ( Eq	a, Eq (t a), Tape t a )
>   => Proxy a -> Proxy t
>   -> a -> a -> t a
>   -> Property
> 
> prop_Tape_initAlter _ _ u v xs =
>   let swop x = if x == u then v else x in
>   property $
>     (initInsert v xs)
>       == (initAlter swop $ initInsert u xs)
> 
> cprop_Tape_initAlter pa pm u v xs =
>   cover 1 (isEmpty xs) "xs empty" $
>   cover 40 (u /= v) "u /= v" $
>   prop_Tape_initAlter pa pm u v xs

`initRead` interacts predictably with `initInsert`:

> prop_Tape_initRead_initInsert
>    , cprop_Tape_initRead_initInsert
>   :: forall a t
>    . ( Eq a, Tape t a )
>   => Proxy a -> Proxy t
>   -> a -> t a
>   -> Property
> 
> prop_Tape_initRead_initInsert _ _ a xs =
>   property $
>     (Just a)
>       == (initRead (initInsert a xs))
> 
> cprop_Tape_initRead_initInsert pa pm a xs =
>   cover 1 (isEmpty xs) "xs empty" $
>   cover 30 (not $ isAtInit xs) "xs not at init" $
>   prop_Tape_initRead_initInsert pa pm a xs

`initDelete` interacts predictably with `initInsert`.

> prop_Tape_initDelete_initInsert
>    , cprop_Tape_initDelete_initInsert
>   :: forall a t
>    . ( Eq a, Eq (t a), Tape t a )
>   => Proxy a -> Proxy t
>   -> a -> t a
>   -> Property
> 
> prop_Tape_initDelete_initInsert _ _ a xs =
>   property $
>     xs == (initDelete $ initInsert a xs)
> 
> cprop_Tape_initDelete_initInsert pa pm a xs =
>   cover 1 (isEmpty xs) "xs empty" $
>   cover 30 (not $ isAtInit xs) "xs not at init" $
>   prop_Tape_initDelete_initInsert pa pm a xs

After moving to the beginning of the stream, we are at the beginning of the stream.

> prop_Tape_initMove_isAtInit
>    , cprop_Tape_initMove_isAtInit
>   :: forall a t
>    . ( Eq a, Tape t a, Eq (t a) )
>   => Proxy a -> Proxy t
>   -> t a
>   -> Property
> 
> prop_Tape_initMove_isAtInit _ _ xs =
>   property $
>     isAtInit (initMove xs)
> 
> cprop_Tape_initMove_isAtInit pa pm xs =
>   cover 30 (not $ isAtInit xs) "not at init" $
>   cover 1 (isEmpty xs) "xs empty" $
>   prop_Tape_initMove_isAtInit pa pm xs

`initMove` is idempotent:

> prop_Tape_initMove_idempotent
>    , cprop_Tape_initMove_idempotent
>   :: forall a t
>    . ( Eq a, Tape t a, Eq (t a) )
>   => Proxy a -> Proxy t
>   -> t a
>   -> Property
> 
> prop_Tape_initMove_idempotent _ _ xs =
>   property $
>     (initMove xs)
>       == (initMove $ initMove xs)
> 
> cprop_Tape_initMove_idempotent pa pm xs =
>   cover 30 (not $ isAtInit xs) "not at init" $
>   cover 40 (not $ isEmpty xs) "xs not empty" $
>   prop_Tape_initMove_idempotent pa pm xs



Last properties for Tape
------------------------

Tests manipulating the beginning of the list:

> test_Tape_last
>   :: ( Tape t a
>      , Eq a, Show a, Arbitrary a
>      , Eq (t a), Show (t a), Arbitrary (t a) )
>   => Proxy a -> Proxy t
>   -> TestTree
> test_Tape_last pa pt =
>   testGroup "last related properties"
>     [ testProperty "lastAlter" $
>         cprop_Tape_lastAlter pa pt
>     , testProperty "lastRead lastInsert" $
>         cprop_Tape_lastRead_lastInsert pa pt
>     , testProperty "lastDelete lastInsert" $
>         cprop_Tape_lastDelete_lastInsert pa pt
>     , testProperty "lastMove isAtLast" $
>         cprop_Tape_lastMove_isAtLast pa pt
>     , testProperty "lastMove idempotent" $
>         cprop_Tape_lastMove_idempotent pa pt
>     ]

`lastAlter` behaves predictably with `lastInsert`.

> prop_Tape_lastAlter
>    , cprop_Tape_lastAlter
>   :: forall a t
>    . ( Eq a, Eq (t a), Tape t a )
>   => Proxy a -> Proxy t
>   -> a -> a -> t a
>   -> Property
> 
> prop_Tape_lastAlter _ _ u v xs =
>   let swop x = if x == u then v else x in
>   property $
>     (lastInsert v xs)
>       == (lastAlter swop $ lastInsert u xs)
> 
> cprop_Tape_lastAlter pa pm u v xs =
>   cover 1 (isEmpty xs) "xs empty" $
>   cover 40 (u /= v) "u /= v" $
>   prop_Tape_lastAlter pa pm u v xs

`lastRead` interacts predictably with `lastInsert`:

> prop_Tape_lastRead_lastInsert
>    , cprop_Tape_lastRead_lastInsert
>   :: forall a t
>    . ( Eq a, Tape t a )
>   => Proxy a -> Proxy t
>   -> a -> t a
>   -> Property
> 
> prop_Tape_lastRead_lastInsert _ _ a xs =
>   property $
>     (Just a)
>       == (lastRead (lastInsert a xs))
> 
> cprop_Tape_lastRead_lastInsert pa pm a xs =
>   cover 1 (isEmpty xs) "xs empty" $
>   cover 30 (not $ isAtLast xs) "xs not at last" $
>   prop_Tape_lastRead_lastInsert pa pm a xs

`lastDelete` interacts predictably with `lastInsert`.

> prop_Tape_lastDelete_lastInsert
>    , cprop_Tape_lastDelete_lastInsert
>   :: forall a t
>    . ( Eq a, Eq (t a), Tape t a )
>   => Proxy a -> Proxy t
>   -> a -> t a
>   -> Property
> 
> prop_Tape_lastDelete_lastInsert _ _ a xs =
>   property $
>     xs == (lastDelete $ lastInsert a xs)
> 
> cprop_Tape_lastDelete_lastInsert pa pm a xs =
>   cover 1 (isEmpty xs) "xs empty" $
>   cover 30 (not $ isAtLast xs) "xs not at last" $
>   prop_Tape_lastDelete_lastInsert pa pm a xs

After moving to the beginning of the stream, we are at the beginning of the stream.

> prop_Tape_lastMove_isAtLast
>    , cprop_Tape_lastMove_isAtLast
>   :: forall a t
>    . ( Eq a, Tape t a, Eq (t a) )
>   => Proxy a -> Proxy t
>   -> t a
>   -> Property
> 
> prop_Tape_lastMove_isAtLast _ _ xs =
>   property $
>     isAtLast (lastMove xs)
> 
> cprop_Tape_lastMove_isAtLast pa pm xs =
>   cover 30 (not $ isAtLast xs) "not at last" $
>   cover 1 (isEmpty xs) "xs empty" $
>   prop_Tape_lastMove_isAtLast pa pm xs

`lastMove` is idempotent:

> prop_Tape_lastMove_idempotent
>    , cprop_Tape_lastMove_idempotent
>   :: forall a t
>    . ( Eq a, Tape t a, Eq (t a) )
>   => Proxy a -> Proxy t
>   -> t a
>   -> Property
> 
> prop_Tape_lastMove_idempotent _ _ xs =
>   property $
>     (lastMove xs)
>       == (lastMove $ lastMove xs)
> 
> cprop_Tape_lastMove_idempotent pa pt xs =
>   cover 30 (not $ isAtLast xs) "not at last" $
>   cover 40 (not $ isEmpty xs) "xs not empty" $
>   prop_Tape_lastMove_idempotent pa pt xs



Head properties for Tape
------------------------

Tests manipulating the read head:

> test_Tape_head
>   :: ( Tape t a
>      , Eq a, Show a, Arbitrary a
>      , Eq (t a), Show (t a), Arbitrary (t a) )
>   => Proxy a -> Proxy t
>   -> TestTree
> test_Tape_head pa pt =
>   testGroup "Head properties"
>     [ testProperty "headMoveR . headMoveL == id" $
>         cprop_Tape_headMoveR_headMoveL pa pt
>     , testProperty "headMoveL . headMoveR == id" $
>         cprop_Tape_headMoveL_headMoveR pa pt
>     , testProperty "headDeleteL . headInsertL a == id" $
>         cprop_Tape_headDeleteL_headInsertL pa pt
>     , testProperty "headDeleteR . headInsertR a == id" $
>         cprop_Tape_headDeleteR_headInsertR pa pt
>     ]

`headMoveR` is a left inverse of `headMoveL` (almost):

> prop_Tape_headMoveR_headMoveL
>    , cprop_Tape_headMoveR_headMoveL
>   :: forall a t
>    . ( Tape t a, Eq (t a) )
>   => Proxy a -> Proxy t
>   -> t a
>   -> Property
> 
> prop_Tape_headMoveR_headMoveL _ _ xs =
>   property $
>     if isAtInit xs
>       then True
>       else xs == headMoveR (headMoveL xs)
> 
> cprop_Tape_headMoveR_headMoveL pa pm xs =
>   cover 60 (not $ isEmpty xs) "xs not empty" $
>   cover 20 (not $ isAtInit xs) "xs not at init" $
>   prop_Tape_headMoveR_headMoveL pa pm xs

`headMoveL` is a left inverse of `headMoveR` (almost):

> prop_Tape_headMoveL_headMoveR
>    , cprop_Tape_headMoveL_headMoveR
>   :: forall a t
>    . ( Tape t a, Eq (t a) )
>   => Proxy a -> Proxy t
>   -> t a
>   -> Property
> 
> prop_Tape_headMoveL_headMoveR _ _ xs =
>   property $
>     if isAtLast xs
>       then True
>       else xs == headMoveL (headMoveR xs)
> 
> cprop_Tape_headMoveL_headMoveR pa pm xs =
>   cover 60 (not $ isEmpty xs) "xs not empty" $
>   cover 20 (not $ isAtInit xs) "xs not at init" $
>   prop_Tape_headMoveL_headMoveR pa pm xs

`headDeleteL` is a left inverse of `headInsertL`:

> prop_Tape_headDeleteL_headInsertL
>   :: forall a t
>    . ( Tape t a, Eq (t a) )
>   => Proxy a -> Proxy t
>   -> a -> t a
>   -> Property
> 
> prop_Tape_headDeleteL_headInsertL _ _ a xs =
>   property $
>     xs == headDeleteL (headInsertL a xs)
> 
> cprop_Tape_headDeleteL_headInsertL pa pm a xs =
>   cover 60 (not $ isEmpty xs) "xs not empty" $
>   cover 20 (not $ isAtInit xs) "xs not at init" $
>   cover 20 (not $ isAtLast xs) "xs not at last" $
>   prop_Tape_headDeleteL_headInsertL pa pm a xs

`headDeleteR` is a left inverse of `headInsertR`:

> prop_Tape_headDeleteR_headInsertR
>   :: forall a t
>    . ( Tape t a, Eq (t a) )
>   => Proxy a -> Proxy t
>   -> a -> t a
>   -> Property
> 
> prop_Tape_headDeleteR_headInsertR _ _ a xs =
>   property $
>     xs == headDeleteR (headInsertR a xs)
> 
> cprop_Tape_headDeleteR_headInsertR pa pm a xs =
>   cover 60 (not $ isEmpty xs) "xs not empty" $
>   cover 20 (not $ isAtInit xs) "xs not at init" $
>   cover 20 (not $ isAtLast xs) "xs not at last" $
>   prop_Tape_headDeleteR_headInsertR pa pm a xs



Commuting actions for Tape
--------------------------

> test_Tape_commuting_actions
>   :: ( Tape t a
>      , Eq a, Show a, Arbitrary a
>      , Eq (t a), Show (t a), Arbitrary (t a) )
>   => Proxy a -> Proxy t
>   -> TestTree
> test_Tape_commuting_actions pa pt =
>   testGroup "Commuting Actions"
>     [ testProperty "initInsert and lastInsert commute" $
>         cprop_Tape_initInsert_lastInsert_commute pa pt
>     , testProperty "initDelete and lastDelete commute" $
>         cprop_Tape_initDelete_lastDelete_commute pa pt
>     ]

`initInsert` and `lastInsert` commute:

> prop_Tape_initInsert_lastInsert_commute
>   :: forall a t
>    . ( Tape t a, Eq (t a) )
>   => Proxy a -> Proxy t
>   -> a -> a -> t a
>   -> Property
> 
> prop_Tape_initInsert_lastInsert_commute _ _ u v xs =
>   property $
>     if isEmpty xs
>       then True
>       else (initInsert u $ lastInsert v xs :: t a)
>             == (lastInsert v $ initInsert u xs)
> 
> cprop_Tape_initInsert_lastInsert_commute pa pt u v xs =
>   cover 60 (not $ isEmpty xs) "xs not empty" $
>   cover 20 (not $ isAtInit xs) "xs not at init" $
>   cover 20 (not $ isAtLast xs) "xs not at last" $
>   prop_Tape_initInsert_lastInsert_commute pa pt u v xs

`initDelete` and `lastDelete` commute:

> prop_Tape_initDelete_lastDelete_commute
>   :: forall a t
>    . ( Tape t a, Eq (t a) )
>   => Proxy a -> Proxy t
>   -> t a
>   -> Property
> 
> prop_Tape_initDelete_lastDelete_commute _ _ xs =
>   property $
>     (initDelete $ lastDelete xs :: t a)
>       == (lastDelete $ initDelete xs)
> 
> cprop_Tape_initDelete_lastDelete_commute pa pt xs =
>   cover 60 (not $ isEmpty xs) "xs not empty" $
>   cover 20 (not $ isAtInit xs) "xs not at init" $
>   cover 20 (not $ isAtLast xs) "xs not at last" $
>   prop_Tape_initDelete_lastDelete_commute pa pt xs



Tape test suite
---------------

> test_Tape
>   :: ( Tape t a
>      , Eq a, Arbitrary a, Show a
>      , Eq (t a), Arbitrary (t a), Show (t a) )
>   => Proxy a -> Proxy (t :: * -> *)
>   -> TestTree
> test_Tape pa pt =
>   testGroup "Tape"
>     [ test_Tape_conversion pa pt
>     , test_Tape_init pa pt
>     , test_Tape_last pa pt
>     , test_Tape_head pa pt
>     , test_Tape_commuting_actions pa pt
>     ]
