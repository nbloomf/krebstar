---
title: Kreb.Struct.FingerTreeZip.Test
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
>     MultiParamTypeClasses
>   , ScopedTypeVariables
>   , KindSignatures
>   , DeriveGeneric
> #-}
> 
> module Kreb.Struct.FingerTreeZip.Test (
>     test_FingerTreeZip
>   , test_Tape
> ) where
> 
> import Data.Proxy
> import GHC.Generics
> 
> import Test.Tasty
> 
> import Kreb.Check
> import Kreb.Struct
> import Kreb.Struct.FingerTree.Test

> pCount :: Proxy Count
> pCount = Proxy
> 
> pChar :: Proxy Char
> pChar = Proxy
> 
> pBool :: Proxy Bool
> pBool = Proxy
> 
> pTup :: Proxy Tup
> pTup = Proxy





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

> check_FingerTreeZip_unTape_mkTape
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> [a]
>   -> Check
> check_FingerTreeZip_unTape_mkTape _ _ xs =
>   check $
>     let z = mkTapeFTZ xs :: FingerTreeZip m a
>     in xs == unTapeFTZ z
> 
> test_FingerTreeZip_unTape_mkTape :: TestTree
> test_FingerTreeZip_unTape_mkTape =
>   testGroup "unTape . mkTape == id"
>     [ testKreb "Char/Count" $
>         check_FingerTreeZip_unTape_mkTape pChar pCount
>     , testKreb "Bool/Tup" $
>         check_FingerTreeZip_unTape_mkTape pBool pTup
>     ]

`mkTape` is a left inverse for `unTape` (almost):

> check_FingerTreeZip_mkTape_unTape
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTreeZip m a
>   -> Check
> check_FingerTreeZip_mkTape_unTape _ _ xs =
>   check $
>     (initMoveFTZ xs)
>       == mkTapeFTZ (unTapeFTZ xs)
> 
> test_FingerTreeZip_mkTape_unTape :: TestTree
> test_FingerTreeZip_mkTape_unTape =
>   testGroup "mkTape . unTape == initMove"
>     [ testKreb "Char/Count" $
>         check_FingerTreeZip_mkTape_unTape pChar pCount
>     , testKreb "Bool/Tup" $
>         check_FingerTreeZip_mkTape_unTape pBool pTup
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

> check_FingerTreeZip_isAtInit_mkTapeFocus
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> [a] -> a -> [a]
>   -> Check
> check_FingerTreeZip_isAtInit_mkTapeFocus _ _ as x bs =
>   let w = mkTapeFocusFTZ as x bs :: FingerTreeZip m a in
>   check $
>     (as == []) == (isAtInitFTZ w)
> 
> test_FingerTreeZip_isAtInit_mkTapeFocus :: TestTree
> test_FingerTreeZip_isAtInit_mkTapeFocus =
>   testGroup "isAtInit mkTapeFocus"
>     [ testKreb "Char/Count" $
>         check_FingerTreeZip_isAtInit_mkTapeFocus pChar pCount
>     , testKreb "Bool/Tup" $
>         check_FingerTreeZip_isAtInit_mkTapeFocus pBool pTup
> 
>     , testKrebCases "Count/Char"
>       (uncurry3 $ check_FingerTreeZip_isAtInit_mkTapeFocus pChar pCount)
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

> check_FingerTreeZip_initAlter
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> a -> FingerTreeZip m a
>   -> Check
> check_FingerTreeZip_initAlter _ _ u v xs =
>   let swop x = if x == u then v else x in
>   check $
>     (initInsertFTZ v xs)
>       == (initAlterFTZ swop $ initInsertFTZ u xs)
> 
> test_FingerTreeZip_initAlter :: TestTree
> test_FingerTreeZip_initAlter =
>   testGroup "initAlter cons"
>     [ testKreb "Char/Count" $
>         check_FingerTreeZip_initAlter pChar pCount
>     , testKreb "Bool/Tup" $
>         check_FingerTreeZip_initAlter pBool pTup
> 
>     , testKrebCases "Count/Char"
>       (uncurry3 $ check_FingerTreeZip_initAlter pChar pCount)
>       [ ( "empty tail"
>         , ( 'a'
>           , 'b'
>           , emptyFTZ
>           )
>         )
>       ]
>     ]

We also check some concrete input/output pairs for `initAlter`.

> check_FingerTreeZip_initAlter_examples
>   :: forall m a
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> (a -> a) -> FingerTreeZip m a
>   -> FingerTreeZip m a
>   -> Check
> check_FingerTreeZip_initAlter_examples _ _ f xs ys =
>   check $
>     ys == (initAlterFTZ f xs)
> 
> test_FingerTreeZip_initAlter_examples :: TestTree
> test_FingerTreeZip_initAlter_examples =
>   testGroup "initAlter examples"
>     [ testKrebCases "Count/Char"
>       (uncurry3 $ check_FingerTreeZip_initAlter_examples pChar pCount)
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

> check_FingerTreeZip_initRead_initInsert
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> FingerTreeZip m a
>   -> Check
> check_FingerTreeZip_initRead_initInsert _ _ a xs =
>   check $
>     (Just a)
>       == (initReadFTZ (initInsertFTZ a xs))
> 
> test_FingerTreeZip_initRead_initInsert :: TestTree
> test_FingerTreeZip_initRead_initInsert =
>   testGroup "initRead cons"
>     [ testKreb "Char/Count" $
>         check_FingerTreeZip_initRead_initInsert pChar pCount
>     , testKreb "Bool/Tup" $
>         check_FingerTreeZip_initRead_initInsert pBool pTup
> 
>     , testKrebCases "Count/Char"
>       (uncurry $ check_FingerTreeZip_initRead_initInsert pChar pCount)
>       [ ( "empty tail"
>         , ( 'a', emptyFTZ )
>         )
>       ]
>     ]

Some concrete examples for `initRead`:

> check_FingerTreeZip_initRead_examples
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTreeZip m a -> Maybe a
>   -> Check
> check_FingerTreeZip_initRead_examples _ _ xs z =
>   check $
>     z == initReadFTZ xs
> 
> test_FingerTreeZip_initRead_examples :: TestTree
> test_FingerTreeZip_initRead_examples =
>   testGroup "initRead examples"
>     [ testKrebCases "Count/Char"
>       (uncurry $ check_FingerTreeZip_initRead_examples pChar pCount)
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

> check_FingerTreeZip_initDelete_initInsert
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> FingerTreeZip m a
>   -> Check
> check_FingerTreeZip_initDelete_initInsert _ _ a xs =
>   check $
>     xs == (initDeleteFTZ $ initInsertFTZ a xs)
> 
> test_FingerTreeZip_initDelete_initInsert :: TestTree
> test_FingerTreeZip_initDelete_initInsert =
>   testGroup "initDelete cons"
>     [ testKreb "Char/Count" $
>         check_FingerTreeZip_initDelete_initInsert pChar pCount
>     , testKreb "Bool/Tup" $
>         check_FingerTreeZip_initDelete_initInsert pBool pTup
>     ]

And some examples for `initDelete`:

> check_FingerTreeZip_initDelete_examples
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTreeZip m a -> FingerTreeZip m a
>   -> Check
> check_FingerTreeZip_initDelete_examples _ _ xs ys =
>   check $
>     ys == (initDeleteFTZ xs)
> 
> test_FingerTreeZip_initDelete_examples :: TestTree
> test_FingerTreeZip_initDelete_examples =
>   testGroup "initDelete examples"
>     [ testKrebCases "Count/Char"
>       (uncurry $ check_FingerTreeZip_initDelete_examples pChar pCount)
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

> check_FingerTreeZip_initMove_isAtInit
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTreeZip m a
>   -> Check
> check_FingerTreeZip_initMove_isAtInit _ _ xs =
>   check $
>     isAtInitFTZ (initMoveFTZ xs)
> 
> test_FingerTreeZip_initMove_isAtInit :: TestTree
> test_FingerTreeZip_initMove_isAtInit =
>   testGroup "initMove isAtInit"
>     [ testKreb "Char/Count" $
>         check_FingerTreeZip_initMove_isAtInit pChar pCount
>     , testKreb "Bool/Tup" $
>         check_FingerTreeZip_initMove_isAtInit pBool pTup
>     ]

We can also test `initMove` on some specific cases.

> check_FingerTreeZip_initMove_examples
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTreeZip m a
>   -> FingerTreeZip m a
>   -> Check
> check_FingerTreeZip_initMove_examples _ _ xs ys =
>   check $
>     ys == (initMoveFTZ xs)
> 
> test_FingerTreeZip_initMove_examples :: TestTree
> test_FingerTreeZip_initMove_examples =
>   testGroup "initMove examples"
>     [ testKrebCases "Count/Char"
>       (uncurry $ check_FingerTreeZip_initMove_examples pChar pCount)
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

> check_FingerTreeZip_initMove_idempotent
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTreeZip m a
>   -> Check
> check_FingerTreeZip_initMove_idempotent _ _ xs =
>   check $
>     (initMoveFTZ xs)
>       == (initMoveFTZ $ initMoveFTZ xs)
> 
> test_FingerTreeZip_initMove_idempotent :: TestTree
> test_FingerTreeZip_initMove_idempotent =
>   testGroup "initMove is idempotent"
>     [ testKreb "Char/Count" $
>         check_FingerTreeZip_initMove_idempotent pChar pCount
>     , testKreb "Bool/Tup" $
>         check_FingerTreeZip_initMove_idempotent pBool pTup
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

> check_FingerTreeZip_lastAlter
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> a -> FingerTreeZip m a
>   -> Check
> check_FingerTreeZip_lastAlter _ _ u v xs =
>   let swop x = if x == u then v else x in
>   check $
>     (lastInsertFTZ v xs)
>       == (lastAlterFTZ swop $ lastInsertFTZ u xs)
> 
> test_FingerTreeZip_lastAlter :: TestTree
> test_FingerTreeZip_lastAlter =
>   testGroup "lastAlter cons"
>     [ testKreb "Char/Count" $
>         check_FingerTreeZip_lastAlter pChar pCount
>     , testKreb "Bool/Tup" $
>         check_FingerTreeZip_lastAlter pBool pTup
> 
>     , testKrebCases "Count/Char"
>       (uncurry3 $ check_FingerTreeZip_lastAlter pChar pCount)
>       [ ( "empty tail"
>         , ( 'a'
>           , 'b'
>           , emptyFTZ
>           )
>         )
>       ]
>     ]

We also check some concrete input/output pairs for `lastAlter`.

> check_FingerTreeZip_lastAlter_examples
>   :: forall m a
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> (a -> a) -> FingerTreeZip m a
>   -> FingerTreeZip m a
>   -> Check
> check_FingerTreeZip_lastAlter_examples _ _ f xs ys =
>   check $
>     ys == (lastAlterFTZ f xs)
> 
> test_FingerTreeZip_lastAlter_examples :: TestTree
> test_FingerTreeZip_lastAlter_examples =
>   testGroup "lastAlter examples"
>     [ testKrebCases "Count/Char"
>       (uncurry3 $ check_FingerTreeZip_lastAlter_examples pChar pCount)
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

> check_FingerTreeZip_lastRead_lastInsert
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> FingerTreeZip m a
>   -> Check
> check_FingerTreeZip_lastRead_lastInsert _ _ a xs =
>   check $
>     (Just a)
>       == (lastReadFTZ (lastInsertFTZ a xs))
> 
> test_FingerTreeZip_lastRead_lastInsert :: TestTree
> test_FingerTreeZip_lastRead_lastInsert =
>   testGroup "lastRead cons"
>     [ testKreb "Char/Count" $
>         check_FingerTreeZip_lastRead_lastInsert pChar pCount
>     , testKreb "Bool/Tup" $
>         check_FingerTreeZip_lastRead_lastInsert pBool pTup
> 
>     , testKrebCases "Count/Char"
>       (uncurry $ check_FingerTreeZip_lastRead_lastInsert pChar pCount)
>       [ ( "empty tail"
>         , ( 'a', emptyFTZ )
>         )
>       ]
>     ]

Some concrete examples for `lastRead`:

> check_FingerTreeZip_lastRead_examples
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTreeZip m a -> Maybe a
>   -> Check
> check_FingerTreeZip_lastRead_examples _ _ xs z =
>   check $
>     z == lastReadFTZ xs
> 
> test_FingerTreeZip_lastRead_examples :: TestTree
> test_FingerTreeZip_lastRead_examples =
>   testGroup "lastRead examples"
>     [ testKrebCases "Count/Char"
>       (uncurry $ check_FingerTreeZip_lastRead_examples pChar pCount)
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

> check_FingerTreeZip_lastDelete_lastInsert
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> FingerTreeZip m a
>   -> Check
> check_FingerTreeZip_lastDelete_lastInsert _ _ a xs =
>   check $
>     xs == (lastDeleteFTZ $ lastInsertFTZ a xs)
> 
> test_FingerTreeZip_lastDelete_lastInsert :: TestTree
> test_FingerTreeZip_lastDelete_lastInsert =
>   testGroup "lastDelete cons"
>     [ testKreb "Char/Count" $
>         check_FingerTreeZip_lastDelete_lastInsert pChar pCount
>     , testKreb "Bool/Tup" $
>         check_FingerTreeZip_lastDelete_lastInsert pBool pTup
>     ]

And some examples for `lastDelete`:

> check_FingerTreeZip_lastDelete_examples
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTreeZip m a -> FingerTreeZip m a
>   -> Check
> check_FingerTreeZip_lastDelete_examples _ _ xs ys =
>   check $
>     ys == (lastDeleteFTZ xs)
> 
> test_FingerTreeZip_lastDelete_examples :: TestTree
> test_FingerTreeZip_lastDelete_examples =
>   testGroup "lastDelete examples"
>     [ testKrebCases "Count/Char"
>       (uncurry $ check_FingerTreeZip_lastDelete_examples pChar pCount)
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

> check_FingerTreeZip_lastMove_isAtLast
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTreeZip m a
>   -> Check
> check_FingerTreeZip_lastMove_isAtLast _ _ xs =
>   check $
>     isAtLastFTZ (lastMoveFTZ xs)
> 
> test_FingerTreeZip_lastMove_isAtLast :: TestTree
> test_FingerTreeZip_lastMove_isAtLast =
>   testGroup "lastMove isAtLast"
>     [ testKreb "Char/Count" $
>         check_FingerTreeZip_lastMove_isAtLast pChar pCount
>     , testKreb "Bool/Tup" $
>         check_FingerTreeZip_lastMove_isAtLast pBool pTup
>     ]

We can also test `lastMove` on some specific cases.

> check_FingerTreeZip_lastMove_examples
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTreeZip m a
>   -> FingerTreeZip m a
>   -> Check
> check_FingerTreeZip_lastMove_examples _ _ xs ys =
>   check $
>     ys == (lastMoveFTZ xs)
> 
> test_FingerTreeZip_lastMove_examples :: TestTree
> test_FingerTreeZip_lastMove_examples =
>   testGroup "lastMove examples"
>     [ testKrebCases "Count/Char"
>       (uncurry $ check_FingerTreeZip_lastMove_examples pChar pCount)
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

> check_FingerTreeZip_lastMove_idempotent
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTreeZip m a
>   -> Check
> check_FingerTreeZip_lastMove_idempotent _ _ xs =
>   check $
>     (lastMoveFTZ xs)
>       == (lastMoveFTZ $ lastMoveFTZ xs)
> 
> test_FingerTreeZip_lastMove_idempotent :: TestTree
> test_FingerTreeZip_lastMove_idempotent =
>   testGroup "lastMove is idempotent"
>     [ testKreb "Char/Count" $
>         check_FingerTreeZip_lastMove_idempotent pChar pCount
>     , testKreb "Bool/Tup" $
>         check_FingerTreeZip_lastMove_idempotent pBool pTup
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

> check_FingerTreeZip_headMoveR_headMoveL
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTreeZip m a
>   -> Check
> check_FingerTreeZip_headMoveR_headMoveL _ _ xs =
>   check $
>     if isAtInitFTZ xs
>       then True
>       else xs == headMoveRFTZ (headMoveLFTZ xs)
> 
> test_FingerTreeZip_headMoveR_headMoveL :: TestTree
> test_FingerTreeZip_headMoveR_headMoveL =
>   testGroup "headMoveR . headMoveL == id"
>     [ testKreb "Char/Count" $
>         check_FingerTreeZip_headMoveR_headMoveL pChar pCount
>     , testKreb "Bool/Tup" $
>         check_FingerTreeZip_headMoveR_headMoveL pBool pTup
>     ]

`headMoveL` is a left inverse of `headMoveR` (almost):

> check_FingerTreeZip_headMoveL_headMoveR
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTreeZip m a
>   -> Check
> check_FingerTreeZip_headMoveL_headMoveR _ _ xs =
>   check $
>     if isAtLastFTZ xs
>       then True
>       else xs == headMoveLFTZ (headMoveRFTZ xs)
> 
> test_FingerTreeZip_headMoveL_headMoveR :: TestTree
> test_FingerTreeZip_headMoveL_headMoveR =
>   testGroup "headMoveL . headMoveR == id"
>     [ testKreb "Char/Count" $
>         check_FingerTreeZip_headMoveL_headMoveR pChar pCount
>     , testKreb "Bool/Tup" $
>         check_FingerTreeZip_headMoveL_headMoveR pBool pTup
>     ]

`headMoveR` examples:

> check_FingerTreeZip_headMoveR_examples
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTreeZip m a
>   -> FingerTreeZip m a
>   -> Check
> check_FingerTreeZip_headMoveR_examples _ _ xs ys =
>   check $
>     ys == headMoveRFTZ xs
> 
> test_FingerTreeZip_headMoveR_examples :: TestTree
> test_FingerTreeZip_headMoveR_examples =
>   testGroup "headMoveR examples"
>     [ testKrebCases "Count/Char"
>       (uncurry $ check_FingerTreeZip_headMoveR_examples pChar pCount)
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

> check_FingerTreeZip_headMoveL_examples
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTreeZip m a
>   -> FingerTreeZip m a
>   -> Check
> check_FingerTreeZip_headMoveL_examples _ _ xs ys =
>   check $
>     ys == headMoveLFTZ xs
> 
> test_FingerTreeZip_headMoveL_examples :: TestTree
> test_FingerTreeZip_headMoveL_examples =
>   testGroup "headMoveL examples"
>     [ testKrebCases "Count/Char"
>       (uncurry $ check_FingerTreeZip_headMoveL_examples pChar pCount)
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

> check_FingerTreeZip_headDeleteL_headInsertL
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> FingerTreeZip m a
>   -> Check
> check_FingerTreeZip_headDeleteL_headInsertL _ _ a xs =
>   check $
>     xs == headDeleteLFTZ (headInsertLFTZ a xs)
> 
> test_FingerTreeZip_headDeleteL_headInsertL :: TestTree
> test_FingerTreeZip_headDeleteL_headInsertL =
>   testGroup "headDeleteL . headInsertL a == id"
>     [ testKreb "Char/Count" $
>         check_FingerTreeZip_headDeleteL_headInsertL pChar pCount
>     , testKreb "Bool/Tup" $
>         check_FingerTreeZip_headDeleteL_headInsertL pBool pTup
>     ]

`headDeleteR` is a left inverse of `headInsertR`:

> check_FingerTreeZip_headDeleteR_headInsertR
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> FingerTreeZip m a
>   -> Check
> check_FingerTreeZip_headDeleteR_headInsertR _ _ a xs =
>   check $
>     xs == headDeleteRFTZ (headInsertRFTZ a xs)
> 
> test_FingerTreeZip_headDeleteR_headInsertR :: TestTree
> test_FingerTreeZip_headDeleteR_headInsertR =
>   testGroup "headDeleteR . headInsertR a == id"
>     [ testKreb "Char/Count" $
>         check_FingerTreeZip_headDeleteR_headInsertR pChar pCount
>     , testKreb "Bool/Tup" $
>         check_FingerTreeZip_headDeleteR_headInsertR pBool pTup
>     ]

Examples of `headDeleteL`:

> check_FingerTreeZip_headDeleteL_examples
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTreeZip m a
>   -> FingerTreeZip m a
>   -> Check
> check_FingerTreeZip_headDeleteL_examples _ _ xs ys =
>   check $
>     ys == headDeleteLFTZ xs
> 
> test_FingerTreeZip_headDeleteL_examples :: TestTree
> test_FingerTreeZip_headDeleteL_examples =
>   testGroup "headDeleteL examples"
>     [ testKrebCases "Count/Char"
>       (uncurry $ check_FingerTreeZip_headDeleteL_examples pChar pCount)
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

> check_FingerTreeZip_headDeleteR_examples
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> FingerTreeZip m a
>   -> FingerTreeZip m a
>   -> Check
> check_FingerTreeZip_headDeleteR_examples _ _ xs ys =
>   check $
>     ys == headDeleteRFTZ xs
> 
> test_FingerTreeZip_headDeleteR_examples :: TestTree
> test_FingerTreeZip_headDeleteR_examples =
>   testGroup "headDeleteR examples"
>     [ testKrebCases "Count/Char"
>       (uncurry $ check_FingerTreeZip_headDeleteR_examples pChar pCount)
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

> check_FingerTreeZip_headInsertL_examples
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> FingerTreeZip m a
>   -> FingerTreeZip m a
>   -> Check
> check_FingerTreeZip_headInsertL_examples _ _ a xs ys =
>   check $
>     ys == (headInsertLFTZ a xs)
> 
> test_FingerTreeZip_headInsertL_examples :: TestTree
> test_FingerTreeZip_headInsertL_examples =
>   testGroup "headInsertL examples"
>     [ testKrebCases "Count/Char"
>       (uncurry3 $ check_FingerTreeZip_headInsertL_examples pChar pCount)
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

> check_FingerTreeZip_headInsertR_examples
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> FingerTreeZip m a
>   -> FingerTreeZip m a
>   -> Check
> check_FingerTreeZip_headInsertR_examples _ _ a xs ys =
>   check $
>     ys == (headInsertRFTZ a xs)
> 
> test_FingerTreeZip_headInsertR_examples :: TestTree
> test_FingerTreeZip_headInsertR_examples =
>   testGroup "headInsertR examples"
>     [ testKrebCases "Count/Char"
>       (uncurry3 $ check_FingerTreeZip_headInsertR_examples pChar pCount)
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

> check_FingerTreeZip_initInsert_lastInsert
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> a -> FingerTreeZip m a
>   -> Check
> check_FingerTreeZip_initInsert_lastInsert _ _ u v xs =
>   check $
>     if isEmptyFTZ xs
>       then True
>       else (initInsertFTZ u $ lastInsertFTZ v xs)
>             == (lastInsertFTZ v $ initInsertFTZ u xs)
> 
> test_FingerTreeZip_initInsert_lastInsert :: TestTree
> test_FingerTreeZip_initInsert_lastInsert =
>   testGroup "initInsert and lastInsert commute"
>     [ testKreb "Char/Count" $
>         check_FingerTreeZip_initInsert_lastInsert pChar pCount
>     , testKreb "Bool/Tup" $
>         check_FingerTreeZip_initInsert_lastInsert pBool pTup
>     ]

`initDelete` and `lastDelete` commute:

> check_FingerTreeZip_initDelete_lastDelete
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> a -> a -> FingerTreeZip m a
>   -> Check
> check_FingerTreeZip_initDelete_lastDelete _ _ u v xs =
>   check $
>     (initDeleteFTZ $ lastDeleteFTZ xs)
>       == (lastDeleteFTZ $ initDeleteFTZ xs)
> 
> test_FingerTreeZip_initDelete_lastDelete :: TestTree
> test_FingerTreeZip_initDelete_lastDelete =
>   testGroup "initDelete and lastDelete commute"
>     [ testKreb "Char/Count" $
>         check_FingerTreeZip_initDelete_lastDelete pChar pCount
>     , testKreb "Bool/Tup" $
>         check_FingerTreeZip_initDelete_lastDelete pBool pTup
>     ]



Split and integrate
-------------------

> check_FingerTreeZip_split_integrate
>   :: forall a m
>    . ( Eq a, Valued m a )
>   => Proxy a -> Proxy m
>   -> Fun m Bool -> FingerTree m a
>   -> Check
> check_FingerTreeZip_split_integrate _ _ p xs =
>   check $ if (apFun p mempty == False) && (apFun p (value xs) == True)
>     then xs == integrateFTZ (splitFTZ (apFun p) xs)
>     else True
> 
> test_FingerTreeZip_split_integrate :: TestTree
> test_FingerTreeZip_split_integrate =
>   testGroup "Split then integrate"
>     [ testKreb "Char/Count" $
>         check_FingerTreeZip_split_integrate pChar pCount
>     , testKreb "Bool/Tup" $
>         check_FingerTreeZip_split_integrate pBool pTup
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
>      , Eq a, Show a, Arb a, Prune a
>      , Eq (t a), Show (t a), Arb (t a), Prune (t a) )
>   => Proxy a -> Proxy t
>   -> TestTree
> test_Tape_conversion pa pt =
>   testGroup "Conversion properties"
>     [ testKreb "unTape . mkTape == id" $
>         check_Tape_mkTape_unTape pa pt
>     , testKreb "mkTape . unTape == initMove" $
>         check_Tape_unTape_mkTape pa pt
>     ]

`unTape` is a left inverse of `mkTape`:

> check_Tape_unTape_mkTape
>   :: forall a t
>    . ( Tape t a, Eq a, Eq (t a) )
>   => Proxy a -> Proxy t
>   -> [a]
>   -> Check
> check_Tape_unTape_mkTape _ _ xs =
>   check $
>     let z = mkTape xs :: t a
>     in xs == unTape z

`mkTape` is a left inverse of `unTape` (almost):

> check_Tape_mkTape_unTape
>   :: forall a t
>    . ( Tape t a, Eq a, Eq (t a) )
>   => Proxy a -> Proxy t
>   -> t a
>   -> Check
> check_Tape_mkTape_unTape _ _ xs =
>   check $
>     (initMove xs) == mkTape (unTape xs)



Init properties for Tape
------------------------

Tests manipulating the beginning of the list:

> test_Tape_init
>   :: ( Tape t a
>      , Eq a, Show a, Arb a, Prune a
>      , Eq (t a), Show (t a), Arb (t a), Prune (t a) )
>   => Proxy a -> Proxy t
>   -> TestTree
> test_Tape_init pa pt =
>   testGroup "Init related properties"
>     [ testKreb "isAtInit mkTapeFocus" $
>         check_Tape_isAtInit_mkTapeFocus pa pt
>     , testKreb "initAlter" $
>         check_Tape_initAlter pa pt
>     , testKreb "initRead initInsert" $
>         check_Tape_initRead_initInsert pa pt
>     , testKreb "initDelete initInsert" $
>         check_Tape_initDelete_initInsert pa pt
>     , testKreb "initMove isAtInit" $
>         check_Tape_initMove_isAtInit pa pt
>     , testKreb "initMove idempotent" $
>         check_Tape_initMove_idempotent pa pt
>     ]

`isAtInit` returns true if and only if the segment before the read head is empty.

> check_Tape_isAtInit_mkTapeFocus
>   :: forall a (t :: * -> *)
>    . ( Eq a, Tape t a )
>   => Proxy a -> Proxy t
>   -> [a] -> a -> [a]
>   -> Check
> check_Tape_isAtInit_mkTapeFocus _ _ as x bs =
>   let w = mkTapeFocus as x bs :: t a in
>   check $
>     (as == []) == (isAtInit w)

`initAlter` behaves predictably with `initInsert`.

> check_Tape_initAlter
>   :: forall a t
>    . ( Eq	a, Eq (t a), Tape t a )
>   => Proxy a -> Proxy t
>   -> a -> a -> t a
>   -> Check
> check_Tape_initAlter _ _ u v xs =
>   let swop x = if x == u then v else x in
>   check $
>     (initInsert v xs)
>       == (initAlter swop $ initInsert u xs)

`initRead` interacts predictably with `initInsert`:

> check_Tape_initRead_initInsert
>   :: forall a t
>    . ( Eq a, Tape t a )
>   => Proxy a -> Proxy t
>   -> a -> t a
>   -> Check
> check_Tape_initRead_initInsert _ _ a xs =
>   check $
>     (Just a)
>       == (initRead (initInsert a xs))

`initDelete` interacts predictably with `initInsert`.

> check_Tape_initDelete_initInsert
>   :: forall a t
>    . ( Eq a, Eq (t a), Tape t a )
>   => Proxy a -> Proxy t
>   -> a -> t a
>   -> Check
> check_Tape_initDelete_initInsert _ _ a xs =
>   check $
>     xs == (initDelete $ initInsert a xs)

After moving to the beginning of the stream, we are at the beginning of the stream.

> check_Tape_initMove_isAtInit
>   :: forall a t
>    . ( Eq a, Tape t a, Eq (t a) )
>   => Proxy a -> Proxy t
>   -> t a
>   -> Check
> check_Tape_initMove_isAtInit _ _ xs =
>   check $
>     isAtInit (initMove xs)

`initMove` is idempotent:

> check_Tape_initMove_idempotent
>   :: forall a t
>    . ( Eq a, Tape t a, Eq (t a) )
>   => Proxy a -> Proxy t
>   -> t a
>   -> Check
> check_Tape_initMove_idempotent _ _ xs =
>   check $
>     (initMove xs)
>       == (initMove $ initMove xs)



Last properties for Tape
------------------------

Tests manipulating the beginning of the list:

> test_Tape_last
>   :: ( Tape t a
>      , Eq a, Show a, Arb a, Prune a
>      , Eq (t a), Show (t a), Arb (t a), Prune (t a) )
>   => Proxy a -> Proxy t
>   -> TestTree
> test_Tape_last pa pt =
>   testGroup "last related properties"
>     [ testKreb "lastAlter" $
>         check_Tape_lastAlter pa pt
>     , testKreb "lastRead lastInsert" $
>         check_Tape_lastRead_lastInsert pa pt
>     , testKreb "lastDelete lastInsert" $
>         check_Tape_lastDelete_lastInsert pa pt
>     , testKreb "lastMove isAtLast" $
>         check_Tape_lastMove_isAtLast pa pt
>     , testKreb "lastMove idempotent" $
>         check_Tape_lastMove_idempotent pa pt
>     ]

`lastAlter` behaves predictably with `lastInsert`.

> check_Tape_lastAlter
>   :: forall a t
>    . ( Eq a, Eq (t a), Tape t a )
>   => Proxy a -> Proxy t
>   -> a -> a -> t a
>   -> Check
> check_Tape_lastAlter _ _ u v xs =
>   let swop x = if x == u then v else x in
>   check $
>     (lastInsert v xs)
>       == (lastAlter swop $ lastInsert u xs)

`lastRead` interacts predictably with `lastInsert`:

> check_Tape_lastRead_lastInsert
>   :: forall a t
>    . ( Eq a, Tape t a )
>   => Proxy a -> Proxy t
>   -> a -> t a
>   -> Check
> check_Tape_lastRead_lastInsert _ _ a xs =
>   check $
>     (Just a)
>       == (lastRead (lastInsert a xs))

`lastDelete` interacts predictably with `lastInsert`.

> check_Tape_lastDelete_lastInsert
>   :: forall a t
>    . ( Eq a, Eq (t a), Tape t a )
>   => Proxy a -> Proxy t
>   -> a -> t a
>   -> Check
> check_Tape_lastDelete_lastInsert _ _ a xs =
>   check $
>     xs == (lastDelete $ lastInsert a xs)

After moving to the beginning of the stream, we are at the beginning of the stream.

> check_Tape_lastMove_isAtLast
>   :: forall a t
>    . ( Eq a, Tape t a, Eq (t a) )
>   => Proxy a -> Proxy t
>   -> t a
>   -> Check
> check_Tape_lastMove_isAtLast _ _ xs =
>   check $
>     isAtLast (lastMove xs)

`lastMove` is idempotent:

> check_Tape_lastMove_idempotent
>   :: forall a t
>    . ( Eq a, Tape t a, Eq (t a) )
>   => Proxy a -> Proxy t
>   -> t a
>   -> Check
> check_Tape_lastMove_idempotent _ _ xs =
>   check $
>     (lastMove xs)
>       == (lastMove $ lastMove xs)



Head properties for Tape
------------------------

Tests manipulating the read head:

> test_Tape_head
>   :: ( Tape t a
>      , Eq a, Show a, Arb a, Prune a
>      , Eq (t a), Show (t a), Arb (t a), Prune (t a) )
>   => Proxy a -> Proxy t
>   -> TestTree
> test_Tape_head pa pt =
>   testGroup "Head properties"
>     [ testKreb "headMoveR . headMoveL == id" $
>         check_Tape_headMoveR_headMoveL pa pt
>     , testKreb "headMoveL . headMoveR == id" $
>         check_Tape_headMoveL_headMoveR pa pt
>     , testKreb "headDeleteL . headInsertL a == id" $
>         check_Tape_headDeleteL_headInsertL pa pt
>     , testKreb "headDeleteR . headInsertR a == id" $
>         check_Tape_headDeleteR_headInsertR pa pt
>     ]

`headMoveR` is a left inverse of `headMoveL` (almost):

> check_Tape_headMoveR_headMoveL
>   :: forall a t
>    . ( Tape t a, Eq (t a) )
>   => Proxy a -> Proxy t
>   -> t a
>   -> Check
> check_Tape_headMoveR_headMoveL _ _ xs =
>   check $
>     if isAtInit xs
>       then True
>       else xs == headMoveR (headMoveL xs)

`headMoveL` is a left inverse of `headMoveR` (almost):

> check_Tape_headMoveL_headMoveR
>   :: forall a t
>    . ( Tape t a, Eq (t a) )
>   => Proxy a -> Proxy t
>   -> t a
>   -> Check
> check_Tape_headMoveL_headMoveR _ _ xs =
>   check $
>     if isAtLast xs
>       then True
>       else xs == headMoveL (headMoveR xs)

`headDeleteL` is a left inverse of `headInsertL`:

> check_Tape_headDeleteL_headInsertL
>   :: forall a t
>    . ( Tape t a, Eq (t a) )
>   => Proxy a -> Proxy t
>   -> a -> t a
>   -> Check
> check_Tape_headDeleteL_headInsertL _ _ a xs =
>   check $
>     xs == headDeleteL (headInsertL a xs)

`headDeleteR` is a left inverse of `headInsertR`:

> check_Tape_headDeleteR_headInsertR
>   :: forall a t
>    . ( Tape t a, Eq (t a) )
>   => Proxy a -> Proxy t
>   -> a -> t a
>   -> Check
> check_Tape_headDeleteR_headInsertR _ _ a xs =
>   check $
>     xs == headDeleteR (headInsertR a xs)



Commuting actions for Tape
--------------------------

> test_Tape_commuting_actions
>   :: ( Tape t a
>      , Eq a, Show a, Arb a, Prune a
>      , Eq (t a), Show (t a), Arb (t a), Prune (t a) )
>   => Proxy a -> Proxy t
>   -> TestTree
> test_Tape_commuting_actions pa pt =
>   testGroup "Commuting Actions"
>     [ testKreb "initInsert and lastInsert commute" $
>         check_Tape_initInsert_lastInsert_commute pa pt
>     , testKreb "initDelete and lastDelete commute" $
>         check_Tape_initDelete_lastDelete_commute pa pt
>     ]

`initInsert` and `lastInsert` commute:

> check_Tape_initInsert_lastInsert_commute
>   :: forall a t
>    . ( Tape t a, Eq (t a) )
>   => Proxy a -> Proxy t
>   -> a -> a -> t a
>   -> Check
> check_Tape_initInsert_lastInsert_commute _ _ u v xs =
>   check $
>     if isEmpty xs
>       then True
>       else (initInsert u $ lastInsert v xs :: t a)
>             == (lastInsert v $ initInsert u xs)

`initDelete` and `lastDelete` commute:

> check_Tape_initDelete_lastDelete_commute
>   :: forall a t
>    . ( Tape t a, Eq (t a) )
>   => Proxy a -> Proxy t
>   -> t a
>   -> Check
> check_Tape_initDelete_lastDelete_commute _ _ xs =
>   check $
>     (initDelete $ lastDelete xs :: t a)
>       == (lastDelete $ initDelete xs)



Tape test suite
---------------

> test_Tape
>   :: ( Tape t a
>      , Eq a, Arb a, Show a, Prune a
>      , Eq (t a), Arb (t a), Show (t a), Prune (t a) )
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