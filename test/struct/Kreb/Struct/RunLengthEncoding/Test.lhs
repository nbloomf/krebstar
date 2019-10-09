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
> import Test.Tasty
> 
> import Kreb.Check
> 
> import Kreb.Struct
> 
> import Kreb.Struct.FingerTree.Test





Test Helpers
------------

> instance Valued Count Bool where
>   value _ = Count 1

> instance Semigroup Int where
>   (<>) = (+)
> 
> instance Monoid Int where
>   mempty = 0

> pInt :: Proxy Int
> pInt = Proxy
> 
> pChar :: Proxy Char
> pChar = Proxy
> 
> pBool :: Proxy Bool
> pBool = Proxy



Tests
=====



RunSize is a monoid
-------------------

> test_RunSize_Monoid :: TestTree
> test_RunSize_Monoid =
>   testGroup "RunSize is a monoid"
>     [ test_Semigroup_laws (Proxy :: Proxy RunSize)
>     , test_Monoid_laws (Proxy :: Proxy RunSize)
>     ]



Run is a functor
----------------

`Run` has a functor instance, which for good measure we should validate.

> test_Run_Functor :: TestTree
> test_Run_Functor =
>   testGroup "Functor laws for Run"
>     [ test_Functor_laws
>         (Proxy :: Proxy Run) pInt pInt pInt
>     ]



RunLengthEncoding is a functor
------------------------------

`RunLengthEncoding` also has a functor instance, which for good measure we should validate.

> test_RunLengthEncoding_Functor :: TestTree
> test_RunLengthEncoding_Functor =
>   testGroup "Functor laws for RunLengthEncoding"
>     [ test_Functor_laws
>         (Proxy :: Proxy RunLengthEncoding) pInt pInt pInt
>     , test_Functor_laws
>         (Proxy :: Proxy RunLengthEncoding) pChar pChar pChar
>     ]



RunLengthEncoding is a Monoid
-----------------------------

> test_RunLengthEncoding_Monoid :: TestTree
> test_RunLengthEncoding_Monoid =
>   testGroup "Monoid laws for RunLengthEncoding"
>     [ test_Semigroup_laws (Proxy :: Proxy (RunLengthEncoding Int))
>     , test_Monoid_laws (Proxy :: Proxy (RunLengthEncoding Int))
>     ]



Foldable laws for RunLengthEncoding
-----------------------------------

`RunLengthEncoding` has a `Foldable` instance, which we can validate.

> test_RunLengthEncoding_Foldable :: TestTree
> test_RunLengthEncoding_Foldable =
>   testGroup "Foldable laws"
>     [ test_Foldable_laws (Proxy :: Proxy (RunLengthEncoding)) pInt pInt pInt
>     , test_FoldableFunctor_laws (Proxy :: Proxy (RunLengthEncoding)) pInt pInt pInt
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
>   -> Check
> prop_RunLengthEncoding_fromFreqList_toFreqList _ xs =
>   check $
>     xs == fromFreqList (toFreqList xs)
> 
> test_RunLengthEncoding_fromFreqList_toFreqList :: TestTree
> test_RunLengthEncoding_fromFreqList_toFreqList =
>   testGroup "fromFreqList . toFreqList == id"
>     [ testKreb "Char" $
>         prop_RunLengthEncoding_fromFreqList_toFreqList pChar
>     , testKreb "Bool" $
>         prop_RunLengthEncoding_fromFreqList_toFreqList pBool
>     ]

For fun we can also test some specific examples for `fromFreqList` to check our intuition.

> prop_RunLengthEncoding_fromFreqList_examples
>   :: forall a
>    . ( Eq a )
>   => Proxy a
>   -> [(Integer, a)]
>   -> RunLengthEncoding a
>   -> Check
> prop_RunLengthEncoding_fromFreqList_examples _ xs ys =
>   check $
>     ys == fromFreqList xs
> 
> test_RunLengthEncoding_fromFreqList_examples :: TestTree
> test_RunLengthEncoding_fromFreqList_examples =
>   testGroup "fromFreqList examples"
>     [ testKrebCases "Char"
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
>     , localOption (KrebCheckTests 200)
>         $ test_RunLengthEncoding_Monoid
>     , localOption (KrebCheckTests 200)
>         $ test_RunLengthEncoding_Foldable
>     , test_RunLengthEncoding_conversion
>     ]
