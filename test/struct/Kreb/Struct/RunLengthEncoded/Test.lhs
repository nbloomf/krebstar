---
title: Kreb.Struct.RunLengthEncoded.Test
---

> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE FlexibleContexts #-}
> 
> module Kreb.Struct.RunLengthEncoded.Test (
>     test_RunLengthEncoded
> ) where
> 
> import Data.Proxy
> import Data.Foldable
> 
> import Test.Tasty
> 
> import Kreb.Check
> import Kreb.Struct.RunLengthEncoded



> instance Semigroup Int where
>   (<>) = (+)
> 
> instance Monoid Int where
>   mempty = 0

> test_RunLengthEncoded :: TestTree
> test_RunLengthEncoded =
>   testGroup "RunLengthEncoded"
>     [ testGroup "Laws"
>       [ testGroup "RunSize is a monoid"
>         [ test_Semigroup_laws (Proxy :: Proxy RunSize)
>         , test_Monoid_laws (Proxy :: Proxy RunSize)
>         ]
> 
>       , testGroup "Run is a Functor"
>         [ test_Functor_laws (Proxy :: Proxy Run)
>             (Proxy :: Proxy Int) (Proxy :: Proxy Int) (Proxy :: Proxy Int)
>         ]
> 
>       , testGroup "Functor laws for RunLengthEncoded"
>         [ test_Functor_laws (Proxy :: Proxy RunLengthEncoded)
>             (Proxy :: Proxy Int) (Proxy :: Proxy Int) (Proxy :: Proxy Int)
>         , test_Functor_laws (Proxy :: Proxy RunLengthEncoded)
>             (Proxy :: Proxy Char) (Proxy :: Proxy Char) (Proxy :: Proxy Char)
>         ]
> 
>       , testGroup "Foldable laws"
>         [ test_Foldable_laws (Proxy :: Proxy (RunLengthEncoded))
>             (Proxy :: Proxy Int) (Proxy :: Proxy Int) (Proxy :: Proxy Int)
>         , test_FoldableFunctor_laws (Proxy :: Proxy (RunLengthEncoded))
>             (Proxy :: Proxy Int) (Proxy :: Proxy Int) (Proxy :: Proxy Int)
>         ]
> 
>       , testGroup "Monoid laws for RunLengthEncoded"
>         [ test_Semigroup_laws (Proxy :: Proxy (RunLengthEncoded Int))
>         , test_Monoid_laws (Proxy :: Proxy (RunLengthEncoded Int))
>         ]
>       ]
> 
>     , testGroup "Properties"
>       [ test_RunLengthEncoded_properties
>           "Char" (Proxy :: Proxy Char)
>       , test_RunLengthEncoded_properties
>           "Int" (Proxy :: Proxy Int)
>       ]
>     ]



> test_RunLengthEncoded_properties
>   :: forall a
>    . ( Eq a, Show a, Arb a, Prune a, MakeTo a, CoArb a )
>   => String -> Proxy a -> TestTree
> test_RunLengthEncoded_properties label _ =
>   let title = "RunLengthEncoded properties (" ++ label ++ ")"
>   in testGroup title
>     [ testKreb
>         "fromRuns (toRuns as) == as" $
>         \(as :: RunLengthEncoded a) ->
>           claimEqual
>             (as)
>             (fromRuns (toRuns as))
> 
>     , testKreb
>         "isSingleton (singleton a) == True" $
>         \(a :: a) ->
>           claimTrue (isSingleton (singleton a))
> 
>     , testKreb
>         "toList (fromList as) == as" $
>         \(as :: [a]) ->
>           claimEqual
>             (as)
>             (toList (fromList as))
> 
>     , testKreb
>         "fromList (toList as) == as" $
>         \(as :: RunLengthEncoded a) ->
>           claimEqual
>             (as)
>             (fromList (toList as))
> 
>     , testKreb
>         "cons a (fromList as) == fromList (a:as)" $
>         \(a :: a) (as :: [a]) ->
>           claimEqual
>             (cons a (fromList as))
>             (fromList (a:as))
> 
>     , testKreb
>         "toList (cons a as) == a : (toList as)" $
>         \(a :: a) (as :: RunLengthEncoded a) ->
>           claimEqual
>             (toList (cons a as))
>             (a : (toList as))
> 
>     , testKreb
>         "cons a as === (singleton a) <> as" $
>         \(a :: a) (as :: RunLengthEncoded a) ->
>           claimEqual
>             (cons a as)
>             ((singleton a) <> as)
> 
>     , testKreb
>         "Just (a, as) == uncons (cons a as)" $
>         \(a :: a) (as :: RunLengthEncoded a) ->
>           claimEqual
>             (Just (a, as))
>             (uncons (cons a as))
> 
>     , testKreb
>         "snoc a (fromList as) == fromList (as ++ [a])" $
>         \(a :: a) (as :: [a]) ->
>           claimEqual
>             (snoc a (fromList as))
>             (fromList (as ++ [a]))
> 
>     , testKreb
>         "toList (snoc a as) == (toList as) ++ [a]" $
>         \(a :: a) (as :: RunLengthEncoded a) ->
>           claimEqual
>             (toList (snoc a as))
>             ((toList as) ++ [a])
> 
>     , testKreb
>         "snoc a as === as <> (singleton a)" $
>         \(a :: a) (as :: RunLengthEncoded a) ->
>           claimEqual
>             (snoc a as)
>             (as <> (singleton a))
> 
>     , testKreb
>         "Just (a, as) == unsnoc (snoc a as)" $
>         \(a :: a) (as :: RunLengthEncoded a) ->
>           claimEqual
>             (Just (a, as))
>             (unsnoc (snoc a as))
> 
>     , testKreb
>         "cons a (snoc b as) == snoc b (cons a as)" $
>         \(a :: a) (b :: a) (as :: RunLengthEncoded a) ->
>           claimEqual
>             (cons a (snoc b as))
>             (snoc b (cons a as))
>     ]
