---
title: Kreb.Struct.FingerTree.Test
---



> {-# LANGUAGE
>     MultiParamTypeClasses
>   , ScopedTypeVariables
> #-}
> 
> module Kreb.Struct.FingerTree.Test (
>     test_FingerTree
>   , Tup(..)
> ) where
> 
> import Data.Proxy
> import Data.Foldable
> 
> import Test.Tasty
> 
> import Kreb.Check
> import Kreb.Struct.Valued
> import Kreb.Struct.FingerTree



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
>       , test_Foldable_laws_with
>           (fold :: FingerTree Count Bool -> Bool)
>           (foldMap :: forall u n . (Monoid n) => (u -> n) -> FingerTree Count u -> n)
>           (foldr :: (Bool -> Bool -> Bool) -> Bool -> FingerTree Count Bool -> Bool)
>       , test_FoldableFunctor_laws_with
>           (fmapFT :: (Bool -> Bool) -> FingerTree Count Bool -> FingerTree Count Bool)
>           (fmapFT :: (Bool -> Bool) -> FingerTree Count Bool -> FingerTree Count Bool)
>           (fold :: FingerTree Count Bool -> Bool)
>           (foldMap :: forall u n . (Monoid n) => (u -> n) -> FingerTree Count u -> n)
>       ]
>     ]



> test_FingerTree_properties
>   :: forall m a
>    . ( Eq a, Show a, Arb a, Prune a, MakeTo a, Valued m a )
>   => String -> Proxy a -> Proxy m -> TestTree
> test_FingerTree_properties label _ _ =
>   let title = "FingerTree (" ++ label ++ ")"
>   in testGroup title
>     [ testKreb
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
>     ]








Dummy Types
-----------

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
> instance Valued Tup (ZZ a) where
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


> {-


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






> 









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






> -}
