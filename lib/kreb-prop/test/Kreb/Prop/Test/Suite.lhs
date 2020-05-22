> {-# LANGUAGE
>     TypeFamilies
>   , ScopedTypeVariables
>   , FlexibleInstances
>   , DataKinds
>   , OverloadedStrings
> #-}

> module Kreb.Prop.Test.Suite (
>     kreb_prop_test_suite
> ) where

> import Data.Proxy

> import Test.Tasty

> import Kreb.Format (reflow)

> import Kreb.Prop


> kreb_prop_test_suite :: TestTree
> kreb_prop_test_suite =
>   testGroup "All Tests"
>     [ testGroup "Atomic properties"
>       [ krebProp "True" True
> 
>       , krebProp "accept" accept
>       ]
> 
>     , testGroup "Algebraic properties"
>       [ let neg = negate :: Int -> Int in
>         testGroup "negate on Int"
>           [ krebProp "involutive" $ check_prop_involutive neg
>           ]
> 
>       , let plus = (+) :: Int -> Int -> Int in
>         testGroup "(+) on Int"
>           [ krebProp "left neutral" $ check_prop_left_neutral 0 plus
>           , krebProp "right neutral" $ check_prop_right_neutral 0 plus
>           , krebProp "commutative" $ check_prop_commutative plus
>           , krebProp "associative" $ check_prop_associative plus
>           ]
> 
>       , let times = (*) :: Int -> Int -> Int in
>         testGroup "(*) on Int"
>           [ krebProp "left neutral" $ check_prop_left_neutral 1 times
>           , krebProp "right neutral" $ check_prop_right_neutral 1 times
>           , krebProp "left absorbing" $ check_prop_left_absorbing 0 times
>           , krebProp "right absorbing" $ check_prop_right_absorbing 0 times
>           , krebProp "commutative" $ check_prop_commutative times
>           , krebProp "associative" $ check_prop_associative times
>           ]
>        ]
> 
>     , testGroup "Functions"
>       [ krebProp "concatMap" $
>           \(f :: Fun Int [Int]) (xs :: [Int]) ->
>             claimEqual
>               (concatMap (apFun f) xs)
>               (concat $ map (apFun f) xs)
> 
>       , krebProp "Definition of composition" $
>           \(f :: Fun Int Int) (g :: Fun Int Int) xs ->
>             claimEqual
>               (map (apFun g) $ map (apFun f) xs)
>               (map ((apFun g) . (apFun f)) xs)
>       ]
> 
>     , testGroup "Class Laws"
>       [ testGroup "Semigroup"
>         [ let p = Proxy :: Proxy ()
>           in testGroup "()"
>           [ test_Eq_laws p
>           , test_Semigroup_laws p
>           , test_Monoid_laws p
>           ]
> 
>         , let p = Proxy :: Proxy [()]
>           in testGroup "[()]"
>           [ test_Eq_laws p
>           , test_Semigroup_laws p
>           , test_Monoid_laws p
>           ]
>         ]
>       ]
> 
>     , testGroup "Adjectives"
>       [ krebProp "Positive" $
>           \(Positive k :: Positive Int) ->
>             k > 0
> 
>       , krebProp "NonNegative" $
>           \(NonNegative k :: NonNegative Int) ->
>             k >= 0
> 
>       , krebProp "AtMost 5" $
>           \(AtMost k :: AtMost 5 Int) ->
>             explain "Expecting a number less than or equal to 5." $
>               claimTrue $ (k <= 5)
> 
>       , krebProp "Between 1 10" $
>           \(Between k :: Between 1 10 Int) ->
>             explain "Expecting a number between 1 and 10." $
>               claimTrue $ (1 <= k) && (k <= 10)
>       ]
>     ]

> {-

> instance StateMachine Int where
>   data Transition Int
>     = T_Add Int
>     | T_Mul Int
>     | T_Sub Int
>     deriving (Show)
> 
>   transition z m = case z of
>     T_Add k -> m + k
>     T_Mul k -> m * k
>     T_Sub k -> m - k
> 
> instance Prune (Transition Int) where
>   prune z = case z of
>     T_Add k -> map T_Add $ prune k
>     T_Mul k -> map T_Mul $ prune k
>     T_Sub k -> map T_Sub $ prune k
> 
> instance Arb (Transition Int) where
>   arb = pickFrom3
>     ( T_Add <$> arb
>     , T_Mul <$> arb
>     , T_Sub <$> arb
>     )



> condInt :: Cond Int
> condInt = Cond $ \t -> case t of
>   T_Add k ->
>     [ Impl
>       { elucidate = "Addition works as expected"
>       , antecedent = \m -> Just (m, "")
>       , consequent = \mold mnew ->
>           if mnew == mold + k then Nothing else Just "Unexpected addition result"
>       }
>     ]
>   T_Mul k ->
>     [ Impl
>       { elucidate = ""
>       , antecedent = \m -> if m == 0 then Nothing else Just ((), "m nonzero")
>       , consequent = \() mnew ->
>           if (k /= 0)
>             then if (mnew /= 0) then Nothing else Just ""
>             else Nothing
>       }
>     ]
>   T_Sub k ->
>     [
>     ]

> -}
