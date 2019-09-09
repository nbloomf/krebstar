> {-# LANGUAGE
>     TypeFamilies
>   , ScopedTypeVariables
>   , FlexibleInstances
> #-}

> module Kreb.Check.Test.Suite (
>     kreb_check_test_suite
> ) where

> import Data.Proxy

> import Test.Tasty

> import Kreb.Check


> kreb_check_test_suite :: TestTree
> kreb_check_test_suite =
>   testGroup "All Tests"
>     [ testGroup "Atomic properties"
>       [ testKreb "True" True
> 
>       , testKreb "accept" accept
>       ]
> 
>     , testGroup "Algebraic properties"
>       [ let neg = negate :: Int -> Int in
>         testGroup "negate on Int"
>           [ testKreb "involutive" $ check_prop_involutive neg
>           ]
> 
>       , let plus = (+) :: Int -> Int -> Int in
>         testGroup "(+) on Int"
>           [ testKreb "left neutral" $ check_prop_left_neutral 0 plus
>           , testKreb "right neutral" $ check_prop_right_neutral 0 plus
>           , testKreb "commutative" $ check_prop_commutative plus
>           , testKreb "associative" $ check_prop_associative plus
>           ]
> 
>       , let times = (*) :: Int -> Int -> Int in
>         testGroup "(*) on Int"
>           [ testKreb "left neutral" $ check_prop_left_neutral 1 times
>           , testKreb "right neutral" $ check_prop_right_neutral 1 times
>           , testKreb "left absorbing" $ check_prop_left_absorbing 0 times
>           , testKreb "right absorbing" $ check_prop_right_absorbing 0 times
>           , testKreb "commutative" $ check_prop_commutative times
>           , testKreb "associative" $ check_prop_associative times
>           ]
>        ]
> 
>  --   , testKreb "func" $ \(f :: Fun Int Int) (g :: Fun Int Int) xs ->
>  --       (map (apFun g) $ map (apFun f) xs) == (map ((apFun g) . (apFun f)) xs)
> 
>     , testGroup "Class Laws"
>       [ testGroup "Semigroup"
>         [ let p = Proxy :: Proxy ()
>           in test_Semigroup_laws p
> 
>         , let p = Proxy :: Proxy [()]
>           in test_Semigroup_laws p
>         ]
>       ]
> 
>     , testGroup "State Machine"
>       [ testKreb "Int" $ checkTransitions condInt
>       ]
> 
>     , testGroup "Adjectives"
>       [ testKreb "Positive" $ \(Positive (k :: Int)) -> k > 0
>       ]
>     ]


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
