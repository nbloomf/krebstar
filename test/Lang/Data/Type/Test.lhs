> {-# LANGUAGE
>     ScopedTypeVariables
>   , FlexibleInstances
> #-}

> module Lang.Data.Type.Test (
>     test_Type
> ) where

> import Prelude hiding (Word)
> import GHC.Stack

> import qualified Data.Map as M
> import qualified Data.Set as S
> import qualified Data.List as L

> import Data.Proxy

> import Test.QuickCheck
> import Test.Tasty
> import Test.Tasty.QuickCheck

> import Lang.Data.Expr
> import Lang.Data.Type

> import Ned.Data.FingerTree.Test

> test_Type :: TestTree
> test_Type = let k = 10000 in
>   testGroup "QType"
>     [ test_Vars_Semigroup k
>     , test_Vars_Seminearring k
> 
>     , test_Eq k (Proxy :: Proxy Arrow)
>     , test_Eq k (Proxy :: Proxy Stack)
>     , test_Eq k (Proxy :: Proxy Type)
> 
>     , test_Subs_augment k
>     , test_Subs_Semigroup k
> 
>     , test_Eq_rename k (Proxy :: Proxy Arrow)
>     , test_Eq_rename k (Proxy :: Proxy Stack)
>     , test_Eq_rename k (Proxy :: Proxy Type)
> 
>     , test_HasVars_Subs k (Proxy :: Proxy Arrow)
>     , test_HasVars_Subs k (Proxy :: Proxy Stack)
>     , test_HasVars_Subs k (Proxy :: Proxy Type)
> 
>     , test_Unify k (Proxy :: Proxy Arrow)
>     , test_Unify k (Proxy :: Proxy Stack)
>     , test_Unify k (Proxy :: Proxy Type)
> 
>     , test_Unify_examples
> 
>     , test_Match k (Proxy :: Proxy Arrow)
>     , test_Match k (Proxy :: Proxy Stack)
>     , test_Match k (Proxy :: Proxy Type)
> 
>     , test_Match_examples
> 
>     , test_GenericInstance k
> 
>     , test_GenericInstance_examples
> 
>     , test_Subsume k
> 
>     , test_Subsume_examples
> 
>     , test_ComposeArrows_examples
> 
>     , test_Infer_examples
>     ]

> instance Arbitrary Arrow where
>   arbitrary = ForAll
>     <$> scale (`div` 3) arbitrary
>     <*> scale (`div` 3) arbitrary
>     <*> scale (`div` 3) arbitrary
> 
>   shrink (ForAll xs s1 s2) = do
>     xs' <- shrink xs
>     s1' <- shrink s1
>     s2' <- shrink s2
>     return $ ForAll xs' s1' s2'
> 
> instance Arbitrary Stack where
>   arbitrary = do
>     k <- getSize
>     p <- arbitrary
>     if p || (k <= 0)
>       then Stack <$> arbitrary <*> pure []
>       else do
>         m <- choose (0,10)
>         Stack
>           <$> arbitrary
>           <*> vectorOf m (scale (`div` (m+2)) arbitrary)
> 
>   shrink (Stack x ts) = do
>     ts' <- shrink ts
>     return $ Stack x ts'
> 
> instance Arbitrary Type where
>   arbitrary = do
>     k <- getSize
>     p <- arbitrary
>     if p || (k <= 0)
>       then oneof
>         [ TyCon <$> arbitrary
>         , TyVar <$> arbitrary
>         ]
>       else oneof
>         [ TyCon <$> arbitrary
>         , TyVar <$> arbitrary
>         , TyApp
>             <$> scale (`div` 2) arbitrary
>             <*> scale (`div` 2) arbitrary
>         , TyArr
>             <$> scale (`div` 2) arbitrary
>         ]
> 
>   shrink z = case z of
>     TyApp x y -> [x, y] ++ do
>       x' <- shrink x
>       y' <- shrink y
>       return $ TyApp x' y'
>     TyArr a -> map TyArr $ shrink a
>     _ -> []
> 
> instance Arbitrary (V Type) where
>   arbitrary = do
>     c <- elements ['t', 'u', 'v']
>     k <- choose (0 :: Int, 5)
>     return $ V (c : show k)
> 
> instance Arbitrary (C Type) where
>   arbitrary = do
>     c <- elements ['a', 'b', 'c']
>     k <- choose (0 :: Int, 5)
>     return $ C (c : show k)
> 
> instance Arbitrary (V Stack) where
>   arbitrary = do
>     c <- elements ['R', 'S', 'T']
>     k <- choose (0 :: Int, 5)
>     return $ V (c : show k)
> 
> instance Arbitrary Vars where
>   arbitrary = do
>     mt <- choose (0,5)
>     mv <- choose (0,5)
>     ts <- L.nub <$> vectorOf mt (scale (`div` (mt+mv+2)) arbitrary)
>     vs <- L.nub <$> vectorOf mv (scale (`div` (mt+mv+2)) arbitrary)
>     return $ Vars ts vs
> 
>   shrink (Vars ts vs) = do
>     ts' <- shrink ts
>     vs' <- shrink vs
>     return $ Vars ts' vs'
> 
> instance Arbitrary Subs where
>   arbitrary = Subs
>     <$> (do
>           ma <- choose (0,10)
>           M.fromList <$> vectorOf ma (scale (`div` (ma+1)) arbitrary))
>     <*> (do
>           mb <- choose (0,10)
>           M.fromList <$> vectorOf mb (scale (`div` (mb+1)) arbitrary))
> 
>   shrink (Subs st ty) = do
>     st' <- shrink st
>     ty' <- shrink ty
>     return $ Subs st' ty'





> test_Vars_Semigroup :: Int -> TestTree
> test_Vars_Semigroup k =
>   testGroup "Vars Semigroup"
>     [ localOption (QuickCheckTests k) $
>       testProperty "x <> mempty == x" $
>         \(x :: Vars) ->
>           x === (x <> mempty)
> 
>     , localOption (QuickCheckTests k) $
>       testProperty "mempty <> x == x" $
>         \(x :: Vars) ->
>           x === (mempty <> x)
> 
>     , localOption (QuickCheckTests k) $
>       testProperty "x <> (y <> z) == (x <> y) <> z" $
>         \(x :: Vars) y z ->
>           (x <> (y <> z)) === ((x <> y) <> z)
>     ]

> test_Vars_Seminearring :: Int -> TestTree
> test_Vars_Seminearring k =
>   testGroup "Vars Seminearring"
>     [ localOption (QuickCheckTests k) $
>       testProperty "x <> x == x" $
>         \(x :: Vars) ->
>           x === (x <> x)
> 
>     , localOption (QuickCheckTests k) $
>       testProperty "x meet x == x" $
>         \(x :: Vars) ->
>           x === (meet x x)
> 
>     , localOption (QuickCheckTests k) $
>       testProperty "x meet (y meet z) == (x meet y) meet z" $
>         \(x :: Vars) y z ->
>           (meet x (meet y z)) === (meet (meet x y) z)
> 
>     , localOption (QuickCheckTests k) $
>       testProperty "(x <> y) meet z == (x meet z) <> (y meet z)" $
>         \(x :: Vars) y z ->
>           (meet (x <> y) z) === ((meet x z) <> (meet y z))
> 
>     , localOption (QuickCheckTests k) $
>       testProperty "x meet mempty == mempty" $
>         \(x :: Vars) ->
>           mempty === (meet x mempty)
> 
>     , localOption (QuickCheckTests k) $
>       testProperty "mempty meet x == mempty" $
>         \(x :: Vars) ->
>           mempty === (meet mempty x)
>     ]

> test_Eq
>   :: forall t
>    . ( Eq t, Show t, Arbitrary t )
>   => Int -> Proxy t -> TestTree
> test_Eq k _ =
>   testGroup "Eq"
>     [ localOption (QuickCheckTests k) $
>       testProperty "x == x" $
>         \(x :: t) ->
>           x === x
> 
>     , localOption (QuickCheckTests k) $
>       testProperty "(x == y) === (y == x)" $
>         \(x :: t) y ->
>           (x == y) === (y == x)
> 
>     , localOption (QuickCheckTests k) $
>       testProperty "if (x == y) && (y == z) then (x == z)" $
>         \(x :: t) y z ->
>           if (x == y) && (y == z)
>             then x === z
>             else property True
>     ]

> test_Subs_augment :: Int -> TestTree
> test_Subs_augment k =
>   testGroup "Subs augment"
>     [ localOption (QuickCheckTests k) $
>       testProperty "augment s mempty == s" $
>         \(s :: Subs) ->
>           s === (augment s mempty)
> 
>     , localOption (QuickCheckTests k) $
>       testProperty "augment mempty s == s" $
>         \(s :: Subs) ->
>           s === (augment mempty s)
> 
>     , localOption (QuickCheckTests k) $
>       testProperty "augment s s == s" $
>         \(s :: Subs) ->
>           s === (augment s s)
> 
>     , localOption (QuickCheckTests k) $
>       testProperty "augment is associative" $
>         \(s1 :: Subs) s2 s3 ->
>           (augment s1 (augment s2 s3))
>            === (augment (augment s1 s2) s3)
>     ]

> test_Subs_Semigroup :: Int -> TestTree
> test_Subs_Semigroup k =
>   testGroup "Subs Semigroup"
>     [ localOption (QuickCheckTests k) $
>       testProperty "x <> mempty == x" $
>         \(x :: Subs) ->
>           x === (x <> mempty)
> 
>     , localOption (QuickCheckTests k) $
>       testProperty "mempty <> x == x" $
>         \(x :: Subs) ->
>           x === (mempty <> x)
> 
>     , localOption (QuickCheckTests k) $
>       testProperty "x <> (y <> z) == (x <> y) <> z" $
>         \(x :: Subs) y z ->
>           (x <> (y <> z)) === ((x <> y) <> z)
>     ]


> test_Eq_rename
>   :: forall t
>    . ( Eq t, Show t, Arbitrary t, Normalize t, GetVars t, Rename t )
>   => Int -> Proxy t -> TestTree
> test_Eq_rename k _ =
>   testGroup "Eq rename"
>     [ localOption (QuickCheckTests k) $
>       testProperty "x == normalize x" $
>         \(x :: t) ->
>           x === (normalize x)
> 
>     , localOption (QuickCheckTests k) $
>       testProperty "x == renameBinders x" $
>         \(x :: t) ->
>           x === (renameBinders x)
>     ]

> test_HasVars_Subs
>   :: forall t
>    . ( GetVars t, Rename t, Eq t, Show t, Arbitrary t )
>   => Int -> Proxy t -> TestTree
> test_HasVars_Subs k _ =
>   testGroup "HasVars Subs"
>     [ localOption (QuickCheckTests k) $
>       testProperty "subs mempty x == x" $
>         \(x :: t) ->
>           x === (subs mempty x)
> 
>     , localOption (QuickCheckTests k) $
>       testProperty "subs s1 (subs s2 x) == subs (s1 <> s2) x" $
>         \(x :: t) s1 s2 ->
>           (subs s1 (subs s2 x)) === (subs (s1 <> s2) x)
>     ]


> test_Unify
>   :: forall t
>    . ( Eq t, Show t, Arbitrary t, Unify t, Rename t, GetVars t, Normalize t )
>   => Int -> Proxy t -> TestTree
> test_Unify k _ =
>   testGroup "Unify"
>     [ localOption (QuickCheckTests k) $
>       testProperty "subs (unify x y) x == subs (unify x y) y" $
>         \(x :: t) y ->
>           case runInfer (emptyTypeEnv $ const idArrow) $ unify x y of
>             Left _ -> True
>             Right s -> if (subs s x) == (subs s y)
>               then True
>               else error $ unlines
>                 [ show x, show y, show s, show (subs s x), show (subs s y) ]
> 
>     , localOption (QuickCheckTests k) $
>       testProperty "unify x x" $
>         \(x :: t) ->
>           case runInfer (emptyTypeEnv $ const idArrow) $ unify x x of
>             Left err -> error $ unlines [ show err ]
>             Right _ -> True
> 
>     , localOption (QuickCheckTests k) $
>       testProperty "unify x (renameBinders x)" $
>         \(x :: t) ->
>           case runInfer (emptyTypeEnv $ const idArrow) $ unify x (renameBinders x) of
>             Left err -> error $ unlines [ show err ]
>             Right _ -> True
> 
>     , localOption (QuickCheckTests k) $
>       testProperty "unify x (normalize x)" $
>         \(x :: t) ->
>           case runInfer (emptyTypeEnv $ const idArrow) $ unify x (normalize x) of
>             Left err -> error $ unlines [ show err ]
>             Right _ -> True
> 
>     , localOption (QuickCheckTests k) $
>       testProperty "unify x y == unify y x" $
>         \(x :: t) y ->
>           let
>             u = runInfer (emptyTypeEnv $ const idArrow) $ unify x y
>             v = runInfer (emptyTypeEnv $ const idArrow) $ unify y x
>           in case (u,v) of
>             (Left err, Right _) -> error $ unlines [ show err ]
>             (Right _, Left err) -> error $ unlines [ show err ]
>             _ -> True
>     ]

> test_Subsume
>   :: Int -> TestTree
> test_Subsume k =
>   testGroup "Subsume"
>     [ localOption (QuickCheckTests k) $
>       testProperty "subs (subsume x y) y <<< subs (subsume x y) x" $
>         \x y ->
>           case runInfer (emptyTypeEnv $ const idArrow) $ subsume x y of
>             Left _ -> True
>             Right s -> if (subs s y) <<< (subs s x)
>               then True
>               else error $ unlines
>                 [ show x, show y, show s, show (subs s x), show (subs s y) ]
>     ]

> test_Match
>   :: forall t
>    . ( Eq t, Show t, Arbitrary t, Match t, Rename t, GetVars t, Normalize t )
>   => Int -> Proxy t -> TestTree
> test_Match k _ =
>   testGroup "Match"
>     [ localOption (QuickCheckTests k) $
>       testProperty "subs (match x y) x == y" $
>         \(x :: t) y ->
>           case runInfer (emptyTypeEnv $ const idArrow) $ match x y of
>             Left _ -> True
>             Right s -> if (subs s x) == y
>               then True
>               else error $ unlines
>                 [ show x, show y, show s, show (subs s x) ]
> 
>     , localOption (QuickCheckTests k) $
>       testProperty "match x x" $
>         \(x :: t) ->
>           case runInfer (emptyTypeEnv $ const idArrow) $ match x x of
>             Left err -> error $ show err
>             Right s -> s === mempty
> 
>     , localOption (QuickCheckTests k) $
>       testProperty "match x (renameBinders x)" $
>         \(x :: t) ->
>           case runInfer (emptyTypeEnv $ const idArrow) $ match x (renameBinders x) of
>             Left err -> error $ show err
>             Right _ -> True
> 
>     , localOption (QuickCheckTests k) $
>       testProperty "match x (normalize x)" $
>         \(x :: t) ->
>           case runInfer (emptyTypeEnv $ const idArrow) $ match x (normalize x) of
>             Left err -> error $ show err
>             Right _ -> True
> 
>     , localOption (QuickCheckTests k) $
>       testProperty "match x (subs s x)" $
>         \(x :: t) s ->
>           (mempty == meet (getFreeVars x) (getFreeVars s)) ==>
>           case runInfer (emptyTypeEnv $ const idArrow) $ match x (subs s x) of
>             Left err -> error $ show err
>             Right _ -> True
>     ]



> prop_Unify_examples
>   :: forall t
>    . ( Eq t, Show t, Unify t, Rename t, GetVars t )
>   => (t, t, Either Err Subs)
>   -> Property
> prop_Unify_examples (x, y, r) =
>   r === runInfer (emptyTypeEnv $ const idArrow) (unify x y)

> test_Unify_examples :: TestTree
> test_Unify_examples =
>   testGroup "Unify examples"
>     [ testCases
>       prop_Unify_examples
>       [ ( "Type #1: x and x"
>         , ( TyVar (V "x")
>           , TyVar (V "x")
>           , Right mempty
>           )
>         )
> 
>       , ( "Type #2: x and y"
>         , ( TyVar (V "x")
>           , TyVar (V "y")
>           , Right $ Subs mempty (M.fromList [(V "x", TyVar (V "y"))])
>           )
>         )
> 
>       , ( "Type #3: c and c"
>         , ( TyCon (C "c")
>           , TyCon (C "c")
>           , Right mempty
>           )
>         )
> 
>       , ( "Type #4: c and d"
>         , ( TyCon (C "c")
>           , TyCon (C "d")
>           , Left $ CannotUnifyTypeCon (C "c") (C "d")
>           )
>         )
> 
>       , ( "Type #5: c x and c x"
>         , ( TyApp (TyCon (C "c")) (TyVar (V "x"))
>           , TyApp (TyCon (C "c")) (TyVar (V "x"))
>           , Right mempty
>           )
>         )
> 
>       , ( "Type #6: c x and c y"
>         , ( TyApp (TyCon (C "c")) (TyVar (V "x"))
>           , TyApp (TyCon (C "c")) (TyVar (V "y"))
>           , Right $ Subs mempty (M.fromList [(V "x", TyVar (V "y"))])
>           )
>         )
> 
>       , ( "Type #7: c x and d x"
>         , ( TyApp (TyCon (C "c")) (TyVar (V "x"))
>           , TyApp (TyCon (C "d")) (TyVar (V "x"))
>           , Left $ CannotUnifyTypeCon (C "c") (C "d")
>           )
>         )
> 
>       , ( "Type #8: c x and d y"
>         , ( TyApp (TyCon (C "c")) (TyVar (V "x"))
>           , TyApp (TyCon (C "d")) (TyVar (V "y"))
>           , Left $ CannotUnifyTypeCon (C "c") (C "d")
>           )
>         )
> 
>       , ( "Type #9: x and c x"
>         , ( TyVar (V "x")
>           , TyApp (TyCon (C "c")) (TyVar (V "x"))
>           , Left $ OccursCheckType (V "x") (TyApp (TyCon (C "c")) (TyVar (V "x")))
>           )
>         )
> 
>       , ( "Type #10: x and c y"
>         , ( TyVar (V "x")
>           , TyApp (TyCon (C "c")) (TyVar (V "y"))
>           , Right $ Subs mempty (M.fromList [(V "x", TyApp (TyCon (C "c")) (TyVar (V "y")))])
>           )
>         )
> 
>       , ( "Type #11: x c and x c"
>         , ( TyApp (TyVar (V "x")) (TyCon (C "c"))
>           , TyApp (TyVar (V "x")) (TyCon (C "c"))
>           , Right mempty
>           )
>         )
> 
>       , ( "Type #12: x c and y c"
>         , ( TyApp (TyVar (V "x")) (TyCon (C "c"))
>           , TyApp (TyVar (V "y")) (TyCon (C "c"))
>           , Right $ Subs mempty (M.fromList [(V "x", TyVar (V "y"))])
>           )
>         )
> 
>       , ( "Type #13: x c and x d"
>         , ( TyApp (TyVar (V "x")) (TyCon (C "c"))
>           , TyApp (TyVar (V "x")) (TyCon (C "d"))
>           , Left $ CannotUnifyTypeCon (C "c") (C "d")
>           )
>         )
> 
>       , ( "Type #14: x c and y d"
>         , ( TyApp (TyVar (V "x")) (TyCon (C "c"))
>           , TyApp (TyVar (V "y")) (TyCon (C "d"))
>           , Left $ CannotUnifyTypeCon (C "c") (C "d")
>           )
>         )
>       ]
> 
>     , testCases
>       prop_Unify_examples
>       [ ( "Stack #1: S and S"
>         , ( Stack (V "S") []
>           , Stack (V "S") []
>           , Right mempty
>           )
>         )
> 
>       , ( "Stack #2: S and T"
>         , ( Stack (V "S") []
>           , Stack (V "T") []
>           , Right $ Subs (M.fromList [(V "S", Stack (V "T") [])]) mempty
>           )
>         )
> 
>       , ( "Stack #3: S and S a"
>         , ( Stack (V "S") []
>           , Stack (V "S") [TyVar (V "a")]
>           , Left $ OccursCheckStack (V "S") (Stack (V "S") [TyVar (V "a")])
>           )
>         )
> 
>       , ( "Stack #4: S and T a"
>         , ( Stack (V "S") []
>           , Stack (V "T") [TyVar (V "a")]
>           , Right $ Subs (M.fromList [(V "S", Stack (V "T") [TyVar (V "a")])]) mempty
>           )
>         )
> 
>       , ( "Stack #5: T a and S"
>         , ( Stack (V "T") [TyVar (V "a")]
>           , Stack (V "S") []
>           , Right $ Subs (M.fromList [(V "S", Stack (V "T") [TyVar (V "a")])]) mempty
>           )
>         )
> 
>       , ( "Stack #6: S a and S a"
>         , ( Stack (V "S") [TyVar (V "a")]
>           , Stack (V "S") [TyVar (V "a")]
>           , Right mempty
>           )
>         )
> 
>       , ( "Stack #7: S a and S b"
>         , ( Stack (V "S") [TyVar (V "a")]
>           , Stack (V "S") [TyVar (V "b")]
>           , Right $ Subs mempty (M.fromList [(V "a", TyVar (V "b"))])
>           )
>         )
> 
>       , ( "Stack #8: S a and S a a"
>         , ( Stack (V "S") [TyVar (V "a")]
>           , Stack (V "S") [TyVar (V "a"), TyVar (V "a")]
>           , Left $ OccursCheckStack (V "S") (Stack (V "S") [TyVar (V "a")])
>           )
>         )
> 
>       , ( "Stack #9: T a and S a a"
>         , ( Stack (V "T") [TyVar (V "a")]
>           , Stack (V "S") [TyVar (V "a"), TyVar (V "a")]
>           , Right $ Subs (M.fromList [(V "T", Stack (V "S") [TyVar (V "a")])]) mempty
>           )
>         )
> 
>       , ( "Stack #10: T a b and S a a"
>         , ( Stack (V "T") [TyVar (V "a"), TyVar (V "b")]
>           , Stack (V "S") [TyVar (V "a"), TyVar (V "a")]
>           , Right $ Subs (M.fromList [(V "T", Stack (V "S") [])]) (M.fromList [(V "b", TyVar (V "a"))])
>           )
>         )
> 
>       , ( "Stack #11: S a and S (c b)"
>         , ( Stack (V "S") [TyApp (TyCon (C "c")) (TyVar (V "b"))]
>           , Stack (V "S") [TyVar (V "a")]
>           , Right $ Subs mempty (M.fromList [(V "a", TyApp (TyCon (C "c")) (TyVar (V "b")))])
>           )
>         )
> 
>       , ( "Stack #12: S a and S (c a)"
>         , ( Stack (V "S") [TyApp (TyCon (C "c")) (TyVar (V "a"))]
>           , Stack (V "S") [TyVar (V "a")]
>           , Left $ OccursCheckType (V "a") (TyApp (TyCon (C "c")) (TyVar (V "a")))
>           )
>         )
>       ]
>     ]



> prop_Match_examples
>   :: forall t
>    . ( Eq t, Show t, Match t, Rename t, GetVars t )
>   => (t, t, Either Err Subs)
>   -> Property
> prop_Match_examples (x, y, r) =
>   r === runInfer (emptyTypeEnv $ const idArrow) (match x y)

> test_Match_examples :: TestTree
> test_Match_examples =
>   testGroup "Match examples"
>     [ testCases
>       prop_Match_examples
>       [ ( "Type #1: x and x"
>         , ( TyVar (V "x")
>           , TyVar (V "x")
>           , Right mempty
>           )
>         )
> 
>       , ( "Type #2: x and y"
>         , ( TyVar (V "x")
>           , TyVar (V "y")
>           , Right $ Subs mempty (M.fromList [(V "x", TyVar (V "y"))])
>           )
>         )
> 
>       , ( "Type #3: x and c"
>         , ( TyVar (V "x")
>           , TyCon (C "c")
>           , Right $ Subs mempty (M.fromList [(V "x", TyCon (C "c"))])
>           )
>         )
> 
>       , ( "Type #4: c and c"
>         , ( TyCon (C "c")
>           , TyCon (C "c")
>           , Right mempty
>           )
>         )
> 
>       , ( "Type #5: c and x"
>         , ( TyCon (C "c")
>           , TyVar (V "x")
>           , Left $ CannotMatchType (TyCon (C "c")) (TyVar (V "x"))
>           )
>         )
> 
>       , ( "Type #6: c x and c x"
>         , ( TyApp (TyCon (C "c")) (TyVar (V "x"))
>           , TyApp (TyCon (C "c")) (TyVar (V "x"))
>           , Right mempty
>           )
>         )
> 
>       , ( "Type #7: c x and c y"
>         , ( TyApp (TyCon (C "c")) (TyVar (V "x"))
>           , TyApp (TyCon (C "c")) (TyVar (V "y"))
>           , Right $ Subs mempty (M.fromList [(V "x", TyVar (V "y"))])
>           )
>         )
> 
>       , ( "Type #8: c x and c d"
>         , ( TyApp (TyCon (C "c")) (TyVar (V "x"))
>           , TyApp (TyCon (C "c")) (TyCon (C "d"))
>           , Right $ Subs mempty (M.fromList [(V "x", TyCon (C "d"))])
>           )
>         )
>       ]
>     ]

> test_GenericInstance :: Int -> TestTree
> test_GenericInstance k =
>   testGroup "Generic Instance"
>     [ localOption (QuickCheckTests k) $
>       testProperty "x <<< x" $
>         \x ->
>           True === (x <<< x)
> 
>     , localOption (QuickCheckTests k) $
>       testProperty "x <<< y && y <<< x --> x == y" $
>         \x y ->
>           if (x <<< y) && (y <<< x)
>             then x === y
>             else property True
> 
>     , localOption (QuickCheckTests k) $
>       testProperty "x <<< y && y <<< z --> x <<< y" $
>         \x y z ->
>           if (x <<< y) && (y <<< z)
>             then x <<< z
>             else True
>     ]

> prop_GenericInstance_examples
>   :: (Arrow, Arrow, Bool)
>   -> Property
> prop_GenericInstance_examples (x, y, r) =
>   r === (x <<< y)

> test_GenericInstance_examples :: TestTree
> test_GenericInstance_examples =
>   testGroup "Generic Instance examples"
>     [ testCases
>       prop_GenericInstance_examples
>       [ ( "#1: !S. S -> S and !S. S -> S"
>         , ( ForAll (Vars [V "S"] []) (Stack (V "S") []) (Stack (V "S") [])
>           , ForAll (Vars [V "S"] []) (Stack (V "S") []) (Stack (V "S") [])
>           , True
>           )
>         )
> 
>       , ( "#2: !S. S -> S and T -> T"
>         , ( ForAll (Vars [V "S"] []) (Stack (V "S") []) (Stack (V "S") [])
>           , ForAll (Vars [] []) (Stack (V "T") []) (Stack (V "T") [])
>           , True
>           )
>         )
> 
>       , ( "#3: !S. S -> S and T -> U"
>         , ( ForAll (Vars [V "S"] []) (Stack (V "S") []) (Stack (V "S") [])
>           , ForAll (Vars [] []) (Stack (V "T") []) (Stack (V "U") [])
>           , False
>           )
>         )
> 
>       , ( "#4: !S. S -> S and T -> T a"
>         , ( ForAll (Vars [V "S"] []) (Stack (V "S") []) (Stack (V "S") [])
>           , ForAll (Vars [] []) (Stack (V "T") []) (Stack (V "T") [TyVar (V "a")])
>           , False
>           )
>         )
> 
>       , ( "#5: !S. S -> S a and T -> T a"
>         , ( ForAll (Vars [V "S"] []) (Stack (V "S") []) (Stack (V "S") [TyVar (V "a")])
>           , ForAll (Vars [] []) (Stack (V "T") []) (Stack (V "T") [TyVar (V "a")])
>           , True
>           )
>         )
> 
>       , ( "#6: !S x. S -> S x and T -> T y"
>         , ( ForAll (Vars [V "S"] [V "x"]) (Stack (V "S") []) (Stack (V "S") [TyVar (V "x")])
>           , ForAll (Vars [] []) (Stack (V "T") []) (Stack (V "T") [TyVar (V "y")])
>           , True
>           )
>         )
>       ]
>     ]



> prop_Subsume_examples
>   :: forall t
>    . ( Eq t, Show t, Subsume t, Rename t, GetVars t )
>   => (t, t, Either Err Subs)
>   -> Property
> prop_Subsume_examples (x, y, r) =
>   r === runInfer (emptyTypeEnv $ const idArrow) (subsume x y)

> test_Subsume_examples :: TestTree
> test_Subsume_examples =
>   testGroup "Subsume examples"
>     [ testCases
>       prop_Subsume_examples
>       [ ( "#1: !S. S -> S and !S. S -> S"
>         , ( ForAll (Vars [V "S"] []) (Stack (V "S") []) (Stack (V "S") [])
>           , ForAll (Vars [V "S"] []) (Stack (V "S") []) (Stack (V "S") [])
>           , Right mempty
>           )
>         )
> 
>       , ( "#2: !S. S -> S and !. S -> S"
>         , ( ForAll (Vars [] []) (Stack (V "S") []) (Stack (V "S") [])
>           , ForAll (Vars [V "S"] []) (Stack (V "S") []) (Stack (V "S") [])
>           , Right mempty
>           )
>         )
> 
>       , ( "#3: !S. S -> T and !S. S -> U"
>         , ( ForAll (Vars [V "S"] []) (Stack (V "S") []) (Stack (V "T") [])
>           , ForAll (Vars [V "S"] []) (Stack (V "S") []) (Stack (V "U") [])
>           , Right $ Subs (M.fromList [(V "U", Stack (V "T") [])]) mempty
>           )
>         )
> 
>       , ( "#4: !S. S -> S and !S x. S x -> S x"
>         , ( ForAll (Vars [V "S"] [V "x"]) (Stack (V "S") [TyVar (V "x")]) (Stack (V "S") [TyVar (V "x")])
>           , ForAll (Vars [V "S"] []) (Stack (V "S") []) (Stack (V "S") [])
>           , Right mempty
>           )
>         )
>       ]
>     ]



> prop_ComposeArrows_examples
>   :: (Arrow, Arrow, Either Err Arrow)
>   -> Property
> prop_ComposeArrows_examples (x, y, r) =
>   r === runInfer (emptyTypeEnv $ const idArrow) (composeArrows x y)

> test_ComposeArrows_examples :: TestTree
> test_ComposeArrows_examples =
>   testGroup "ComposeArrows examples"
>     [ testCases
>       prop_ComposeArrows_examples
>       [ ( "#1: !S. S -> S and !S. S -> S"
>         , ( ForAll (Vars [V "S"] []) (Stack (V "S") []) (Stack (V "S") [])
>           , ForAll (Vars [V "S"] []) (Stack (V "S") []) (Stack (V "S") [])
>           , Right $ ForAll (Vars [V "S"] []) (Stack (V "S") []) (Stack (V "S") [])
>           )
>         )
> 
>       , ( "#2: !S. S -> S and !T. T -> T"
>         , ( ForAll (Vars [V "S"] []) (Stack (V "S") []) (Stack (V "S") [])
>           , ForAll (Vars [V "T"] []) (Stack (V "T") []) (Stack (V "T") [])
>           , Right $ ForAll (Vars [V "S"] []) (Stack (V "S") []) (Stack (V "S") [])
>           )
>         )
> 
>       , ( "#3: !a S. S -> S a and !S. S -> S"
>         , ( ForAll (Vars [V "S"] [V "a"]) (Stack (V "S") []) (Stack (V "S") [TyVar (V "a")])
>           , ForAll (Vars [V "S"] []) (Stack (V "S") []) (Stack (V "S") [])
>           , Right $ ForAll (Vars [V "S"] [V "a"]) (Stack (V "S") []) (Stack (V "S") [TyVar (V "a")])
>           )
>         )
> 
>       , ( "#4: !a S. S -> S a and !a S. S a -> S"
>         , ( ForAll (Vars [V "S"] [V "a"]) (Stack (V "S") []) (Stack (V "S") [TyVar (V "a")])
>           , ForAll (Vars [V "S"] [V "a"]) (Stack (V "S") [TyVar (V "a")]) (Stack (V "S") [])
>           , Right $ ForAll (Vars [V "S"] []) (Stack (V "S") []) (Stack (V "S") [])
>           )
>         )
> 
>       , ( "#5: !a S. S -> S a and !b S. S b -> S"
>         , ( ForAll (Vars [V "S"] [V "a"]) (Stack (V "S") []) (Stack (V "S") [TyVar (V "a")])
>           , ForAll (Vars [V "S"] [V "a"]) (Stack (V "S") [TyVar (V "a")]) (Stack (V "S") [])
>           , Right $ ForAll (Vars [V "S"] []) (Stack (V "S") []) (Stack (V "S") [])
>           )
>         )
> 
>       , ( "#6: !a b S. S b -> S a and !b S. S b -> S"
>         , ( ForAll (Vars [V "S"] [V "a", V "b"]) (Stack (V "S") [TyVar (V "b")]) (Stack (V "S") [TyVar (V "a")])
>           , ForAll (Vars [V "S"] [V "a"]) (Stack (V "S") [TyVar (V "a")]) (Stack (V "S") [])
>           , Right $ ForAll (Vars [V "S"] [V "a"]) (Stack (V "S") [TyVar (V "a")]) (Stack (V "S") [])
>           )
>         )
>       ]
>     ]



> testEnv :: TypeEnv
> testEnv = TypeEnv (M.fromList
>   -- !S. S -> S
>   [ ( Atom "id"
>     , ForAll
>         (Vars [V "S"] [])
>         (Stack (V "S") [])
>         (Stack (V "S") [])
>     )
> 
>   -- !S. S -> S Int
>   , ( Atom "2"
>     , ForAll
>         (Vars [V "S"] [])
>         (Stack (V "S") [])
>         (Stack (V "S") [TyCon (C "Int")])
>     )
> 
>   -- !S. S -> S Char
>   , ( Atom "'a'"
>     , ForAll
>         (Vars [V "S"] [])
>         (Stack (V "S") [])
>         (Stack (V "S") [TyCon (C "Char")])
>     )
> 
>   -- !S. S -> S (!R. R -> R)
>   , ( Atom "q_id"
>     , ForAll
>         (Vars [V "S"] [])
>         (Stack (V "S") [])
>         (Stack (V "S") [
>           TyArr $ ForAll
>             (Vars [V "R"] [])
>             (Stack (V "R") [])
>             (Stack (V "R") [])])
>     )
> 
>   -- !S R. S (S -> R) -> R
>   , ( Atom "apply"
>     , ForAll
>         (Vars [V "S", V "R"] [])
>         (Stack (V "S") [
>           TyArr $ ForAll
>             mempty
>             (Stack (V "S") [])
>             (Stack (V "R") [])])
>         (Stack (V "R") [])
>     )
>   ]) (const idArrow)



> prop_Infer_examples
>   :: ( HasCallStack )
>   => (Phrase, Arrow)
>   -> Property
> prop_Infer_examples (term, arr) =
>   case runInfer testEnv $ infer term of
>     Left err -> error $ unlines
>       [ "Error:", show err
>       , "Term:", show term
>       , "Expected:", show arr
>       ]
>     Right m -> m === arr

> test_Infer_examples :: HasCallStack => TestTree
> test_Infer_examples =
>   testGroup "Infer examples"
>     [ testCases
>       prop_Infer_examples
>       [ ( "#1: id"
>         , ( Then (Only (Atom "id")) Silence
>           , ForAll
>               (Vars [V "S"] [])
>               (Stack (V "S") [])
>               (Stack (V "S") [])
>           )
>         )
> 
>       , ( "#2: id id"
>         , ( Then (Only (Atom "id")) $
>               Then (Only (Atom "id")) Silence
>           , ForAll
>               (Vars [V "S"] [])
>               (Stack (V "S") [])
>               (Stack (V "S") [])
>           )
>         )
> 
>       , ( "#3: 2 id"
>         , ( Then (Only (Atom "2")) $
>               Then (Only (Atom "id")) Silence
>           , ForAll
>               (Vars [V "S"] [])
>               (Stack (V "S") [])
>               (Stack (V "S") [TyCon (C "Int")])
>           )
>         )
> 
>       , ( "#4: id 2"
>         , ( Then (Only (Atom "id")) $
>               Then (Only (Atom "2")) Silence
>           , ForAll
>               (Vars [V "S"] [])
>               (Stack (V "S") [])
>               (Stack (V "S") [TyCon (C "Int")])
>           )
>         )
> 
>       , ( "#5: 2 2"
>         , ( Then (Only (Atom "2")) $
>               Then (Only (Atom "2")) Silence
>           , ForAll
>               (Vars [V "S"] [])
>               (Stack (V "S") [])
>               (Stack (V "S") [TyCon (C "Int"), TyCon (C "Int")])
>           )
>         )
> 
>       , ( "#6: 2 2 2"
>         , ( Then (Only (Atom "2")) $
>               Then (Only (Atom "2")) $
>               Then (Only (Atom "2")) Silence
>           , ForAll
>               (Vars [V "S"] [])
>               (Stack (V "S") [])
>               (Stack (V "S") [TyCon (C "Int"), TyCon (C "Int"), TyCon (C "Int")])
>           )
>         )
> 
>       , ( "#7: 'a' 2"
>         , ( Then (Only (Atom "'a'")) $
>               Then (Only (Atom "2")) Silence
>           , ForAll
>               (Vars [V "S"] [])
>               (Stack (V "S") [])
>               (Stack (V "S") [TyCon (C "Int"), TyCon (C "Char")])
>           )
>         )
> 
>       , ( "#8: [id] apply"
>         , ( Then (Only (Atom "q_id")) $
>               Then (Only (Atom "apply")) Silence
>           , ForAll
>               (Vars [V "S"] [])
>               (Stack (V "S") [])
>               (Stack (V "S") [])
>           )
>         )
>       ]
>     ]
