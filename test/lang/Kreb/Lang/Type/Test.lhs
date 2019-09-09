> {-# LANGUAGE
>     ScopedTypeVariables
>   , FlexibleInstances
> #-}

> module Kreb.Lang.Type.Test (
>     test_Type
> ) where

> import Prelude hiding (Word)
> import GHC.Stack

> import qualified Data.Map as M
> import qualified Data.Set as S
> import qualified Data.List as L

> import Data.Proxy

> import Test.Tasty

> import Kreb.Lang.Expr
> import Kreb.Lang.Type

> import Kreb.Check



Tests
-----

> test_Eq_rename
>   :: forall t
>    . ( Eq t, Show t, Arb t, Prune t
>      , Normalize t, GetVars t, Rename t )
>   => Proxy t -> TestTree
> test_Eq_rename _ =
>   testGroup "Eq rename"
>     [ testKreb "x == normalize x" $
>         \(x :: t) ->
>           claimEqual x (normalize x)
> 
>     , testKreb "x == renameBinders x" $
>         \(x :: t) ->
>           claimEqual x (renameBinders x)
>     ]



> test_Unify
>   :: forall t
>    . ( Eq t, Show t, Arb t, Prune t
>      , Unify t, Rename t, GetVars t, Normalize t )
>   => Proxy t -> TestTree
> test_Unify _ =
>   testGroup "Unify"
>     [ testKreb "subs (unify x y) x == subs (unify x y) y" $
>         \(x :: t) y ->
>           case runInfer (emptyTypeEnv $ const Nothing) $ unify x y of
>             Left _ -> accept
>             Right s -> if (subs s x) == (subs s y)
>               then accept
>               else reject $ unlines
>                 [ show x, show y, show s, show (subs s x), show (subs s y) ]
> 
>     , testKreb "unify x x" $
>         \(x :: t) ->
>           case runInfer (emptyTypeEnv $ const Nothing) $ unify x x of
>             Left err -> reject $ unlines [ show err ]
>             Right _ -> accept
> 
>     , testKreb "unify x (renameBinders x)" $
>         \(x :: t) ->
>           case runInfer (emptyTypeEnv $ const Nothing) $ unify x (renameBinders x) of
>             Left err -> reject $ unlines [ show err ]
>             Right _ -> accept
> 
>     , testKreb "unify x (normalize x)" $
>         \(x :: t) ->
>           case runInfer (emptyTypeEnv $ const Nothing) $ unify x (normalize x) of
>             Left err -> reject $ unlines [ show err ]
>             Right _ -> accept
> 
>     , testKreb "unify x y == unify y x" $
>         \(x :: t) y ->
>           let
>             u = runInfer (emptyTypeEnv $ const Nothing) $ unify x y
>             v = runInfer (emptyTypeEnv $ const Nothing) $ unify y x
>           in case (u,v) of
>             (Left err, Right _) -> reject $ unlines [ show err ]
>             (Right _, Left err) -> reject $ unlines [ show err ]
>             _ -> accept
>     ]



> prop_Unify_examples
>   :: forall t
>    . ( Eq t, Show t, Unify t, Rename t, GetVars t )
>   => (t, t, Either Err Subs)
>   -> Check
> prop_Unify_examples (x, y, r) =
>   claimEqual r
>     (runInfer (emptyTypeEnv $ const Nothing) (unify x y))

> test_Unify_examples :: TestTree
> test_Unify_examples =
>   testGroup "Unify examples"
>     [ testKrebCases "Unify"
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
>     , testKrebCases "Stack"
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



> test_Match
>   :: forall t
>    . ( Eq t, Show t, Arb t, Prune t
>      , Match t, Rename t, GetVars t, Normalize t )
>   => Proxy t -> TestTree
> test_Match _ =
>   testGroup "Match"
>     [ testKreb "subs (match x y) x == y" $
>         \(x :: t) y ->
>           case runInfer (emptyTypeEnv $ const Nothing) $ match x y of
>             Left _ -> accept
>             Right s -> if (subs s x) == y
>               then accept
>               else reject $ unlines
>                 [ show x, show y, show s, show (subs s x) ]
> 
>     , testKreb "match x x" $
>         \(x :: t) ->
>           case runInfer (emptyTypeEnv $ const Nothing) $ match x x of
>             Left err -> error $ show err
>             Right s -> claimEqual s mempty
> 
>     , testKreb "match x (renameBinders x)" $
>         \(x :: t) ->
>           case runInfer (emptyTypeEnv $ const Nothing) $ match x (renameBinders x) of
>             Left err -> reject $ show err
>             Right _ -> accept
> 
>     , testKreb "match x (normalize x)" $
>         \(x :: t) ->
>           case runInfer (emptyTypeEnv $ const Nothing) $ match x (normalize x) of
>             Left err -> reject $ show err
>             Right _ -> accept
> 
>     , localOption (KrebCheckDiscard 8) $
>       testKreb "match x (subs s x)" $
>         \(x :: t) s ->
>           provisio
>             [ ("disjoint", mempty == meet (getFreeVars x) (getFreeVars s))
>             ] $
>             case runInfer (emptyTypeEnv $ const Nothing) $ match x (subs s x) of
>               Left err -> reject $ show err
>               Right _ -> accept
>     ]



> prop_Match_examples
>   :: forall t
>    . ( Eq t, Show t, Match t, Rename t, GetVars t )
>   => (t, t, Either Err Subs)
>   -> Check
> prop_Match_examples (x, y, r) =
>   claimEqual r (runInfer (emptyTypeEnv $ const Nothing) (match x y))

> test_Match_examples :: TestTree
> test_Match_examples =
>   testGroup "Match examples"
>     [ testKrebCases "Match"
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



> test_GenericInstance :: TestTree
> test_GenericInstance =
>   testGroup "Generic Instance"
>     [ testKreb "x <<< x" $
>         \x ->
>           claimEqual True (x <<< x)
> 
>     , testKreb "x <<< y && y <<< x --> x == y" $
>         \x y ->
>           if (x <<< y) && (y <<< x)
>             then claimEqual x y
>             else accept
> 
>     , testKreb "x <<< y && y <<< z --> x <<< y" $
>         \x y z ->
>           if (x <<< y) && (y <<< z)
>             then x <<< z
>             else True
>     ]

> prop_GenericInstance_examples
>   :: (Scheme, Scheme, Bool)
>   -> Check
> prop_GenericInstance_examples (x, y, r) =
>   claimEqual r (x <<< y)

> test_GenericInstance_examples :: TestTree
> test_GenericInstance_examples =
>   testGroup "Generic Instance examples"
>     [ testKrebCases "Generic Instance"
>       prop_GenericInstance_examples
>       [ ( "#1: !S. S -> S and !S. S -> S"
>         , ( ForAll (Vars [V "S"] []) $ Arrow (Stack (V "S") []) (Stack (V "S") [])
>           , ForAll (Vars [V "S"] []) $ Arrow (Stack (V "S") []) (Stack (V "S") [])
>           , True
>           )
>         )
> 
>       , ( "#2: !S. S -> S and T -> T"
>         , ( ForAll (Vars [V "S"] []) $ Arrow (Stack (V "S") []) (Stack (V "S") [])
>           , ForAll (Vars [] []) $ Arrow (Stack (V "T") []) (Stack (V "T") [])
>           , True
>           )
>         )
> 
>       , ( "#3: !S. S -> S and T -> U"
>         , ( ForAll (Vars [V "S"] []) $ Arrow (Stack (V "S") []) (Stack (V "S") [])
>           , ForAll (Vars [] []) $ Arrow (Stack (V "T") []) (Stack (V "U") [])
>           , False
>           )
>         )
> 
>       , ( "#4: !S. S -> S and T -> T a"
>         , ( ForAll (Vars [V "S"] []) $ Arrow (Stack (V "S") []) (Stack (V "S") [])
>           , ForAll (Vars [] []) $ Arrow (Stack (V "T") []) (Stack (V "T") [TyVar (V "a")])
>           , False
>           )
>         )
> 
>       , ( "#5: !S. S -> S a and T -> T a"
>         , ( ForAll (Vars [V "S"] []) $ Arrow (Stack (V "S") []) (Stack (V "S") [TyVar (V "a")])
>           , ForAll (Vars [] []) $ Arrow (Stack (V "T") []) (Stack (V "T") [TyVar (V "a")])
>           , True
>           )
>         )
> 
>       , ( "#6: !S x. S -> S x and T -> T y"
>         , ( ForAll (Vars [V "S"] [V "x"]) $ Arrow (Stack (V "S") []) (Stack (V "S") [TyVar (V "x")])
>           , ForAll (Vars [] []) $ Arrow (Stack (V "T") []) (Stack (V "T") [TyVar (V "y")])
>           , True
>           )
>         )
>       ]
>     ]



> test_Subsume
>   :: TestTree
> test_Subsume =
>   testGroup "Subsume"
>     [ testKreb "subs (subsume x y) y <<< subs (subsume x y) x" $
>         \x y ->
>           case runInfer (emptyTypeEnv $ const Nothing) $ subsume x y of
>             Left _ -> True
>             Right s -> if (subs s y) <<< (subs s x)
>               then True
>               else error $ unlines
>                 [ show x, show y, show s, show (subs s x), show (subs s y) ]
>     ]

> prop_Subsume_examples
>   :: forall t
>    . ( Eq t, Show t, Subsume t, Rename t, GetVars t )
>   => (t, t, Either Err Subs)
>   -> Check
> prop_Subsume_examples (x, y, r) =
>   claimEqual r (runInfer (emptyTypeEnv $ const Nothing) (subsume x y))

> test_Subsume_examples :: TestTree
> test_Subsume_examples =
>   testGroup "Subsume examples"
>     [ testKrebCases "Subsume"
>       prop_Subsume_examples
>       [ ( "#1: !S. S -> S and !S. S -> S"
>         , ( ForAll (Vars [V "S"] []) $ Arrow (Stack (V "S") []) (Stack (V "S") [])
>           , ForAll (Vars [V "S"] []) $ Arrow (Stack (V "S") []) (Stack (V "S") [])
>           , Right mempty
>           )
>         )
> 
>       , ( "#2: !S. S -> S and !. S -> S"
>         , ( ForAll (Vars [] []) $ Arrow (Stack (V "S") []) (Stack (V "S") [])
>           , ForAll (Vars [V "S"] []) $ Arrow (Stack (V "S") []) (Stack (V "S") [])
>           , Right mempty
>           )
>         )
> 
>       , ( "#3: !S. S -> T and !S. S -> U"
>         , ( ForAll (Vars [V "S"] []) $ Arrow (Stack (V "S") []) (Stack (V "T") [])
>           , ForAll (Vars [V "S"] []) $ Arrow (Stack (V "S") []) (Stack (V "U") [])
>           , Right $ Subs (M.fromList [(V "U", Stack (V "T") [])]) mempty
>           )
>         )
> 
>       , ( "#4: !S. S -> S and !S x. S x -> S x"
>         , ( ForAll (Vars [V "S"] [V "x"]) $ Arrow (Stack (V "S") [TyVar (V "x")]) (Stack (V "S") [TyVar (V "x")])
>           , ForAll (Vars [V "S"] []) $ Arrow (Stack (V "S") []) (Stack (V "S") [])
>           , Right mempty
>           )
>         )
>       ]
>     ]



> prop_ComposeArrows_examples
>   :: (Scheme, Scheme, Either Err Scheme)
>   -> Check
> prop_ComposeArrows_examples (x, y, r) =
>   claimEqual r (runInfer (emptyTypeEnv $ const Nothing) (composeArrows x y))

> test_ComposeArrows_examples :: TestTree
> test_ComposeArrows_examples =
>   testGroup "ComposeArrows examples"
>     [ testKrebCases "Compose Arrows"
>       prop_ComposeArrows_examples
>       [ ( "#1: !S. S -> S and !S. S -> S"
>         , ( ForAll (Vars [V "S"] []) $ Arrow (Stack (V "S") []) (Stack (V "S") [])
>           , ForAll (Vars [V "S"] []) $ Arrow (Stack (V "S") []) (Stack (V "S") [])
>           , Right $ ForAll (Vars [V "S"] []) $ Arrow (Stack (V "S") []) (Stack (V "S") [])
>           )
>         )
> 
>       , ( "#2: !S. S -> S and !T. T -> T"
>         , ( ForAll (Vars [V "S"] []) $ Arrow (Stack (V "S") []) (Stack (V "S") [])
>           , ForAll (Vars [V "T"] []) $ Arrow (Stack (V "T") []) (Stack (V "T") [])
>           , Right $ ForAll (Vars [V "S"] []) $ Arrow (Stack (V "S") []) (Stack (V "S") [])
>           )
>         )
> 
>       , ( "#3: !a S. S -> S a and !S. S -> S"
>         , ( ForAll (Vars [V "S"] [V "a"]) $ Arrow (Stack (V "S") []) (Stack (V "S") [TyVar (V "a")])
>           , ForAll (Vars [V "S"] []) $ Arrow (Stack (V "S") []) (Stack (V "S") [])
>           , Right $ ForAll (Vars [V "S"] [V "a"]) $ Arrow (Stack (V "S") []) (Stack (V "S") [TyVar (V "a")])
>           )
>         )
> 
>       , ( "#4: !a S. S -> S a and !a S. S a -> S"
>         , ( ForAll (Vars [V "S"] [V "a"]) $ Arrow (Stack (V "S") []) (Stack (V "S") [TyVar (V "a")])
>           , ForAll (Vars [V "S"] [V "a"]) $ Arrow (Stack (V "S") [TyVar (V "a")]) (Stack (V "S") [])
>           , Right $ ForAll (Vars [V "S"] []) $ Arrow (Stack (V "S") []) (Stack (V "S") [])
>           )
>         )
> 
>       , ( "#5: !a S. S -> S a and !b S. S b -> S"
>         , ( ForAll (Vars [V "S"] [V "a"]) $ Arrow (Stack (V "S") []) (Stack (V "S") [TyVar (V "a")])
>           , ForAll (Vars [V "S"] [V "a"]) $ Arrow (Stack (V "S") [TyVar (V "a")]) (Stack (V "S") [])
>           , Right $ ForAll (Vars [V "S"] []) $ Arrow (Stack (V "S") []) (Stack (V "S") [])
>           )
>         )
> 
>       , ( "#6: !a b S. S b -> S a and !b S. S b -> S"
>         , ( ForAll (Vars [V "S"] [V "a", V "b"]) $ Arrow (Stack (V "S") [TyVar (V "b")]) (Stack (V "S") [TyVar (V "a")])
>           , ForAll (Vars [V "S"] [V "a"]) $ Arrow (Stack (V "S") [TyVar (V "a")]) (Stack (V "S") [])
>           , Right $ ForAll (Vars [V "S"] [V "a"]) $ Arrow (Stack (V "S") [TyVar (V "a")]) (Stack (V "S") [])
>           )
>         )
>       ]
>     ]



> testEnv :: TypeEnv
> testEnv = TypeEnv (M.fromList
>   -- !S. S -> S
>   [ ( Atom "id"
>     , ForAll
>         (Vars [V "S"] []) $ Arrow
>         (Stack (V "S") [])
>         (Stack (V "S") [])
>     )
> 
>   -- !S. S -> S Int
>   , ( Atom "2"
>     , ForAll
>         (Vars [V "S"] []) $ Arrow
>         (Stack (V "S") [])
>         (Stack (V "S") [TyCon (C "Int")])
>     )
> 
>   -- !S. S -> S Char
>   , ( Atom "'a'"
>     , ForAll
>         (Vars [V "S"] []) $ Arrow
>         (Stack (V "S") [])
>         (Stack (V "S") [TyCon (C "Char")])
>     )
> 
>   -- !S. S -> S (!R. R -> R)
>   , ( Atom "q_id"
>     , ForAll
>         (Vars [V "S", V "R"] []) $ Arrow
>         (Stack (V "S") [])
>         (Stack (V "S") [
>           TyArr $ Arrow
>             (Stack (V "R") [])
>             (Stack (V "R") [])])
>     )
> 
>   -- !S R. S (S -> R) -> R
>   , ( Atom "apply"
>     , ForAll
>         (Vars [V "S", V "R"] []) $ Arrow
>         (Stack (V "S") [
>           TyArr $ Arrow
>             (Stack (V "S") [])
>             (Stack (V "R") [])])
>         (Stack (V "R") [])
>     )
>   ]) (const Nothing)



> prop_Infer_examples
>   :: ( HasCallStack )
>   => (Phrase, Scheme)
>   -> Check
> prop_Infer_examples (term, arr) =
>   case runInfer testEnv $ infer term of
>     Left err -> reject $ unlines
>       [ "Error:", show err
>       , "Term:", show term
>       , "Expected:", show arr
>       ]
>     Right m -> claimEqual m arr

> test_Infer_examples :: HasCallStack => TestTree
> test_Infer_examples =
>   testGroup "Infer examples"
>     [ testKrebCases "Infer"
>       prop_Infer_examples
>       [ ( "#1: id"
>         , ( Then (Only (Atom "id")) Silence
>           , ForAll
>               (Vars [V "S"] []) $ Arrow
>               (Stack (V "S") [])
>               (Stack (V "S") [])
>           )
>         )
> 
>       , ( "#2: id id"
>         , ( Then (Only (Atom "id")) $
>               Then (Only (Atom "id")) Silence
>           , ForAll
>               (Vars [V "S"] []) $ Arrow
>               (Stack (V "S") [])
>               (Stack (V "S") [])
>           )
>         )
> 
>       , ( "#3: 2 id"
>         , ( Then (Only (Atom "2")) $
>               Then (Only (Atom "id")) Silence
>           , ForAll
>               (Vars [V "S"] []) $ Arrow
>               (Stack (V "S") [])
>               (Stack (V "S") [TyCon (C "Int")])
>           )
>         )
> 
>       , ( "#4: id 2"
>         , ( Then (Only (Atom "id")) $
>               Then (Only (Atom "2")) Silence
>           , ForAll
>               (Vars [V "S"] []) $ Arrow
>               (Stack (V "S") [])
>               (Stack (V "S") [TyCon (C "Int")])
>           )
>         )
> 
>       , ( "#5: 2 2"
>         , ( Then (Only (Atom "2")) $
>               Then (Only (Atom "2")) Silence
>           , ForAll
>               (Vars [V "S"] []) $ Arrow
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
>               (Vars [V "S"] []) $ Arrow
>               (Stack (V "S") [])
>               (Stack (V "S") [TyCon (C "Int"), TyCon (C "Int"), TyCon (C "Int")])
>           )
>         )
> 
>       , ( "#7: 'a' 2"
>         , ( Then (Only (Atom "'a'")) $
>               Then (Only (Atom "2")) Silence
>           , ForAll
>               (Vars [V "S"] []) $ Arrow
>               (Stack (V "S") [])
>               (Stack (V "S") [TyCon (C "Int"), TyCon (C "Char")])
>           )
>         )
> 
>       , ( "#8: [id] apply"
>         , ( Then (Only (Atom "q_id")) $
>               Then (Only (Atom "apply")) Silence
>           , ForAll
>               (Vars [V "S"] []) $ Arrow
>               (Stack (V "S") [])
>               (Stack (V "S") [])
>           )
>         )
>       ]
>     ]



> test_Type :: TestTree
> test_Type =
>   testGroup "Lang"
>     [ test_Semigroup_laws (Proxy :: Proxy Vars)
>     , test_Monoid_laws (Proxy :: Proxy Vars)
>     , test_Left_Dioid_laws
>         (mempty :: Vars)
>         ((<>) :: Vars -> Vars -> Vars)
>         (meet :: Vars -> Vars -> Vars)
> 
>     , test_Eq_laws (Proxy :: Proxy Scheme)
>     , test_Eq_laws (Proxy :: Proxy Arrow)
>     , test_Eq_laws (Proxy :: Proxy Stack)
>     , test_Eq_laws (Proxy :: Proxy Type)
> 
>     , test_Semigroup_laws (Proxy :: Proxy Subs)
>     , test_Monoid_laws (Proxy :: Proxy Subs)
>     , test_Idempotent_Monoid_laws
>         (mempty :: Subs)
>         (augment :: Subs -> Subs -> Subs)
> 
>     , test_Monoid_left_action_laws
>         (subs :: Subs -> Scheme -> Scheme)
>     , test_Monoid_left_action_laws
>         (subs :: Subs -> Arrow -> Arrow)
>     , test_Monoid_left_action_laws
>         (subs :: Subs -> Stack -> Stack)
>     , test_Monoid_left_action_laws
>         (subs :: Subs -> Type -> Type)
> 
>     , test_Eq_rename (Proxy :: Proxy Scheme)
>     , test_Eq_rename (Proxy :: Proxy Arrow)
>     , test_Eq_rename (Proxy :: Proxy Stack)
>     , test_Eq_rename (Proxy :: Proxy Type)
> 
>     , test_Unify (Proxy :: Proxy Scheme)
>     , test_Unify (Proxy :: Proxy Arrow)
>     , test_Unify (Proxy :: Proxy Stack)
>     , test_Unify (Proxy :: Proxy Type)
> 
>     , test_Unify_examples
> 
>     , test_Match (Proxy :: Proxy Scheme)
>     , test_Match (Proxy :: Proxy Arrow)
>     , test_Match (Proxy :: Proxy Stack)
>     , test_Match (Proxy :: Proxy Type)
> 
>     , test_Match_examples
> 
>     , test_GenericInstance
>     , test_GenericInstance_examples
> 
>     , test_Subsume
>     , test_Subsume_examples
> 
>     , test_ComposeArrows_examples
>     , test_Infer_examples
>     ]
