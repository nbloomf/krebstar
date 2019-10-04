> {-# LANGUAGE
>     MultiParamTypeClasses
>   , FlexibleInstances
>   , ScopedTypeVariables
> #-}

> module Kreb.Lang.Type where

> import GHC.Stack

> import Prelude hiding (Word)
> import Control.Monad.ST

> import Data.STRef
> import Data.List( union, nub, (\\), intersect, isPrefixOf )
> import Data.Maybe( fromMaybe )
> import qualified Data.Set as S
> import qualified Data.Map as M
> import qualified Data.Map.Merge.Lazy as M
> import Control.Monad (ap)

> import Kreb.Check
> import Kreb.Format

> import Kreb.Lang.PrettyPrint
> import Kreb.Lang.Expr



> data Uniq
>   = Uniq Int
>   deriving (Eq, Ord, Show)
> 
> instance DisplayNeat Uniq where
>   displayNeat (Uniq k) = show k
> 
> instance Arb Uniq where
>   arb = Uniq <$> arb
> 
> instance Prune Uniq where
>   prune (Uniq k) = map Uniq $ prune k





Type Grammar
============

> data Scheme
>   = ForAll Vars Arrow
>   deriving (Show)
> 
> instance DisplayNeat Scheme where
>   displayNeat (ForAll _ ar) = displayNeat ar
> 
> instance Arb Scheme where
>   arb = do
>     xs <- adjustSize (`div` 2) arb
>     ar <- adjustSize (`div` 2) arb
>     return $ ForAll xs ar
> 
> instance Prune Scheme where
>   prune (ForAll xs ar) =
>     [ ForAll u ar | u <- prune xs ] ++
>     [ ForAll xs u | u <- prune ar ]
> 
> data Arrow
>   = Arrow Stack Stack
>   deriving (Eq, Show)
> 
> instance DisplayNeat Arrow where
>   displayNeat (Arrow st1 st2) = concat
>     [ displayNeat st1, " -> ", displayNeat st2 ]
> 
> instance Arb Arrow where
>   arb = Arrow
>     <$> (adjustSize (`div` 2) arb)
>     <*> (adjustSize (`div` 2) arb)
> 
> instance Prune Arrow where
>   prune (Arrow x y) =
>     [ Arrow u y | u <- prune x ] ++
>     [ Arrow x u | u <- prune y ]
> 
> data Stack
>   = Stack (V Stack) [Type]
>   deriving (Eq, Show)
> 
> instance DisplayNeat Stack where
>   displayNeat (Stack x ts) = concat
>     [ "$", displayNeat x
>     , concatMap (\t -> ' ' : paren (displayNeat t)) ts ]
>     where paren str = if elem ' ' str then concat ["(", str, ")"] else str
> 
> instance Arb Stack where
>   arb = do
>     k <- randIn (0,5)
>     ts <- if k == 0
>       then pure []
>       else vectOf k $ adjustSize (`div` k) arb
>     Stack <$> arb <*> pure ts
> 
> instance Prune Stack where
>   prune (Stack x ts) =
>     map (Stack x) $ prune ts
> 
> data Type
>   = TyCon (C Type)
>   | TyVar (V Type)
>   | TyApp Type Type
>   | TyArr Arrow
>   deriving (Eq, Show)
> 
> instance DisplayNeat Type where
>   displayNeat x = case x of
>     TyCon c -> displayNeat c
>     TyVar z -> displayNeat z
>     TyApp u v -> displayNeat u ++ " " ++ displayNeat v
>     TyArr arr -> "(" ++ displayNeat arr ++ ")"
> 
> instance Arb Type where
>   arb = do
>     k <- askSize
>     p <- arb
>     if p || (k <= 0)
>       then pickFrom2
>         ( TyCon <$> arb
>         , TyVar <$> arb
>         )
>       else adjustSize (`div` 2) $ pickFrom4
>         ( TyCon <$> arb
>         , TyVar <$> arb
>         , TyApp <$> arb <*> arb
>         , TyArr <$> arb
>         )
> 
> instance Prune Type where
>   prune z = case z of
>     TyCon _ -> []
>     TyVar _ -> []
>     TyApp t1 t2 ->
>       [ t1, t2 ] ++
>       [ TyApp u t2 | u <- prune t1 ] ++
>       [ TyApp t1 u | u <- prune t2 ]
>     TyArr a -> map TyArr $ prune a
> 
> data V u
>   = V String
>   | Skolem Uniq
>   deriving (Eq, Ord, Show)
> 
> instance DisplayNeat (V u) where
>   displayNeat z = case z of
>     V str -> str
>     Skolem u -> "#SKOL_" ++ displayNeat u
> 
> instance Arb (V Stack) where
>   arb = do
>     c <- pickFrom3 (pure 'S', pure 'T', pure 'U')
>     i <- randIn (0 :: Int, 5)
>     return $ V (c : show i)
> 
> instance Arb (V Type) where
>   arb = do
>     c <- pickFrom3 (pure 'a', pure 'b', pure 'c')
>     i <- randIn (0 :: Int, 5)
>     return $ V (c : show i)
> 
> instance Prune (V u) where
>   prune _ = []
> 
> data C u
>   = C String
>   deriving (Eq, Ord, Show)
> 
> instance DisplayNeat (C u) where
>   displayNeat (C str) = str
> 
> instance Arb (C Type) where
>   arb = do
>     c <- pickFrom2 (pure 'h', pure 'k')
>     i <- randIn (0 :: Int, 5)
>     return $ C (c : show i)
> 
> instance Prune (C Type) where
>   prune _ = []



> instance PrettyPrint Scheme where
>   pretty (ForAll vars arr) = pretty arr

> instance PrettyPrint Arrow where
>   pretty (Arrow s1 s2) = mconcat
>     [ pretty s1, " -> ", pretty s2
>     ]
> 
> instance PrettyPrint Stack where
>   pretty (Stack (V x) ts) = unwords
>     [ x, mconcat $ reverse $ map f $ map pretty ts ]
>     where
>       f x = if elem ' ' x then "(" ++ x ++ ")" else x
> 
> instance PrettyPrint Type where
>   pretty z = case z of
>     TyCon (C c) -> c
>     TyVar (V x) -> x
>     TyApp x y -> mconcat [ pretty x, " ", pretty y ]
>     TyArr a -> "(" ++ pretty a ++ ")"





Variables
=========

> data Vars = Vars
>   { _stVar :: [V Stack]
>   , _tyVar :: [V Type]
>   } deriving (Eq, Show)
> 
> instance Arb Vars where
>   arb = do
>     i <- randIn (0,5)
>     j <- randIn (0,5)
>     Vars
>       <$> (nub <$> vectOf i (adjustSize (`div` (i+1)) arb))
>       <*> (nub <$> vectOf j (adjustSize (`div` (j+1)) arb))
> 
> instance Prune Vars where
>   prune (Vars xs ys) =
>     [ Vars u ys | u <- prune xs ] ++
>     [ Vars xs u | u <- prune ys ]
> 
> isSubsetOf :: Vars -> Vars -> Bool
> isSubsetOf (Vars s1 t1) (Vars s2 t2) = and
>   [ all (\x -> elem x s2) s1
>   , all (\x -> elem x t2) t1
>   ]
> 
> instance Semigroup Vars where
>   v1 <> v2 = Vars
>     { _stVar = union (_stVar v1) (_stVar v2)
>     , _tyVar = union (_tyVar v1) (_tyVar v2)
>     }
> 
> instance Monoid Vars where
>   mempty = Vars [] []
> 
> toss :: Vars -> Vars -> Vars
> toss v1 v2 = Vars
>   { _stVar = (_stVar v1) \\ (_stVar v2)
>   , _tyVar = (_tyVar v1) \\ (_tyVar v2)
>   }
> 
> meet :: Vars -> Vars -> Vars
> meet v1 v2 = Vars
>   { _stVar = intersect (_stVar v1) (_stVar v2)
>   , _tyVar = intersect (_tyVar v1) (_tyVar v2)
>   }

> instance PrettyPrint Vars where
>   pretty (Vars xs ys) = unwords $
>     (map (\(V x) -> x) xs) ++
>     (map (\(V x) -> x) ys)





Substitutions
=============

> data Subs = Subs
>   { _st :: M.Map (V Stack) Stack
>   , _ty :: M.Map (V Type) Type
>   } deriving (Eq, Show)
> 
> instance Arb Subs where
>   arb = Subs
>     <$> (do
>           ma <- randIn (0,10)
>           M.fromList <$> vectOf ma (adjustSize (`div` (ma+1)) arb))
>     <*> (do
>           mb <- randIn (0,10)
>           M.fromList <$> vectOf mb (adjustSize (`div` (mb+1)) arb))
> 
> instance Prune Subs where
>   prune (Subs xs ys) =
>     [ Subs u ys | u <- prune xs ] ++
>     [ Subs xs u | u <- prune ys ]
> 
> dom :: Subs -> Vars
> dom (Subs st ty) = Vars (M.keys st) (M.keys ty)
> 
> undefineOn :: Subs -> Vars -> Subs
> undefineOn s v = Subs
>   { _st = foldr M.delete (_st s) (_stVar v)
>   , _ty = foldr M.delete (_ty s) (_tyVar v)
>   }
> 
> augment :: Subs -> Subs -> Subs
> augment (Subs u1 v1) (Subs u2 v2) =
>   Subs (M.union u1 u2) (M.union v1 v2)
> 
> unionSubs :: Subs -> Subs -> Maybe Subs
> unionSubs (Subs s1 t1) (Subs s2 t2) =
>   let
>     f :: forall k x. M.WhenMissing Maybe k x x
>     f = M.traverseMissing $ \_ x -> Just x
>     g :: forall k x. (Eq x) => M.WhenMatched Maybe k x x x
>     g = M.zipWithAMatched $ \_ x y ->
>           if x == y then Just x else Nothing
>   in Subs
>     <$> M.mergeA f f g s1 s2
>     <*> M.mergeA f f g t1 t2

> filterTrivial :: Subs -> Subs
> filterTrivial s@(Subs st ty) =
>   let
>     triv = Vars
>       (M.keys $ M.filterWithKey (\k a -> a == Stack k []) st)
>       (M.keys $ M.filterWithKey (\k a -> a == TyVar k) ty)
>   in undefineOn s triv





GetVars
=======

> -- In left-to-right order of appearance
> class GetVars t where
>   getFreeVars :: t -> Vars
>   getBinders :: t -> Vars
> 
> instance GetVars Scheme where
>   getFreeVars (ForAll vs arr) =
>     (getFreeVars arr) `toss` vs
>   getBinders (ForAll vs arr) =
>     vs <> (getBinders arr)
> 
> instance GetVars Arrow where
>   getFreeVars (Arrow s1 s2) =
>     (getFreeVars s1) <> (getFreeVars s2)
>   getBinders (Arrow s1 s2) =
>     (getBinders s1) <> (getBinders s2)
> 
> instance GetVars Stack where
>   getFreeVars (Stack x ts) =
>     (Vars [x] []) <> (mconcat $ map getFreeVars ts)
>   getBinders (Stack _ ts) =
>     mconcat $ map getBinders ts
> 
> instance GetVars Type where
>   getFreeVars z = case z of
>     TyCon _ -> mempty
>     TyVar x -> Vars [] [x]
>     TyApp x y -> (getFreeVars x) <> (getFreeVars y)
>     TyArr a -> getFreeVars a
>   getBinders z = case z of
>     TyArr a -> getBinders a
>     TyApp x y -> (getBinders x) <> (getBinders y)
>     _ -> mempty
> 
> instance GetVars Subs where
>   getFreeVars (Subs sv tv) = mconcat
>     [ mconcat $ map getFreeVars $ M.elems sv
>     , mconcat $ map getFreeVars $ M.elems tv
>     ]
> 
>   getBinders (Subs sv tv) = mconcat
>     [ mconcat $ map getBinders $ M.elems sv
>     , mconcat $ map getBinders $ M.elems tv
>     ]

> freshPrefixT :: Vars -> String
> freshPrefixT (Vars _ tv) =
>   let bads = [x | V x <- tv] in
>   head $ filter (\x -> all (not . isPrefixOf x) bads) allVarsT
> 
> allVarsT :: [String]
> allVarsT =
>   map (:[]) ['a'..'z'] ++
>   [c:cs | c <- ['a'..'z'], cs <- allVarsT]

> freshPrefixS :: Vars -> String
> freshPrefixS (Vars sv _) =
>   let bads = [x | V x <- sv] in
>   head $ filter (\x -> all (not . isPrefixOf x) bads) allVarsS
> 
> allVarsS :: [String]
> allVarsS =
>   map (:[]) ['S','T','U','V'] ++
>   [c:cs | c <- ['S'..'Z'], cs <- allVarsS]





Normalization
=============

> -- Ensure that binders are in left-to-right order
> -- and not redundant.
> class Normalize t where
>   normalize :: t -> t
> 
> instance Normalize Scheme where
>   normalize (ForAll vs arr) =
>     let us = meet (getFreeVars arr) vs
>     in ForAll us (normalize arr)
> 
> instance Normalize Arrow where
>   normalize (Arrow s1 s2) =
>     Arrow (normalize s1) (normalize s2)
> 
> instance Normalize Stack where
>   normalize (Stack x ts) =
>     Stack x (map normalize ts)
> 
> instance Normalize Type where
>   normalize z = case z of
>     TyArr a -> TyArr $ normalize a
>     _ -> z





Fresh Int Monad
===============

> newtype N a = N
>   { unN :: Int -> (a, Int)
>   }
> 
> runN :: N a -> a
> runN x = fst $ unN x 0
> 
> instance Monad N where
>   return a = N $ \k -> (a,k)
> 
>   x >>= f = N $ \k1 ->
>     let (a, k2) = unN x k1
>     in unN (f a) k2
> 
> instance Applicative N where
>   pure = return
>   (<*>) = ap
> 
> instance Functor N where
>   fmap f x = x >>= (return . f)
> 
> freshInt :: N Int
> freshInt = N $ \k -> (k, k+1)
> 
> freshVar :: String -> N (V t)
> freshVar x = do
>   k <- freshInt
>   return $ V (x ++ show k)





Renaming
========

> class Rename t where
>   -- Give bound vars unique names
>   renameBoundN :: String -> String -> t -> N t
> 
>   -- Naive substitution, renaming binders.
>   -- May cause variable capture if string
>   -- args not chosen carefully.
>   subsN :: String -> String -> Subs -> t -> N t
> 
> instance Rename Scheme where
>   renameBoundN tX sX sch = do
>     let ForAll (Vars sv tv) arr = normalize sch
>     sv' <- mapM (\x -> freshVar sX >>= \y -> return (x,y)) sv
>     tv' <- mapM (\x -> freshVar tX >>= \y -> return (x,y)) tv
>     let
>       sv'' = map (\(x,y) -> (x, Stack y [])) sv'
>       tv'' = map (\(x,y) -> (x, TyVar y)) tv'
>       s = Subs (M.fromList sv'') (M.fromList tv'')
>       vars = Vars (map snd sv') (map snd tv')
>     arr' <- subsN tX sX s arr >>= renameBoundN tX sX
>     return $ ForAll vars arr'
> 
>   subsN tX sX s sch = do
>     ForAll vars arr <- renameBoundN tX sX sch
>     let s' = undefineOn s vars
>     arr' <- subsN tX sX s' arr
>     return $ ForAll vars arr'
> 
> instance Rename Arrow where
>   renameBoundN tX sX (Arrow s1 s2) = do
>     s1' <- renameBoundN tX sX s1
>     s2' <- renameBoundN tX sX s2
>     return $ Arrow s1' s2'
> 
>   subsN tX sX s arr = do
>     Arrow s1 s2 <- renameBoundN tX sX arr
>     s1' <- subsN tX sX s s1
>     s2' <- subsN tX sX s s2
>     return $ Arrow s1' s2'
> 
> instance Rename Stack where
>   renameBoundN tX sX (Stack x ts) = do
>     ts' <- mapM (renameBoundN tX sX) ts
>     return $ Stack x ts'
> 
>   subsN tX sX s (Stack x ts) = do
>     ts' <- mapM (subsN tX sX s) ts
>     return $ case M.lookup x (_st s) of
>       Nothing -> Stack x ts'
>       Just (Stack y vs) -> Stack y (ts' ++ vs)
> 
> instance Rename Type where
>   renameBoundN tX sX z = case z of
>     TyArr a -> TyArr <$> renameBoundN tX sX a
>     TyApp x y -> do
>       x' <- renameBoundN tX sX x
>       y' <- renameBoundN tX sX y
>       return $ TyApp x' y'
>     _ -> return z
> 
>   subsN tX sX s z = case z of
>     TyCon c -> return $ TyCon c
>     TyVar x -> return $ case M.lookup x (_ty s) of
>       Nothing -> TyVar x
>       Just w -> w
>     TyApp x y -> do
>       x' <- subsN tX sX s x
>       y' <- subsN tX sX s y
>       return $ TyApp x' y'
>     TyArr a -> TyArr <$> subsN tX sX s a
> 
> instance Rename Subs where
>   renameBoundN tX sX (Subs sv tv) = do
>     sv' <- mapM (renameBoundN tX sX) sv
>     tv' <- mapM (renameBoundN tX sX) tv
>     return $ Subs sv' tv'
> 
>   subsN tX sX s (Subs sv tv) = do
>     sv' <- mapM (subsN tX sX s) sv
>     tv' <- mapM (subsN tX sX s) tv
>     return $ Subs sv' tv'



> renameBinders
>   :: ( Rename t, GetVars t )
>   => t -> t
> renameBinders x =
>   let
>     vars = (getFreeVars x) <> (getBinders x)
>     tX = freshPrefixT vars
>     sX = freshPrefixS vars
>   in runN $ renameBoundN tX sX x

> renameAllBinders
>   :: ( Rename t, GetVars t )
>   => [t] -> [t]
> renameAllBinders ts =
>   let
>     vars = mconcat
>       [ mconcat $ map getFreeVars ts
>       , mconcat $ map getBinders ts ]
>     tX = freshPrefixT vars
>     sX = freshPrefixS vars
>   in runN $ mapM (renameBoundN tX sX) ts

> subs
>   :: ( Rename t, GetVars t )
>   => Subs -> t -> t
> subs s x =
>   let
>     vars = (dom s) <> (getFreeVars s) <> (getBinders s) <> (getFreeVars x) <> (getBinders x)
>     tX = freshPrefixT vars
>     sX = freshPrefixS vars
>   in runN $ subsN tX sX s x

> instance Semigroup Subs where
>   s1 <> s2 = augment (subs s1 s2) s1
> 
> instance Monoid Subs where
>   mempty = Subs mempty mempty




> instance Eq Scheme where
>   s1 == s2 =
>     let
>       b1@(ForAll q1@(Vars u1 v1) arr1) = renameBinders s1
>       b2@(ForAll q2@(Vars u2 v2) arr2) = renameBinders s2
>       vars = q1 <> q2 <> getFreeVars b1 <> getFreeVars b2 <> getBinders b1 <> getBinders b2
>       tX = freshPrefixT vars
>       sX = freshPrefixS vars
>       tvars' = map (\k -> V $ tX ++ show k) [0..]
>       svars' = map (\k -> V $ sX ++ show k) [0..]
>       tvars = map TyVar tvars'
>       svars = map (\x -> Stack x []) svars'
>       x1 = zip u1 svars
>       y1 = zip v1 tvars
>       w1 = Subs (M.fromList x1) (M.fromList y1)
>       x2 = zip u2 svars
>       y2 = zip v2 tvars
>       w2 = Subs (M.fromList x2) (M.fromList y2)
>     in and
>       [ (subs w1 arr1) == (subs w2 arr2)
>       ]





Type Environment
================

> data TypeEnv = TypeEnv
>   { atomEnv :: M.Map Atom Scheme
>   , builtinEnv :: BuiltIn -> Maybe Scheme
>   }

> emptyTypeEnv
>   :: (BuiltIn -> Maybe Scheme) -> TypeEnv
> emptyTypeEnv builtins =
>   TypeEnv mempty builtins

> lookupAtom :: TypeEnv -> Atom -> Infer Scheme
> lookupAtom env atom =
>   case M.lookup atom (atomEnv env) of
>     Nothing -> throw $ AtomNotDefined atom
>     Just sch -> return sch

> lookupBuiltIn :: TypeEnv -> BuiltIn -> Infer Scheme
> lookupBuiltIn env builtin =
>   case (builtinEnv env) builtin of
>     Nothing -> throw $ UnrecognizedBuiltIn builtin
>     Just sch -> return sch





Infer Monad
===========

> data Infer a = Infer
>   { unInfer
>       :: TypeEnv -> Uniq
>       -> (Either Err a, Uniq)
>   }
> 
> instance Monad Infer where
>   return a = Infer $ \_ u ->
>     (Right a, u)
> 
>   x >>= f = Infer $ \te u1 ->
>     let (a, u2) = unInfer x te u1
>     in case a of
>       Right z -> unInfer (f z) te u2
>       Left e -> (Left e, u2)
> 
> instance Applicative Infer where
>   pure = return
>   (<*>) = ap
> 
> instance Functor Infer where
>   fmap f x = x >>= (return . f)
> 
> throw :: Err -> Infer a
> throw err = Infer $ \_ u ->
>   (Left err, u)
> 
> runInfer
>   :: HasCallStack => TypeEnv -> Infer a
>   -> Either Err a
> runInfer te x = 
>   let (y, _) = unInfer x te (Uniq 0)
>   in y

> getLocalTypeEnv :: Infer TypeEnv
> getLocalTypeEnv = Infer $ \env u ->
>   (Right env, u)
 
> unique :: Infer Uniq
> unique = Infer $ \_ (Uniq k) ->
>   (Right (Uniq k), Uniq (k+1))

> data Err
>   = AtomNotDefined Atom
>   | UnrecognizedBuiltIn BuiltIn
>   | BuiltInNotDefined BuiltIn
>   | CannotUnifySkolem (V Type) Type
>   | CannotUnifyType Type Type
>   | CannotUnifyStack Stack Stack
>   | OccursCheckType (V Type) Type
>   | OccursCheckStack (V Stack) Stack 
>   | OccursCheckStackList (V Stack) [Type]
>   | BinderCountMismatch Scheme Scheme
>   | EscapeCheck Scheme Scheme

>   | CannotMatchType Type Type
>   | CannotMatchStack Stack Stack
>   | CannotMatchArrow Arrow Arrow

>   | SkolemCaptureStack Stack Stack

>   | CannotUnifyTypeCon (C Type) (C Type)

>   | CannotCompose Arrow Arrow
>   deriving (Eq, Show)

> instance DisplayNeat Err where
>   displayNeat z = case z of
>     AtomNotDefined a -> concat
>       [ "atom \'", displayNeat a, "\' is not defined." ]
>     UnrecognizedBuiltIn x -> concat
>       [ "builtin \'", displayNeat x, "\' is not recognized." ]





Unification
===========

> class Unify t where
>   -- Arguments must be in normal form.
>   unifyNF :: HasCallStack => t -> t -> Infer Subs
> 
> unify
>   :: ( Unify t, Rename t, GetVars t )
>   => t -> t -> Infer Subs
> unify x y = do
>   let
>     [x',y'] = renameAllBinders [x,y]
>   unifyNF x' y'
> 
> instance Unify Scheme where
>   unifyNF x y = do
>     let
>       ForAll (Vars us1 vs1) arr1 = x
>       ForAll (Vars us2 vs2) arr2 = y
>     if ((length us1) /= (length us2)) || ((length vs1) /= (length vs2))
>       then throw $ BinderCountMismatch x y
>       else do
>         us' <- fmap (map Skolem) $ mapM (const unique) us1
>         vs' <- fmap (map Skolem) $ mapM (const unique) vs1
>         let
>           sk1 = Subs
>             { _ty = M.fromList $ zipWith (\x v -> (x, TyVar v)) vs1 vs'
>             , _st = M.fromList $ zipWith (\x v -> (x, Stack v [])) us1 us'
>             }
>           sk2 = Subs
>             { _ty = M.fromList $ zipWith (\x v -> (x, TyVar v)) vs2 vs'
>             , _st = M.fromList $ zipWith (\x v -> (x, Stack v [])) us2 us'
>             }
>         sub <- unifyNF (subs sk1 arr1) (subs sk2 arr2)
>         let
>           esc = meet (Vars us' vs') (getFreeVars sub)
>           mat = meet (Vars us' vs') (dom sub)
>         if esc /= mempty
>           then throw $ EscapeCheck x y
>           else if mat /= mempty
>             then throw $ EscapeCheck x y
>             else return sub
> 
> instance Unify Arrow where
>   unifyNF (Arrow s1 t1) (Arrow s2 t2) = do
>     sub1 <- unifyNF t1 t2
>     sub2 <- unifyNF (subs sub1 s1) (subs sub1 s2)
>     return (sub2 <> sub1)
> 
> instance Unify Stack where
>   unifyNF x@(Stack x1 ts1) y@(Stack x2 ts2) = case (x1,x2) of
>     (Skolem k1, Skolem k2) -> if (k1 == k2) && (length ts1 == length ts2)
>       then unifyList (Stack x1 []) ts1 (Stack x2 []) ts2
>       else throw $ CannotUnifyStack x y
>     (V _, Skolem _) -> unifyList (Stack x1 []) ts1 (Stack x2 []) ts2
>     (V _, V _) -> unifyList (Stack x1 []) ts1 (Stack x2 []) ts2
>     (Skolem _, V _) -> unifyList (Stack x2 []) ts2 (Stack x1 []) ts1
>     where
>       unifyList :: Stack -> [Type] -> Stack -> [Type] -> Infer Subs
>       unifyList wa@(Stack y1 ys1) as wb@(Stack y2 ys2) bs = case (as, bs) of
>         ([], []) ->
>           if (ys1 == []) && (ys2 == [])
>             then if y1 == y2
>               then return mempty
>               else return $ Subs (M.fromList [(y1, Stack y2 [])]) mempty
>             else if (ys1 == []) && isV y1 && isV y2
>               then let Vars ws _ = getFreeVars wb in
>                 if elem y1 ws
>                   then throw $ OccursCheckStack y1 wb
>                   else return $ Subs (M.fromList [(y1, wb)]) mempty
>               else if (ys2 == []) && isV y1 && isV y2
>                 then let Vars ws _ = getFreeVars wa in
>                   if elem y2 ws
>                     then throw $ OccursCheckStack y2 wa
>                     else return $ Subs (M.fromList [(y2, wa)]) mempty
>                 else if wa == wb
>                   then return mempty
>                   else throw $ CannotUnifyStack x y
>         ([], _) -> if wa == (Stack y2 (bs ++ ys2))
>           then return mempty
>           else if ys1 /= []
>             then throw $ CannotUnifyStack x y
>             else if y1 == y2
>               then throw $ OccursCheckStack y1 (Stack y2 (bs ++ ys2))
>               else let Vars ws _ = getFreeVars (Stack y2 (bs ++ ys2)) in
>                 if elem y1 ws
>                   then throw $ OccursCheckStackList y1 (bs ++ ys2)
>                   else return $ Subs (M.fromList [(y1, Stack y2 (bs ++ ys2))]) mempty
>         (_, []) -> if wb == (Stack y1 (as ++ ys1))
>           then return mempty
>           else if ys2 /= []
>             then throw $ CannotUnifyStack x y
>             else if y1 == y2
>               then throw $ OccursCheckStack y2 (Stack y1 (as ++ ys1))
>               else let Vars ws _ = getFreeVars (Stack y1 (as ++ ys1)) in
>                 if elem y2 ws
>                   then throw $ OccursCheckStackList y2 (as ++ ys1)
>                   else return $ Subs (M.fromList [(y2, Stack y1 (as ++ ys1))]) mempty
>         (u:us, v:vs) -> do
>           sub1 <- unifyNF u v
>           sub2 <- unifyList (subs sub1 wa) (map (subs sub1) us) (subs sub1 wb) (map (subs sub1) vs)
>           return $ sub2 <> sub1
> 
> instance Unify Type where
>   unifyNF x y = case (x,y) of
>     (TyCon c1, TyCon c2) ->
>       if c1 == c2
>         then return mempty
>         else throw $ CannotUnifyTypeCon c1 c2
>     (TyVar x1, TyVar x2) -> case (x1, x2) of
>       (Skolem _, Skolem _) ->
>         if x1 == x2
>           then return mempty
>           else throw $ CannotUnifyType x y
>       (V _, V _) ->
>         if x1 == x2
>           then return mempty
>           else return $ Subs mempty (M.fromList [(x1, TyVar x2)])
>       _ -> throw $ CannotUnifyType x y
>     (TyVar x1, tau2) -> unifyVar x1 tau2
>     (tau1, TyVar x2) -> unifyVar x2 tau1
>     (TyApp x1 y1, TyApp x2 y2) -> do
>       sub1 <- unifyNF y1 y2
>       sub2 <- unifyNF (subs sub1 x1) (subs sub1 x2)
>       return $ sub2 <> sub1
>     (TyArr a1, TyArr a2) -> unifyNF a1 a2
>     _ -> throw $ CannotUnifyType x y
>     where
>       unifyVar :: V Type -> Type -> Infer Subs
>       unifyVar x e = case x of
>         Skolem _ -> throw $ CannotUnifySkolem x e
>         _ -> do
>           let Vars _ ts = getFreeVars e
>           if elem x ts
>             then throw $ OccursCheckType x e
>             else return $ Subs mempty (M.fromList [(x,e)])

> isV :: V a -> Bool
> isV x = case x of
>   V _ -> True
>   Skolem _ -> False

> noSkolems :: Vars -> Bool
> noSkolems (Vars as bs) = and
>   [ all isV as
>   , all isV bs
>   ]



Matching
========

Goal: if match x y = s then subs s x === y

> class Match t where
>   matchNF :: t -> t -> Infer Subs
> 
> match
>   :: ( Match t, Rename t, GetVars t )
>   => t -> t -> Infer Subs
> match x y = do
>   let
>     x' = renameBinders x
>     y' = renameBinders y
>   s <- matchNF x' y'
>   return $ filterTrivial s
> 
> instance Match Scheme where
>   matchNF x y = do
>     let
>       ForAll (Vars us1 vs1) arr1 = x
>       ForAll (Vars us2 vs2) arr2 = y
>     if ((length us1) /= (length us2)) || ((length vs1) /= (length vs2))
>       then throw $ BinderCountMismatch x y
>       else do
>         us' <- fmap (map Skolem) $ mapM (const unique) us1
>         vs' <- fmap (map Skolem) $ mapM (const unique) vs1
>         let
>           sk1 = Subs
>             { _ty = M.fromList $ zipWith (\x v -> (x, TyVar v)) vs1 vs'
>             , _st = M.fromList $ zipWith (\x v -> (x, Stack v [])) us1 us'
>             }
>           sk2 = Subs
>             { _ty = M.fromList $ zipWith (\x v -> (x, TyVar v)) vs2 vs'
>             , _st = M.fromList $ zipWith (\x v -> (x, Stack v [])) us2 us'
>             }
>         sub <- matchNF (subs sk1 arr1) (subs sk2 arr2)
>         let
>           esc = meet (Vars us' vs') (getFreeVars $ filterTrivial sub)
>           mat = meet (Vars us' vs') (dom $ filterTrivial sub)
>         if esc /= mempty
>           then throw $ EscapeCheck x y
>           else if mat /= mempty
>             then throw $ EscapeCheck x y
>             else return sub
> 
> instance Match Arrow where
>   matchNF x@(Arrow s1 t1) y@(Arrow s2 t2) = do
>     sub1 <- matchNF t1 t2
>     sub2 <- matchNF s1 s2
>     case unionSubs sub1 sub2 of
>       Nothing -> throw $ CannotMatchArrow x y
>       Just sub -> return sub
> 
> instance Match Stack where
>   matchNF x@(Stack x1 ts1) y@(Stack x2 ts2) = case (x1,x2) of
>     (Skolem k1, Skolem k2) -> if (k1 == k2) && (length ts1 == length ts2)
>       then matchList (Stack x1 []) ts1 (Stack x2 []) ts2
>       else throw $ CannotMatchStack x y
>     (V _, V _) -> matchList (Stack x1 []) ts1 (Stack x2 []) ts2
>     _ -> throw $ CannotMatchStack x y
>     where
>       matchList :: Stack -> [Type] -> Stack -> [Type] -> Infer Subs
>       matchList wa@(Stack y1 ys1) as wb@(Stack y2 ys2) bs = case (as, bs) of
>         ([], []) ->
>           if (ys1 == []) && (ys2 == []) && isV y1 && isV y2
>             then return $ Subs (M.fromList [(y1, Stack y2 [])]) mempty
>             else if (ys1 == []) && isV y1 && isV y2
>               then let Vars ws _ = getFreeVars wb in
>                 if elem y1 ws
>                   then throw $ OccursCheckStack y1 wb
>                   else if noSkolems (getFreeVars wb)
>                     then return $ Subs (M.fromList [(y1, wb)]) mempty
>                     else throw $ SkolemCaptureStack x y
>               else if wa == wb
>                 then return mempty
>                 else throw $ CannotUnifyStack x y
>         ([], _) -> if wa == (Stack y2 (bs ++ ys2))
>           then return mempty
>           else if ys1 /= []
>             then throw $ CannotUnifyStack x y
>             else if y1 == y2
>               then throw $ OccursCheckStack y1 (Stack y2 (bs ++ ys2))
>               else let Vars ws _ = getFreeVars (Stack y2 (bs ++ ys2)) in
>                 if elem y1 ws
>                   then throw $ OccursCheckStackList y1 (bs ++ ys2)
>                   else if noSkolems $ getFreeVars $ Stack y2 (bs ++ ys2)
>                     then return $ Subs (M.fromList [(y1, Stack y2 (bs ++ ys2))]) mempty
>                     else throw $ SkolemCaptureStack x y
>         (_, []) -> throw $ CannotMatchStack x y
>         (u:us, v:vs) -> do
>           sub1 <- matchNF u v
>           sub2 <- matchList wa us wb vs
>           case unionSubs sub1 sub2 of
>             Nothing -> throw $ CannotUnifyStack x y
>             Just sub -> return sub
> 
> instance Match Type where
>   matchNF x y = case (x,y) of
>     (TyCon c1, TyCon c2) ->
>       if c1 == c2
>         then return mempty
>         else throw $ CannotUnifyTypeCon c1 c2
>     (TyVar x1, TyVar x2) -> case (x1, x2) of
>       (Skolem _, Skolem _) ->
>         if x1 == x2
>           then return mempty
>           else throw $ CannotUnifyType x y
>       (V _, V _) -> return $ Subs mempty (M.fromList [(x1, TyVar x2)])
>       _ -> throw $ CannotUnifyType x y
>     (TyVar x1, tau2) -> matchVar x1 tau2
>     (_, TyVar x2) -> throw $ CannotMatchType x y
>     (TyApp x1 y1, TyApp x2 y2) -> do
>       sub1 <- matchNF y1 y2
>       sub2 <- matchNF x1 x2
>       case unionSubs sub1 sub2 of
>         Nothing -> throw $ CannotMatchType x y
>         Just sub -> return sub
>     (TyArr a1, TyArr a2) -> matchNF a1 a2
>     _ -> throw $ CannotUnifyType x y
>     where
>       matchVar :: V Type -> Type -> Infer Subs
>       matchVar x e = case x of
>         Skolem _ -> throw $ CannotUnifySkolem x e
>         _ -> do
>           let Vars _ ts = getFreeVars e
>           if elem x ts
>             then throw $ OccursCheckType x e
>             else return $ Subs mempty (M.fromList [(x,e)])


> -- 'is generic instance of'
> (<<<) :: Scheme -> Scheme -> Bool
> (<<<) x y =
>   let
>     [u,v] = renameAllBinders [x,y]
>     ForAll xs1 arr1 = u
>     ForAll xs2 arr2 = v
>     z = matchNF arr1 arr2
>   in case runInfer (emptyTypeEnv $ const (Just idArrow)) z of
>     Right sub ->
>       isSubsetOf (dom (filterTrivial sub)) xs1
>     _ -> False

> idArrow :: Scheme
> idArrow = ForAll (Vars [V "S"] []) $ Arrow (Stack (V "S") []) (Stack (V "S") [])





Subsumption
===========

Goal: subs s u <<< subs s v

> class Subsume t where
>   subsumeNF :: HasCallStack => t -> t -> Infer Subs
> 
> subsume
>   :: ( HasCallStack, Subsume t, Rename t, GetVars t )
>   => t -> t -> Infer Subs
> subsume x y = do
>   let
>     [x',y'] = renameAllBinders [x,y]
>   subsumeNF x' y'
> 
> instance Subsume Scheme where
>   subsumeNF x y = do
>     let
>       ForAll (Vars us1 vs1) arr1 = x
>       ForAll ws@(Vars us2 vs2) arr2 = y
>     do
>       us' <- fmap (map Skolem) $ mapM (const unique) us1
>       vs' <- fmap (map Skolem) $ mapM (const unique) vs1
>       let
>         sk = Subs
>           { _ty = M.fromList $ zipWith (\x v -> (x, TyVar v)) vs1 vs'
>           , _st = M.fromList $ zipWith (\x v -> (x, Stack v [])) us1 us'
>           }
>       sub1 <- subsumeNF arr2 (subs sk arr1)
>       let
>         sub = undefineOn sub1 ws
>         esc = meet (Vars us' vs') (getFreeVars sub)
>         mat = meet ws (dom sub)
>       if esc /= mempty
>         then throw $ EscapeCheck x y
>           else if (mat /= mempty) || not (noSkolems (dom sub))
>             then throw $ EscapeCheck x y
>             else return sub
> 
> instance Subsume Arrow where
>   subsumeNF = unifyNF
> 
> instance Subsume Stack where
>   subsumeNF = unifyNF
> 
> instance Subsume Type where
>   subsumeNF x y = case (x,y) of
>     (TyArr u, TyArr v) -> subsumeNF u v
>     _ -> unifyNF x y



> quantify :: HasCallStack => Arrow -> Scheme
> quantify (Arrow s1 s2) =
>   let vars = (getFreeVars s1) <> (getFreeVars s2)
>   in ForAll vars (Arrow s1 s2)

> composeArrows :: HasCallStack => Scheme -> Scheme -> Infer Scheme
> composeArrows x y = do
>   let
>     [x', y'] = renameAllBinders [x, y]
>     ForAll _ (Arrow h1@(Stack s1 us1) k1@(Stack t1 vs1)) = x'
>     ForAll _ (Arrow h2@(Stack s2 us2) k2@(Stack t2 vs2)) = y'
>     (ps, z) = zipTail us2 vs1
>   sub <- subsumePairs ps
>   case z of
>     Nothing -> if t1 == s2
>       then return $ quantify $
>         Arrow (subs sub h1) (subs sub k2)
>       else let sub2 = subs sub $ Subs (M.fromList [(t1, Stack s2 [])]) mempty in
>         case unionSubs sub sub2 of
>           Nothing -> return $ quantify $
>             Arrow (subs sub h1) (subs sub k2)
>           Just sub3 -> return $ quantify $
>             Arrow (subs sub3 h1) (subs sub3 k2)
>     Just (Right us) -> do
>       let sub2 = subs sub $ Subs (M.fromList [(s2, Stack t1 us)]) mempty
>       case unionSubs sub sub2 of
>         Nothing -> return $ quantify $
>           Arrow (subs sub h1) (subs sub k2)
>         Just sub3 -> return $ quantify $
>           Arrow (subs sub3 h1) (subs sub3 k2)
>     Just (Left vs) -> do
>       let sub2 = subs sub $  Subs (M.fromList [(t1, Stack s2 vs)]) mempty
>       case unionSubs sub sub2 of
>         Nothing -> return $ quantify $
>           Arrow (subs sub h1) (subs sub k2)
>         Just sub3 -> return $ quantify $
>           Arrow (subs sub3 h1) (subs sub3 k2)
> 
> zipTail
>   :: HasCallStack => [a] -> [b]
>   -> ([(a,b)], Maybe (Either [a] [b]))
> zipTail xs ys = f xs ys []
>   where
>     f us vs ws = case (us,vs) of
>       ([],[]) -> (reverse ws, Nothing)
>       ([],_ ) -> (reverse ws, Just $ Right vs)
>       (_, []) -> (reverse ws, Just $ Left us)
>       (a:as,b:bs) -> f as bs ((a,b):ws)
> 
> subsumePairs
>   :: HasCallStack => [(Type, Type)] -> Infer Subs
> subsumePairs z = case z of
>   [] -> return mempty
>   (u,v):rest -> do
>     sub1 <- subsume u v
>     sub2 <- subsumePairs $ map (\(a,b) -> (subs sub1 a, subs sub1 b)) rest
>     return $ sub2 <> sub1

> infer :: HasCallStack => Phrase -> Infer Scheme
> infer phrase = do
>   env <- getLocalTypeEnv
>   case phrase of
>     Silence -> return $ ForAll (Vars [V "S"] []) $
>       Arrow (Stack (V "S") []) (Stack (V "S") [])
>     Then word rest -> do
>       arr1 <- case word of
>         Only atom -> lookupAtom env atom
>         BuiltIn b -> lookupBuiltIn env b
>         Quote quote -> do
>           ForAll vars arr2 <- infer quote
>           let s = freshPrefixS vars
>           return $ ForAll ((Vars [V s] []) <> vars) $ Arrow
>             (Stack (V s) [])
>             (Stack (V s) [TyArr arr2])
>       arr2 <- infer rest
>       composeArrows arr1 arr2




> {-

> {-
>   inferKind t = case t of
>     TyCon c -> lookupKindC c
>     TyVar x -> do
>       k <- lookupKindV x
>       case k of
>         Nothing -> throw 
>     TyApp u v -> do
>       m <- kind u
>       case m of
>         AstK -> throw "expected arrow kind"
>         ArrK a b -> do
>           h <- kind v
>           if h == a
>             then return b
>             else throw "ill-formed kind application" -}






\



> -}



