> {-# LANGUAGE
>     MultiParamTypeClasses
>   , FlexibleInstances
>   , ScopedTypeVariables
> #-}

> module Lang.Data.Type where

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

> import Lang.Data.PrettyPrint
> import Lang.Data.Expr



> data Uniq
>   = Uniq Int
>   deriving (Eq, Ord, Show)





Type Grammar
============

> data Arrow
>   = ForAll Vars Stack Stack
>   deriving (Show)
> 
> data Stack
>   = Stack (V Stack) [Type]
>   deriving (Eq, Show)
> 
> data Type
>   = TyCon (C Type)
>   | TyVar (V Type)
>   | TyApp Type Type
>   | TyArr Arrow
>   deriving (Eq, Show)
> 
> data V u
>   = V String
>   | Skolem Uniq
>   deriving (Eq, Ord, Show)
> 
> data C u
>   = C String
>   deriving (Eq, Ord, Show)

> instance PrettyPrint Arrow where
>   pretty (ForAll vars s1 s2) = mconcat
>     [ "forall ", pretty vars, ". ", pretty s1, " -> ", pretty s2
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
> instance GetVars Arrow where
>   getFreeVars (ForAll vs s1 s2) =
>     ((getFreeVars s1) <> (getFreeVars s2)) `toss` vs
>   getBinders (ForAll vs s1 s2) =
>     vs <> (getBinders s1) <> (getBinders s2)
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
> instance Normalize Arrow where
>   normalize a@(ForAll vs s1 s2) =
>     let us = meet (getFreeVars s1 <> getFreeVars s2) vs
>     in ForAll us (normalize s1) (normalize s2)
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

> class Rename t where
>   -- Give bound vars unique names
>   renameBoundN :: String -> String -> t -> N t
> 
>   -- Naive substitution, renaming binders.
>   -- May cause variable capture if string
>   -- args not chosen carefully.
>   subsN :: String -> String -> Subs -> t -> N t
> 
> instance Rename Arrow where
>   renameBoundN tX sX arr = do
>     let ForAll (Vars sv tv) s1 s2 = normalize arr
>     sv' <- mapM (\x -> freshVar sX >>= \y -> return (x,y)) sv
>     tv' <- mapM (\x -> freshVar tX >>= \y -> return (x,y)) tv
>     let
>       sv'' = map (\(x,y) -> (x, Stack y [])) sv'
>       tv'' = map (\(x,y) -> (x, TyVar y)) tv'
>       s = Subs (M.fromList sv'') (M.fromList tv'')
>       vars = Vars (map snd sv') (map snd tv')
>     s1' <- subsN tX sX s s1 >>= renameBoundN tX sX
>     s2' <- subsN tX sX s s2 >>= renameBoundN tX sX
>     return $ ForAll vars s1' s2'
> 
>   subsN tX sX s arr = do
>     ForAll vars s1 s2 <- renameBoundN tX sX arr
>     let s' = undefineOn s vars
>     s1' <- subsN tX sX s' s1
>     s2' <- subsN tX sX s' s2
>     return $ ForAll vars s1' s2'
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




> instance Eq Arrow where
>   a1 == a2 =
>     let
>       b1@(ForAll q1@(Vars u1 v1) s1 t1) = renameBinders a1
>       b2@(ForAll q2@(Vars u2 v2) s2 t2) = renameBinders a2
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
>       [ (subs w1 s1) == (subs w2 s2)
>       , (subs w1 t1) == (subs w2 t2)
>       ]





Type Environment
================

> data TypeEnv = TypeEnv
>   { atomEnv :: M.Map Atom Arrow
>   , builtinEnv :: BuiltIn -> Maybe Arrow
>   }

> emptyTypeEnv
>   :: (BuiltIn -> Maybe Arrow) -> TypeEnv
> emptyTypeEnv builtins =
>   TypeEnv mempty builtins

> lookupAtom :: TypeEnv -> Atom -> Infer Arrow
> lookupAtom env atom =
>   case M.lookup atom (atomEnv env) of
>     Nothing -> throw $ AtomNotDefined atom
>     Just arr -> return arr

> lookupBuiltIn :: TypeEnv -> BuiltIn -> Infer Arrow
> lookupBuiltIn env builtin =
>   case (builtinEnv env) builtin of
>     Nothing -> throw $ UnrecognizedBuiltIn builtin
>     Just arr -> return arr





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
>   | BinderCountMismatch Arrow Arrow
>   | EscapeCheck Arrow Arrow

>   | CannotMatchType Type Type
>   | CannotMatchStack Stack Stack
>   | CannotMatchArrow Arrow Arrow

>   | SkolemCaptureStack Stack Stack

>   | CannotUnifyTypeCon (C Type) (C Type)

>   | CannotCompose Arrow Arrow
>   deriving (Eq, Show)





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
> instance Unify Arrow where
>   unifyNF x y = do
>     let
>       ForAll (Vars us1 vs1) s1 t1 = x
>       ForAll (Vars us2 vs2) s2 t2 = y
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
>         sub1 <- unifyNF (subs sk1 t1) (subs sk2 t2)
>         sub2 <- unifyNF (subs (sub1 <> sk1) s1) (subs (sub1 <> sk2) s2)
>         let
>           sub = sub2 <> sub1
>           esc = meet (Vars us' vs') (getFreeVars sub)
>           mat = meet (Vars us' vs') (dom sub)
>         if esc /= mempty
>           then throw $ EscapeCheck x y
>           else if mat /= mempty
>             then throw $ EscapeCheck x y
>             else return sub
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
> instance Match Arrow where
>   matchNF x y = do
>     let
>       ForAll (Vars us1 vs1) s1 t1 = x
>       ForAll (Vars us2 vs2) s2 t2 = y
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
>         sub1 <- matchNF (subs sk1 t1) (subs sk2 t2)
>         sub2 <- matchNF (subs sk1 s1) (subs sk2 s2)
>         case unionSubs sub1 sub2 of
>           Nothing -> throw $ CannotMatchArrow x y
>           Just sub ->
>             let
>               esc = meet (Vars us' vs') (getFreeVars $ filterTrivial sub)
>               mat = meet (Vars us' vs') (dom $ filterTrivial sub)
>             in if esc /= mempty
>               then throw $ EscapeCheck x y
>               else if mat /= mempty
>                 then throw $ EscapeCheck x y
>                 else return sub
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
> (<<<) :: Arrow -> Arrow -> Bool
> (<<<) x y =
>   let
>     [u,v] = renameAllBinders [x,y]
>     ForAll xs1 s1 t1 = u
>     ForAll xs2 s2 t2 = v
>     z = do
>       sub1 <- matchNF s1 s2
>       sub2 <- matchNF t1 t2
>       return $ unionSubs sub1 sub2
>   in case runInfer (emptyTypeEnv $ const (Just idArrow)) z of
>     Right (Just sub) ->
>       isSubsetOf (dom (filterTrivial sub)) xs1
>     _ -> False

> idArrow :: Arrow
> idArrow = ForAll (Vars [V "S"] []) (Stack (V "S") []) (Stack (V "S") [])





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
> instance Subsume Arrow where
>   subsumeNF x y = do
>     let
>       ForAll (Vars us1 vs1) s1 t1 = x
>       ForAll ws@(Vars us2 vs2) s2 t2 = y
>     do
>       us' <- fmap (map Skolem) $ mapM (const unique) us1
>       vs' <- fmap (map Skolem) $ mapM (const unique) vs1
>       let
>         sk = Subs
>           { _ty = M.fromList $ zipWith (\x v -> (x, TyVar v)) vs1 vs'
>           , _st = M.fromList $ zipWith (\x v -> (x, Stack v [])) us1 us'
>           }
>       sub1 <- subsumeNF t2 (subs sk t1)
>       sub2 <- subsumeNF (subs sub1 s2) (subs (sub1 <> sk) s1)
>       let
>         sub = undefineOn (sub2 <> sub1) ws
>         esc = meet (Vars us' vs') (getFreeVars sub)
>         mat = meet ws (dom sub)
>       if esc /= mempty
>         then throw $ EscapeCheck x y
>           else if (mat /= mempty) || not (noSkolems (dom sub))
>             then throw $ EscapeCheck x y
>             else return sub
> 
> instance Subsume Stack where
>   subsumeNF = unifyNF
> 
> instance Subsume Type where
>   subsumeNF x y = case (x,y) of
>     (TyArr u, TyArr v) -> subsumeNF u v
>     _ -> unifyNF x y



> quantify :: HasCallStack => Arrow -> Arrow
> quantify (ForAll _ s1 s2) =
>   let vars = (getFreeVars s1) <> (getFreeVars s2)
>   in ForAll vars s1 s2

> composeArrows :: HasCallStack => Arrow -> Arrow -> Infer Arrow
> composeArrows x y = do
>   let
>     [x', y'] = renameAllBinders [x, y]
>     ForAll _ h1@(Stack s1 us1) k1@(Stack t1 vs1) = x'
>     ForAll _ h2@(Stack s2 us2) k2@(Stack t2 vs2) = y'
>     (ps, z) = zipTail us2 vs1
>   sub <- subsumePairs ps
>   case z of
>     Nothing -> if t1 == s2
>       then return $ quantify $ ForAll mempty h1 k2
>       else let sub2 = Subs (M.fromList [(t1, Stack s2 [])]) mempty in
>         case unionSubs sub sub2 of
>           Nothing -> return $ quantify $
>             ForAll mempty (subs sub h1) (subs sub k2)
>           Just sub3 -> return $ quantify $
>             ForAll mempty (subs sub3 h1) (subs sub3 k2)
>     Just (Right us) -> do
>       let sub2 = Subs (M.fromList [(s2, Stack t1 us)]) mempty
>       case unionSubs sub sub2 of
>         Nothing -> return $ quantify $
>           ForAll mempty (subs sub h1) (subs sub k2)
>         Just sub3 -> return $ quantify $
>           ForAll mempty (subs sub3 h1) (subs sub3 k2)
>     Just (Left vs) -> do
>       let sub2 = Subs (M.fromList [(t1, Stack s2 vs)]) mempty
>       case unionSubs sub sub2 of
>         Nothing -> return $ quantify $
>           ForAll mempty (subs sub h1) (subs sub k2)
>         Just sub3 -> return $ quantify $
>           ForAll mempty (subs sub3 h1) (subs sub3 k2)
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

> infer :: HasCallStack => Phrase -> Infer Arrow
> infer phrase = do
>   env <- getLocalTypeEnv
>   case phrase of
>     Silence -> return $ ForAll (Vars [V "S"] [])
>       (Stack (V "S") []) (Stack (V "S") [])
>     Then word rest -> do
>       arr1 <- case word of
>         Only atom -> lookupAtom env atom
>         BuiltIn b -> lookupBuiltIn env b
>         Quote quote -> do
>           arr2 <- infer quote
>           return $ ForAll (Vars [V "S"] [])
>             (Stack (V "S") [])
>             (Stack (V "S") [TyArr arr2])
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



