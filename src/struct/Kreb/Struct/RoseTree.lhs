---
title: Rose Trees
---

::: contents
* [Introduction](#introduction): The problem we're solving
:::



::: frontmatter

> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FlexibleContexts #-}
> 
> module Kreb.Struct.RoseTree (
>     RoseTree()
>   , singleton
>   , spineFromList
>   , levelFromList
>   , symbolicExpr

>   , isSingleton

> ) where
> 
> import Data.Foldable
> 
> import Kreb.Check
> import qualified Kreb.Struct.Sequence as S

:::



Introduction
------------

In this module we'll build a data structure for modelling $n$-ary trees, sometimes called _rose trees_. These are similar to binary trees, except at each level we keep track of a _list_ of subtrees rather than just a pair. And instead of ordinary cons lists, we'll build our rose trees on top of `Sequence`.

> data RoseTree a
>   = RoseTree a (S.Sequence a) (S.Sequence (RoseTree a))
>   deriving (Eq, Show)

Note that every rose tree has at least one thing in it, so we don't have the usual empty constructor. We can construct singletons, though.

> singleton
>   :: a -> RoseTree a
> singleton a =
>   RoseTree a S.empty S.empty

We also give two primitives for constructing rose trees from a root and a list: one which arranges the elements in a "spine" with no branching, and one which arranges the elements all on one level.

> spineFromList
>   :: a -> [a] -> RoseTree a
> spineFromList a as =
>   RoseTree a (S.fromList as) S.empty
> 
> levelFromList
>   :: a -> [a] -> RoseTree a
> levelFromList a as =
>   RoseTree a S.empty (S.fromList $ map singleton as)







> symbolicExpr
>   :: (a -> String)
>   -> RoseTree a -> String
> symbolicExpr f (RoseTree a as bs) =
>   if S.isEmpty as && S.isEmpty bs
>     then f a
>     else concat
>       [ "(", f a
>       , concatMap (':':) $ map f $ toList as
>       , concatMap (' ':) $ map (symbolicExpr f) $ toList bs
>       , ")"
>       ]





> instance Functor RoseTree where
>   fmap f (RoseTree a as bs) =
>     RoseTree (f a) (fmap f as) (fmap (fmap f) bs)



Queries
-------

> isSingleton
>   :: RoseTree a -> Bool
> isSingleton (RoseTree _ as bs) =
>   S.isEmpty as && S.isEmpty bs



> instance
>   ( Arb a
>   ) => Arb (RoseTree a)
>   where
>     arb = do
>       ZZ k <- askSize
>       if k <= 1
>         then RoseTree
>           <$> arb <*> pure S.empty <*> pure S.empty
>         else do
>           NonNegative m <- arb
>           adjustSize (`div` (m + 2)) $
>             RoseTree <$> arb <*> arb
>               <*> (fmap S.fromList (vectOf m arb))
> 
> instance
>   ( Prune a
>   ) => Prune (RoseTree a)
>   where
>     prune (RoseTree a as bs) = concat
>       [ [ RoseTree a as bs' | bs' <- prune bs ]
>       , [ RoseTree a as' bs | as' <- prune as ]
>       , [ RoseTree a' as bs | a' <- prune a ]
>       ]
