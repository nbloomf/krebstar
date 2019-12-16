---
title: Pointed Rose Trees
---

::: contents
* [Introduction](#introduction): The problem we're solving
* [Calculating Types](#calculating-types): Programming with Calculus!
:::



::: frontmatter

> module Kreb.Struct.PointedRoseTree (
>     PointedRoseTree()
>   , rewind

>   , sprout
>   , graft

>   , towardRoot
>   , towardLeaf
>   , branchLeft
>   , branchRight
> ) where

> import Kreb.Check
> import qualified Kreb.Struct.Sequence as S
> import qualified Kreb.Struct.RoseTree as RT

:::



Introduction
------------



Calculating Types
-----------------

foo fofofo


$$L(a) = 1 + a L(a)$$

$$\begin{eqnarray}
L'(a)
 & = & \frac{\mathrm{d}}{\mathrm{d}a}(1 + a L(a)) \\
 & = & L(a) + a L'(a) \\
\end{eqnarray}$$

$$L'(a) - a L'(a) = L(a)$$

$$L'(a) = \frac{L(a)}{1 - a}$$

$$\begin{eqnarray}
\frac{1}{1 - a}
 & = & 1 + a + a^2 + a^3 + \cdots \\
 & = & L(a) \\
\end{eqnarray}$$

$$L'(a) = L(a) L(a)$$

$$a L'(a) = a L(a) L(a)$$

$$R(a) = a L(a) L(R(a))$$

$$\begin{eqnarray}
R'(a)
 & = & \frac{\mathrm{d}}{\mathrm{d}a}(a L(a) L(R(a))) \\
 & = & L(a) L(R(a)) + a L'(a) L(R(a)) + a L(a) \frac{\mathrm{d}}{\mathrm{d}a}(L(R(a))) \\
 & = & L(a) L(R(a)) + a L'(a) L(R(a)) + a L(a) L'(R(a)) R'(a) \\
\end{eqnarray}$$

$$R'(a) - a L(a) L'(R(a)) R'(a) = L(a) L(R(a)) + a L'(a) L(R(a))$$

$$\begin{eqnarray}
R'(a)
 & = & \frac{L(a) L(R(a)) + a L'(a) L(R(a))}{1 - a L(a) L'(R(a))} \\
 & = & \frac{L(a) L(R(a)) + a L(a) L(a) L(R(a))}{1 - a L(a) L(R(a)) L(R(a))} \\
 & = & L(a) L(R(a)) \frac{1 + a L(a)}{1 - a L(a) L(R(a)) L(R(a))} \\
 & = & (1 + a L(a)) L(a) L(R(a)) L( a L(a) L(R(a)) L(R(a)) ) \\
\end{eqnarray}$$

$$a R'(a) = a (1 + a L(a)) L(a) L(R(a)) L(a L(a) L(R(a)) L(R(a)))$$

> data PointedRoseTree a = PointedRoseTree
>   { thorn
>       :: a
>   , root
>       :: Maybe (a, S.Sequence a)
>   , stem
>       :: S.Sequence a
>   , fork
>       :: S.Sequence (RT.RoseTree a)
>   , bush
>       :: S.Sequence
>           ( a
>           , S.Sequence a
>           , S.Sequence (RT.RoseTree a)
>           , S.Sequence (RT.RoseTree a) )
>   } deriving (Eq, Show)

> -- to the head of the most recent branch
> rewind
>   :: PointedRoseTree a -> PointedRoseTree a
> rewind t@(PointedRoseTree a r s f b) = case r of
>   Nothing ->
>     t
>   Just (u, us) ->
>     PointedRoseTree u Nothing
>       (S.moveToInit $ S.prepend us $ S.insertInit a s) f b

> sprout
>   :: a -> PointedRoseTree a -> PointedRoseTree a
> sprout x (PointedRoseTree t r s f b) = undefined

> graft
>   :: PointedRoseTree a -> PointedRoseTree a -> PointedRoseTree a
> graft x y = undefined

> towardRoot
>   :: PointedRoseTree a -> PointedRoseTree a
> towardRoot (PointedRoseTree t r s f b) = undefined

> towardLeaf
>   :: PointedRoseTree a -> PointedRoseTree a
> towardLeaf (PointedRoseTree t r s f b) = undefined

> branchLeft
>   :: PointedRoseTree a -> PointedRoseTree a
> branchLeft (PointedRoseTree t r s f b) = undefined

> branchRight
>   :: PointedRoseTree a -> PointedRoseTree a
> branchRight (PointedRoseTree t r s f b) = undefined

> instance
>   ( Arb a
>   ) => Arb (PointedRoseTree a)
>   where
>     arb = do
>       NonNegative sS <- arb
>       NonNegative fS <- arb
>       NonNegative bS <- arb
>       PointedRoseTree
>         <$> arb
>         <*> arb
>         <*> (fmap S.fromList $ vectOf sS $
>               adjustSize (`div` (sS + 2)) arb)
>         <*> (fmap S.fromList $ vectOf fS $
>               adjustSize (`div` (fS + 2)) arb)
>         <*> (fmap S.fromList $ vectOf bS $
>               adjustSize (`div` (bS + 2)) arb)

> instance
>   ( Prune a
>   ) => Prune (PointedRoseTree a)
>   where
>     prune (PointedRoseTree a r s f b) = concat
>       [ [ PointedRoseTree a r s f b' | b' <- prune b ]
>       , [ PointedRoseTree a r s f' b | f' <- prune f ]
>       , [ PointedRoseTree a r s' f b | s' <- prune s ]
>       , [ PointedRoseTree a r' s f b | r' <- prune r ]
>       , [ PointedRoseTree a' r s f b | a' <- prune a ]
>       ]
