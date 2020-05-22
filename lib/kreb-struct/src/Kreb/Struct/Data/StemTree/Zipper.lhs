---
title: Pointed Rose Trees
---

::: contents
* [Calculating Types](#calculating-types): Programming with Calculus!
* [Testing and Debugging](#testing-and-debugging): When things go wrong
:::



::: frontmatter

> {-# LANGUAGE OverloadedStrings #-}

> module Kreb.Struct.Data.StemTree.Zipper (
>     StemTreeZipper(..)
>   , StemTreeContext(..)
> 
>   , stepUpStem
>   , stepDownStem
>   , jumpUpStem
>   , jumpDownStem
> 
>   , spliceTree
> ) where

> import Control.Monad

> import qualified Kreb.Format as Fmt
> import           Kreb.Format (display, (<+>), braceList)
> import           Kreb.Prop
> import           Kreb.Control
> import           Kreb.Control.Constrained

> import Kreb.Struct.Class
> import Kreb.Struct.Data.Deque
> import Kreb.Struct.Data.StemTree

:::



Calculating Types
-----------------

We've defined the type of _stem trees_; these are a specialized representation of $n$-ary or _rose_ trees optimized for trees where most nodes have only one subtree. Now we define a zipper over this type by doing some calculus. First recall the definition of this type:

::: example
``` haskell
data NonEmptyStemTree a
  = NonEmptyRoseTree a (Deque a) (Deque (NonEmptyStemTree a))
  deriving (Eq, Show)

data StemTree a
  = Empty
  | NonEmpty (NonEmptyStemTree a)
  deriving (Eq, Show)
```
:::

To make the notation a little easier to work with, we'd like to express this type a little more compactly. We'll use multiplication to denote tuple types, and addition for sum types. Now let $S$ denote the type of stem trees and $L$ the type of sequences. Then we can write the definition of `StemTree` as $$S(a) = 1 + a L(a) L(S(a)),$$ and since `Sequence` is isomorphic to cons lists we also have $$L(a) = 1 + a L(a).$$ To find the zipper type over stem trees, it's enough to compute $dS/da$ calculus 1 style and then translate back to the language of types.

As a warm up, let's start by finding the derivative of $L$ (we'll need it later).

$$\begin{eqnarray}
L'(a)
 & = & \frac{d}{da}(1 + a L(a)) \\
 & = & \frac{d}{da}(1) + \frac{d}{da}(a L(a)) \\
 & = & 0 + \frac{d}{da}(a)L(a) + a \frac{d}{da}L(a) \\
L'(a)
 & = & L(a) + a L'(a) \\
\end{eqnarray}$$

This is a self-referential definition for $L'$. To get a closed form solution we need to do some arithmetic that doesn't make sense for types, unless we think of our polynomials as power series. First "subtract" $aL'(a)$ to get $$L'(a) - a L'(a) = L(a).$$ Now we can solve for $L'$ as $$L'(a) = \frac{L(a)}{1 - a}.$$ Two things here don't make sense if $L$ is a polynomial type: subtraction and division. However recall the power series equivalence

$$\begin{eqnarray}
\frac{1}{1 - a}
 & = & 1 + a + a^2 + a^3 + \cdots \\
 & = & L(a) \\
\end{eqnarray}$$

Thus we have $L'(a) = L(a) L(a)$, and so $$a L'(a) = a L(a) L(a).$$ For a general polynomial functor $F$, the zipper over $F(a)$ is $aF'(a)$, and so we have computed the type of one-hole contexts for sequences. We interpret the zipper as a distinguished list element $a$, together with the list of items to the left and to the right, both of type $L(a)$.

We'll now pull the same trick to find the zipper type over stem trees. First recall that the function in question is $$S(a) = 1 + a L(a) L(S(a)).$$ Now

$$\begin{eqnarray}
S'(a)
 & = & \frac{d}{da}(1 + a L(a) L(S(a))) \\
 & = & \frac{d}{da}(1) + \frac{d}{da}(a L(a) L(S(a))) \\
 & = & 0 + \frac{d}{da}(a)(L(a) L(S(a))) + a \frac{d}{da}(L(a) L(S(a))) \\
 & = & L(a) L(S(a)) + a L'(a) L(S(a)) + a L(a) \frac{d}{da}(L(S(a))) \\
 & = & L(a) L(S(a)) + a L'(a) L(S(a)) + a L(a) \frac{dL}{da}(S(a)) \frac{d}{da}(S(a)) \\
S'(a)
 & = & L(a) L(S(a)) + a L'(a) L(S(a)) + a L(a) L'(S(a)) S'(a)
\end{eqnarray}$$

Which looks complicated, but now we can isolate $S'$ first by gathering like terms:

$$S'(a) - a L(a) L'(S(a)) S'(a) = L(a) L(S(a)) + a L'(a) L(S(a)).$$

Isolating $S'$ and simplifying, we have.

$$\begin{eqnarray}
S'(a)
 & = & \frac{L(a) L(S(a)) + a L'(a) L(S(a))}{1 - a L(a) L'(S(a))} \\
 & = & \frac{L(a) L(S(a)) + a L(a) L(a) L(S(a))}{1 - a L(a) L(S(a)) L(S(a))} \\
 & = & L(a) L(S(a)) \frac{1 + a L(a)}{1 - a L(a) L(S(a)) L(S(a))} \\
S'(a)
 & = & L(a) L(S(a)) (1 + a L(a)) L( a L(a) L(S(a)) L(S(a)) ).
\end{eqnarray}$$

The last few steps are tricky. We used the fact that $L'(a) = L(a)L(a)$ from the first line to the second, and from the third line to the fourth we used $1/(1 - a) = L(a)$.

Anyway, the zipper over stem trees is $aS'(a)$, which in all its rearranged glory looks like this:

$$a S'(a) = a \cdot (1 + a L(a)) \cdot L(a) \cdot L(S(a)) \cdot L(a L(a) L(S(a)) L(S(a))).$$

This looks really complicated! But each part of this equation has a natural meaning in terms of the "one hole context" interpretation of $S'$. Let's look at them one at a time.

* $a$: This one is free; it is the pointed-at value in the zipper.
* $1 + a L(a)$: This is the only sum among the terms, so it represents a choice. Note that the pointed-at value in the zipper must appear in the stem, and there are two places where it can be -- the distinguished item at the root, represented by $1$, or one of the internal successor items, represented by $a L(a)$ where the $a$ is the actual root and this $L(a)$ is the "prefix" of the successor chain up to the pointed-at value.
* $L(a)$: The previous term accounts for the (possibly empty) prefix of the successor chain up to the pointed-at value; this term is the suffix.
* $L(S(a))$: This term is simply the branches of the tree.
* $L(a L(a) L(S(a)) L(S(a)))$: Finally we have a list of contexts. Note that the item type in this list is almost $S$ itself, except we have two lists of subtrees rather than one. Each of these pairs is the left and right context of the subtree given by the other four terms.

From here we can define the zipper by translating back from polynomials to types. (Remember that $L$ is `Sequence` and $S$ is `StemTree`, multiplication is tuple types and $1+$ is `Maybe`.)

> data StemTreeZipper a = StemTreeZipper
>   { stemPoint    :: a
>   , stemPrefix   :: StemTreePrefix a
>   , stemSuffix   :: Deque a
>   , stemBranches :: Deque (NonEmptyStemTree a)
>   , stemContexts :: Deque (StemTreeContext a)
>   } deriving (Eq, Show)
> 
> data StemTreePrefix a
>   = IsRoot
>   | IsInterior a (Deque a)
>   deriving (Eq, Show)
> 
> data StemTreeContext a = StemTreeContext
>   { ctxRoot         :: a
>   , ctxStem         :: Deque a
>   , ctxPrecBranches :: Deque (NonEmptyStemTree a)
>   , ctxSuccBranches :: Deque (NonEmptyStemTree a)
>   } deriving (Eq, Show)

> instance (Fmt.Display a) => Fmt.Display (StemTreeZipper a) where
>   display x = "StemTreeZipper" <+> braceList
>     [ display (stemPoint x)
>     , display (stemPrefix x)
>     , display (stemSuffix x)
>     , display (stemBranches x)
>     , display (stemContexts x)
>     ]
> 
> instance (Fmt.Display a) => Fmt.Display (StemTreePrefix a) where
>   display x = case x of
>     IsRoot          -> "IsRoot"
>     IsInterior a as -> "IsInterior"
>       <+> display a <+> display as
> 
> instance (Fmt.Display a) => Fmt.Display (StemTreeContext a) where
>   display x = "StemTreeContext" <+> braceList
>     [ display (ctxRoot x)
>     , display (ctxStem x)
>     , display (ctxPrecBranches x)
>     , display (ctxSuccBranches x)
>     ]

Stem tree is a functor in the natural way:

> instance Functor StemTreeZipper where
>   fmap f (StemTreeZipper a p s b c) =
>     StemTreeZipper (f a) (fmap f p) (fmap f s)
>       (fmap (fmap f) b) (fmap (fmap f) c)
> 
> instance Functor StemTreePrefix where
>   fmap f x = case x of
>     IsRoot -> IsRoot
>     IsInterior a as -> IsInterior (f a) (fmap f as)
> 
> instance Functor StemTreeContext where
>   fmap f (StemTreeContext r s b a) =
>     StemTreeContext (f r) (fmap f s)
>       (fmap (fmap f) b) (fmap (fmap f) a)

Stem tree is a container type, and we can define some standard functions.

> instance Container StemTreeZipper where
>   type ContainerConstraint StemTreeZipper = Unconstrained
> 
> instance Singleton StemTreeZipper where
>   singleton
>     :: ( Unconstrained a )
>     => a -> StemTreeZipper a
>   singleton a = StemTreeZipper a IsRoot empty empty empty
> 
>   isSingleton
>     :: ( Unconstrained a )
>     => StemTreeZipper a -> Bool
>   isSingleton (StemTreeZipper _ p s b c) = case p of
>     IsInterior _ _ -> False
>     IsRoot -> isEmpty s && isEmpty b && isEmpty c

Of course stem tree zippers are also zippers.

> instance Zipper StemTreeZipper where
>   type Zipped StemTreeZipper = NonEmptyStemTree
> 
>   toZipper (NonEmptyStemTree a s b) =
>     StemTreeZipper a IsRoot s b empty
> 
>   fromZipper (StemTreeZipper pt pfx sfx bch ctx) =
>     let
>       sub = case pfx of
>         IsRoot ->
>           NonEmptyStemTree pt sfx bch
>         IsInterior pt' pfx' ->
>           NonEmptyStemTree pt' (pfx' <> cons pt sfx) bch
>     in case uncons ctx of
>       Nothing -> sub
>       Just (StemTreeContext rt stm prec succ, ctx') ->
>         fromZipper $ StemTreeZipper
>           { stemPoint = rt
>           , stemPrefix = IsRoot
>           , stemSuffix = stm
>           , stemBranches = prec <> cons sub succ
>           , stemContexts = ctx'
>           }



Navigation
----------

Remember that a zipper structure represents a kind of inside-out version of some other structure that provides us with efficient access to the data under a special "read head", and an important class of operations on zippers are those that move the read head around. For zippers over lists this was straightforward; there the read head can only move "backward" and "forward". With stem trees, however, branching makes this a little more complicated.

Moving toward the root of the tree isn't so bad:

> stepUpStem
>   :: StemTreeZipper a -> StemTreeZipper a
> stepUpStem x@(StemTreeZipper pt pfx sfx bch ctx) =
>   case pfx of
>     IsRoot -> x
>     IsInterior rt pfx' -> case unsnoc pfx' of
>       Nothing ->
>         StemTreeZipper rt IsRoot (cons pt sfx) bch ctx
>       Just (u, pfx'') ->
>         StemTreeZipper u (IsInterior rt pfx'') (cons u sfx) bch ctx
> 
> stepDownStem
>   :: StemTreeZipper a -> StemTreeZipper a
> stepDownStem x@(StemTreeZipper pt pfx sfx bch ctx) =
>   case uncons sfx of
>     Nothing -> x
>     Just (u, sfx') -> case pfx of
>       IsRoot ->
>         StemTreeZipper u (IsInterior pt empty) sfx' bch ctx
>       IsInterior v pfx' ->
>         StemTreeZipper u (IsInterior v (snoc pt pfx')) sfx' bch ctx

> jumpUpStem
>   :: StemTreeZipper a -> StemTreeZipper a
> jumpUpStem x@(StemTreeZipper pt pfx sfx bch ctx) =
>   case pfx of
>     IsRoot -> x
>     IsInterior rt pfx' ->
>       StemTreeZipper rt IsRoot (pfx' <> cons pt sfx) bch ctx
> 
> jumpDownStem
>   :: StemTreeZipper a -> StemTreeZipper a
> jumpDownStem x@(StemTreeZipper pt pfx sfx bch ctx) =
>   case unsnoc sfx of
>     Nothing -> x
>     Just (u, sfx') -> case pfx of
>       IsRoot ->
>         StemTreeZipper u (IsInterior pt sfx') empty bch ctx
>       IsInterior v pfx' ->
>         StemTreeZipper u (IsInterior v (pfx' <> cons pt sfx')) empty bch ctx



Mutation
--------

> spliceTree
>   :: NonEmptyStemTree a -> StemTreeZipper a -> StemTreeZipper a
> spliceTree x y =
>   let
>     NonEmptyStemTree a as bs = x
>     StemTreeZipper pt pfx sfx bch ctx = y
>   in if (isEmpty sfx) && (isEmpty bch)
>     then
>       let
>         pfx'' = case pfx of
>           IsRoot -> IsInterior pt empty
>           IsInterior rt pfx' -> IsInterior rt (snoc pt pfx')
>       in StemTreeZipper a pfx'' as bs ctx
>     else
>       let
>         prec = case uncons sfx of
>           Nothing -> bch
>           Just (c, sfx') -> singleton (NonEmptyStemTree c sfx' bch)
>         z = case pfx of
>           IsRoot -> StemTreeContext pt empty prec empty
>           IsInterior rt pfx' -> StemTreeContext rt (snoc pt pfx') prec empty
>       in StemTreeZipper a IsRoot as bs (cons z ctx)



Testing and Debugging
---------------------

Finally, we have some class instances for integrating with our generative testing framework.

> instance (Arb a) => Arb (StemTreeZipper a) where
>   arb = StemTreeZipper <$> arb <*> arb
>     <*> arbFromList <*> arbFromList <*> arbFromList
> 
> instance (Arb a) => Arb (StemTreePrefix a) where
>   arb = pickFrom2
>     ( pure IsRoot
>     , IsInterior <$> arb <*> arb
>     )
> 
> instance (Arb a) => Arb (StemTreeContext a) where
>   arb = StemTreeContext
>     <$> arb <*> arb <*> arb <*> arb
> 
> instance (Prune a) => Prune (StemTreeZipper a) where
>   prune (StemTreeZipper a r s f b) = concat
>     [ [ StemTreeZipper a r s f b' | b' <- prune b ]
>     , [ StemTreeZipper a r s f' b | f' <- prune f ]
>     , [ StemTreeZipper a r s' f b | s' <- prune s ]
>     , [ StemTreeZipper a r' s f b | r' <- prune r ]
>     , [ StemTreeZipper a' r s f b | a' <- prune a ]
>     ]
> 
> instance (Prune a) => Prune (StemTreePrefix a) where
>   prune x = case x of
>     IsRoot -> []
>     IsInterior a as -> concat
>       [ [ IsRoot ]
>       , [ IsInterior a as' | as' <- prune as ]
>       , [ IsInterior a' as | a' <- prune a ]
>       ]
> 
> instance (Prune a) => Prune (StemTreeContext a) where
>   prune (StemTreeContext a s l r) = concat
>     [ [ StemTreeContext a s l r' | r' <- prune r ]
>     , [ StemTreeContext a s l' r | l' <- prune l ]
>     , [ StemTreeContext a s' l r | s' <- prune s ]
>     , [ StemTreeContext a' s l r | a' <- prune a ]
>     ]
