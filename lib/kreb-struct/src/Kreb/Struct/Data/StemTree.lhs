---
title: Stem Trees
---

::: contents
* [Introduction](#introduction): The problem we're solving
:::



::: frontmatter

> {-# LANGUAGE OverloadedStrings #-}

> module Kreb.Struct.Data.StemTree (
>     StemTree(..)
>   , NonEmptyStemTree(..)
> 
>   , StemTreeFromList(..)
> ) where

> import           Kreb.Control
> import           Kreb.Category
> import qualified Kreb.Format as Fmt
> import           Kreb.Format (display, (<+>))
> import           Kreb.Prop

> import Kreb.Struct.Class
> import Kreb.Struct.Data.Deque

:::



Introduction
------------

Many useful data structures turn out to be just fancy kinds of trees. For instance $n$-ary trees, also known as _rose trees_, can model nested directories, abstract syntax trees, or XML fragments. A typical naive implementation of rose trees in Haskell looks something like this:

::: example
``` haskell
data RoseTree a
  = Empty
  | RoseTree a [RoseTree a]
```
:::

That is, either a rose tree is _empty_, or it has a _value_ and a list of _branches_ to zero or more maximal subtrees.

We can also use rose trees to represent the _possible futures_ of a process. The specific example we're working toward is a branching edit history for a text document -- think undo/redo, where there is only one way to undo, but possibly many ways to redo. In this application the full power of rose trees is usually overkill. Imagine a typical text editing session: there will be some "undo" operations, and some "redo" operations, but these will typically be rare events between long chains of ordinary "do" operations within which the arbitrary branching nature of rose trees is not needed because there is only one possible future.

As an optimization over this case, here we introduce a type of _stem trees_. These are isomorphic to rose trees but have better built-in support for nodes with only one branch. By way of vocabulary, we will use _successor_ to refer to the root element of the (unique) maximal subtree of a rose tree with exactly one branch.

A _nonempty_ rose tree has a value, a list of successors ordered by depth, and a list of subtrees representing the branches of the deepest successor.

> data NonEmptyStemTree a
>   = NonEmptyStemTree a (Deque a) (Deque (NonEmptyStemTree a))
>   deriving (Eq, Show)

(Note that we're building on top of `Deque` instead of ordinary cons lists.) Then an unqualified stem tree is either empty or not:

> data StemTree a
>   = Empty
>   | NonEmpty (NonEmptyStemTree a)
>   deriving (Eq, Show)

We separate the empty and nonempty cases at the type level to allow for more precise types, but of course nonempty stem trees are canonically a subtype of stem trees.

> instance Container StemTree where
>   type ElementOf StemTree = Hask
> 
> instance Container NonEmptyStemTree where
>   type ElementOf NonEmptyStemTree = Hask
> 
> instance Subset NonEmptyStemTree where
>   type SupersetOf NonEmptyStemTree = StemTree
> 
>   inject
>     :: ( Hask a )
>     => NonEmptyStemTree a -> StemTree a
>   inject = NonEmpty
> 
>   restrict
>     :: ( Hask a )
>     => StemTree a -> Maybe (NonEmptyStemTree a)
>   restrict x = case x of
>     Empty -> Nothing
>     NonEmpty w -> Just w
> 
> instance NonEmpty NonEmptyStemTree where
>   empty :: ( Hask a ) => StemTree a
>   empty = Empty
> 
>   isEmpty :: ( Hask a ) => StemTree a -> Bool
>   isEmpty x = case x of
>     Empty -> True
>     _ -> False

We can also construct singleton stem trees in the natural way.

> instance Singleton StemTree where
>   singleton
>     :: ( Hask a )
>     => a -> StemTree a
>   singleton = NonEmpty . singleton
> 
>   fromSingleton
>     :: ( Hask a )
>     => StemTree a -> Maybe a
>   fromSingleton x = case x of
>     Empty      -> Nothing
>     NonEmpty w -> fromSingleton w
> 
> instance Singleton NonEmptyStemTree where
>   singleton
>     :: ( Hask a )
>     => a -> NonEmptyStemTree a
>   singleton a = NonEmptyStemTree a empty empty
> 
>   fromSingleton
>     :: ( Hask a )
>     => NonEmptyStemTree a -> Maybe a
>   fromSingleton (NonEmptyStemTree a u v) =
>     if (isEmpty u) && (isEmpty v)
>       then Just a
>       else Nothing
> 
> instance SubsetSingleton NonEmptyStemTree
> instance NonEmptySingleton NonEmptyStemTree

Stem trees are functors:

> instance Functor StemTree where
>   fmap f x = case x of
>     Empty -> Empty
>     NonEmpty w -> NonEmpty (fmap f w)
> 
> instance Functor NonEmptyStemTree where
>   fmap f (NonEmptyStemTree a u v) =
>     NonEmptyStemTree (f a) (fmap f u) (fmap (fmap f) v)

And we have a Foldable instance.

> instance Foldable StemTree where
>   foldr
>     :: (a -> b -> b) -> b -> StemTree a -> b
>   foldr f e x = case x of
>     Empty -> e
>     NonEmpty w -> foldr f e w
> 
> instance Foldable NonEmptyStemTree where
>   foldr
>     :: (a -> b -> b) -> b -> NonEmptyStemTree a -> b
>   foldr f e (NonEmptyStemTree a u v) =
>     foldr (flip (foldr f)) (foldr f (f a e) u) v

We also give two primitives for constructing stem trees from a root and a list: one which arranges the elements in a "spine" with no branching, and one which arranges the elements all on one level.

> class StemTreeFromList t where
>   spineFromList :: a -> [a] -> t a
>   levelFromList :: a -> [a] -> t a
> 
> instance StemTreeFromList StemTree where
>   spineFromList :: a -> [a] -> StemTree a
>   spineFromList a xs = NonEmpty $ spineFromList a xs
> 
>   levelFromList :: a -> [a] -> StemTree a
>   levelFromList a xs = NonEmpty $ levelFromList a xs
> 
> instance StemTreeFromList NonEmptyStemTree where
>   spineFromList :: a -> [a] -> NonEmptyStemTree a
>   spineFromList a xs =
>     NonEmptyStemTree a (fromList xs) empty
> 
>   levelFromList :: a -> [a] -> NonEmptyStemTree a
>   levelFromList a xs =
>     NonEmptyStemTree a empty (fromList $ map singleton xs)



Testing and Debugging
---------------------

We also have class instances for interfacing with our testing framework.

> instance (Fmt.Display a) => Fmt.Display (StemTree a) where
>   display x = case x of
>     Empty      -> "Empty"
>     NonEmpty z -> "NonEmpty" <+> display z

> instance (Arb a) => Arb (StemTree a) where
>   arb = pickFrom2
>     ( pure Empty
>     , NonEmpty <$> arb
>     )
> 
> instance (Prune a) => Prune (StemTree a) where
>   prune x = case x of
>     Empty -> []
>     NonEmpty w ->
>       Empty : (fmap NonEmpty $ prune w)

> instance (Fmt.Display a) => Fmt.Display (NonEmptyStemTree a) where
>   display (NonEmptyStemTree a as xs) = "NonEmptyStemTree"
>     <+> display a <+> display as <+> display xs
> 
> instance (Arb a) => Arb (NonEmptyStemTree a) where
>   arb = do
>     k <- size
>     if k <= 1
>       then NonEmptyStemTree
>         <$> arb <*> pure empty <*> pure empty
>       else do
>         NonNegative m <- arb
>         adjustSize (`div` (m + 2)) $
>           NonEmptyStemTree <$> arb <*> arb
>             <*> (fmap fromList (vectOf m arb))
> 
> 
> instance (Prune a) => Prune (NonEmptyStemTree a) where
>   prune (NonEmptyStemTree a as bs) = concat
>     [ [ NonEmptyStemTree a as bs' | bs' <- prune bs ]
>     , [ NonEmptyStemTree a as' bs | as' <- prune as ]
>     , [ NonEmptyStemTree a' as bs | a'  <- prune a  ]
>     ]








> {-


>   , symbolicExpr


> symbolicExpr
>   :: (a -> String)
>   -> NonEmptyRoseTree a -> String
> symbolicExpr f (NonEmptyRoseTree a as bs) =
>   if isEmpty as && isEmpty bs
>     then f a
>     else concat
>       [ "(", f a
>       , concatMap (':':) $ map f $ toList as
>       , concatMap (' ':) $ map (symbolicExpr' f) $ toList bs
>       , ")"
>       ]

> symbolicExpr'
>   :: (a -> String)
>   -> RoseTree a -> String
> symbolicExpr' f x = case x of
>   Empty -> "()"
>   NonEmpty w -> symbolicExpr f w



> -}
