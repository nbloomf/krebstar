---
title: Generic Zippers
author: nbloomf
---

::: frontmatter

> {-# LANGUAGE RankNTypes, TypeFamilyDependencies, FlexibleContexts #-}
> 
> module Kreb.Struct.Data.Zipper where
> 
> import Data.Data
> import Data.Typeable
> 
> import Kreb.Struct.Class

:::



> {-

From [Scrap your zippers](https://michaeldadams.org/papers/scrap_your_zippers/ScrapYourZippers-2010.pdf) by Michael Adams.

> data Zipper root =
>   forall hole. (Data hole) =>
>     Zipper hole (Context hole root)

> data Context hole root where
>   NullCtx
>     :: Context a a
>   ConsCtx
>     :: forall rights parent hole root
>      . ( Data parent )
>     => Left (hole -> rights)
>     -> Right rights parent
>     -> Context parent root
>     -> Context hole root

> data Left expects where
>   UnitLeft
>     :: expects
>     -> Left expects
>   ConsLeft
>     :: forall b expects
>      . ( Data b )
>     => (Left (b -> expects)) -> b
>     -> Left expects

> data Right provides parent where
>   NullRight
>     :: Right parent parent
>   ConsRight
>     :: ( Data b )
>     => b -> Right a t
>     -> Right (b -> a) t



> combine
>   :: Left (hole -> rights)
>   -> hole
>   -> Right rights parent
>   -> parent
> combine lefts hole rights =
>   fromRight ((fromLeft lefts) hole) rights

> fromLeft
>   :: Left r -> r
> fromLeft x = case x of
>   UnitLeft a -> a
>   ConsLeft f b -> fromLeft f b

> fromRight
>   :: r -> Right r parent -> parent
> fromRight f x = case x of
>   NullRight -> f
>   ConsRight b r -> fromRight (f b) r



> toLeft
>   :: ( Data root )
>   => Zipper root -> Maybe (Zipper root)
> toLeft (Zipper h x) = case x of
>   ConsCtx (ConsLeft l h') r c ->
>     Just (Zipper h' (ConsCtx l (ConsRight h r) c))
>   _ -> Nothing

> toRight
>   :: ( Data root )
>   => Zipper root -> Maybe (Zipper root)
> toRight (Zipper h x) = case x of
>   ConsCtx l (ConsRight h' r) c ->
>     Just (Zipper h' (ConsCtx (ConsLeft l h) r c))
>   _ -> Nothing

> toUp
>   :: ( Data root )
>   => Zipper root -> Maybe (Zipper root)
> toUp (Zipper h x) = case x of
>   NullCtx -> Nothing
>   ConsCtx l r c -> Just (Zipper (combine l h r) c)

> toDown
>   :: ( Data root )
>   => Zipper root -> Maybe (Zipper root)
> toDown (Zipper h c) =
>   let
>     lefts :: ( Data a ) => a -> Left a
>     lefts = gfoldl ConsLeft UnitLeft
>   in case lefts h of
>     UnitLeft _ -> Nothing
>     ConsLeft l h' ->
>       Just (Zipper h' (ConsCtx l NullRight c))




> fromZipper
>   :: Zipper root -> root
> fromZipper (Zipper hole ctx) = case ctx of
>   NullCtx -> hole
>   ConsCtx l r c -> fromZipper (Zipper (combine l hole r) c)

> toZipper'
>   :: ( Data root )
>   => root -> Zipper root
> toZipper' root = Zipper root NullCtx


> type QueryZ r
>   = forall a. (Data a) => a -> r
> 
> queryZ
>   :: QueryZ b -> Zipper a -> b
> queryZ f (Zipper h _) = f h

> type TransformZ
>   = forall a. (Data a) => a -> a
> 
> transformZ
>   :: TransformZ -> Zipper a -> Zipper a
> transformZ f (Zipper h c) = Zipper (f h) c

> type TransformMZ m
>   = forall a. (Data a) => a -> m a
> 
> transformMZ
>   :: ( Monad m )
>   => TransformMZ m -> Zipper a -> m (Zipper a)
> transformMZ f (Zipper h c) = do
>   h' <- f h
>   return (Zipper h' c)



> getHole
>   :: ( Typeable b )
>   => Zipper a -> Maybe b
> getHole = queryZ cast



> leftQ, rightQ, upQ, downQ
>   :: ( Data a )
>   => b -> (Zipper a -> b) -> Zipper a -> b
> leftQ  = moveQ toLeft
> rightQ = moveQ toRight
> upQ    = moveQ toUp
> downQ  = moveQ toDown

> moveQ
>   :: ( Data a )
>   => (forall r. (Data r) => Zipper r -> Maybe (Zipper r))
>   -> b -> (Zipper a -> b) -> Zipper a -> b
> moveQ move b f z = case move z of
>   Nothing -> b
>   Just w  -> f w

> leftmost, rightmost
>   :: (Data a) => Zipper a -> Zipper a
> leftmost z  = leftQ z leftmost z
> rightmost z = rightQ z rightmost z


> leftT, rightT, downT, upT
>   :: ( Data a )
>   => (Zipper a -> Zipper a) -> Zipper a -> Zipper a
> leftT  f z = moveT toLeft toRight z f z
> rightT f z = moveT toRight toLeft z f z
> downT  f z = moveT toDown toUp z f z
> upT    f z = g z
>   where
>     g z = moveT toRight toLeft (h z) g z
>     h z = moveT toUp toDown z f z

> moveT
>   :: ( Data a )
>   => (forall r. (Data r) => Zipper r -> Maybe (Zipper r))
>   -> (forall r. (Data r) => Zipper r -> Maybe (Zipper r))
>   -> Zipper a -> (Zipper a -> Zipper a) -> Zipper a -> Zipper a
> moveT m1 m2 b f z =
>   moveQ m1 b (moveQ m2 b id . f) z




Experiments

> class Zipped t where
>   type ZipMove t -- reified movements
> 
>   moveZ :: ZipMove t -> Zipper (t a) -> Maybe (Zipper (t a))

> data LinearMove = ToStart | ToEnd

> class
>   ( Zipped t, ZipMove t ~ LinearMove
>   ) => LinearZipper t

> -}




> class (NonEmpty (Zipped t)) => Zipper (t :: * -> *) where
>   type Zipped t = (u :: * -> *) | u -> t
> 
>   toZipper
>     :: ( ElementOf t a )
>     => Zipped t a -> t a
> 
>   fromZipper
>     :: ( ElementOf t a )
>     => t a -> Zipped t a



> class (Container t) => LinearZipper t where
>   readPointer
>     :: ( ElementOf t a )
>     => t a -> Maybe a
> 
>   alterPointer
>     :: ( ElementOf t a )
>     => (a -> a) -> t a -> t a
> 
>   alterPointerM
>     :: ( ElementOf t a, Monad m )
>     => (a -> m a) -> t a -> m (t a)
> 
>   isAtStart
>     :: ( ElementOf t a )
>     => t a -> Bool
> 
>   isAtEnd
>     :: ( ElementOf t a )
>     => t a -> Bool
> 
>   moveTowardStart
>     :: ( ElementOf t a )
>     => t a -> t a
> 
>   moveTowardEnd
>     :: ( ElementOf t a )
>     => t a -> t a
> 
>   moveToStart
>     :: ( ElementOf t a )
>     => t a -> t a
> 
>   moveToEnd
>     :: ( ElementOf t a )
>     => t a -> t a



