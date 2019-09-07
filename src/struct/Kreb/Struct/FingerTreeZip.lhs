---
title: Kreb.Struct.FingerTreeZip
---



Contents
--------

* [Introduction](#introduction)
* [Exposed API](#exposed-api)
* [The Zipper](#the-zipper)
* [The Tape class](#the-tape-class)
* [Testing and debugging](#testing-and-debugging)



Introduction
============

In [Ned.Data.FingerTree](src/Ned/Data/FingerTree.html) we developed a list-like data structure with efficient amortized cons and splitting. Our goal is to use this structure as the basis for a text buffer. But before we get there we need something extra: a _zipper_. Zippers are a powerful technique for turning an algebraic data type into a new type with a distinguished 'pointer' position, or in our case, a _cursor_, where we have constant-time access to data. Think about how when editing text there's a special position in the character stream where any new edits happen; this is the cursor position, and the edits there need to be as efficient as possible.

This code uses the following extensions:

> {-# LANGUAGE
>     MultiParamTypeClasses
>   , FlexibleInstances
>   , ScopedTypeVariables
> #-}

These are necessary so we can use the `Valued` type class from `FingerTree` as a constraint, and because the `Tape` class introduced here has two parameters.



Exposed API
===========

> module Kreb.Struct.FingerTreeZip (
>   -- ** FingerTreeZip
>     FingerTreeZip(..)
>   , splitFTZ
>   , integrateFTZ
>   , remeasureFTZ
>   , emptyFTZ
>   , valueAtHeadFTZ
>   , fmapFTZ
> 
>   -- ** The Tape Class
>   , Tape(..)
>   , isEmptyFTZ
>   , mkTapeFTZ
>   , mkTapeFocusFTZ
>   , unTapeFTZ
>   , isAtInitFTZ
>   , isAtLastFTZ
>   , initReadFTZ  
>   , initAlterFTZ  
>   , initMoveFTZ  
>   , initInsertFTZ  
>   , initDeleteFTZ  
>   , lastReadFTZ  
>   , lastAlterFTZ  
>   , lastMoveFTZ  
>   , lastInsertFTZ  
>   , lastDeleteFTZ  
>   , headReadFTZ  
>   , headAlterFTZ  
>   , headMoveLFTZ  
>   , headMoveRFTZ  
>   , headInsertLFTZ  
>   , headInsertRFTZ  
>   , headDeleteLFTZ  
>   , headDeleteRFTZ
> 
>   -- ** Testing and Debugging
>   , toListDebugFTZ
>   , depthFTZ
>   , showInternalFTZ
>   , validateFTZ
> ) where
> 
> import qualified Data.Foldable as Fold
> import Data.List (unwords)
> 
> import Kreb.Check ( Arb(..), Prune(..), CoArb(..) )
> import Kreb.Struct.FingerTree



The Zipper
==========

The general strategy for constructing a zipper over a type involves expressing it as a functor, calculusifying it, and taking the derivative. Our `FingerTree` type is a little odd though, because of its non-uniform recursion. Actually differentiating the type directly is surely possible, but complicated enough that I don't feel bad cheating a little bit here. Our `FingerTree` is isomorphic to the type of lists, so the derivative is isomorphic to pairs of lists. We can split this into two cases, where the list is either empty or not, like so.

> newtype FingerTreeZip m a = FingerTreeZip
>   { unFingerTreeZip
>       :: Maybe (FingerTree m a, a, FingerTree m a)
>   } deriving (Eq, Show)
> 
> instance
>   ( Arb a, Valued m a
>   ) => Arb (FingerTreeZip m a)
>   where
>     arb = FingerTreeZip <$> arb
> 
> emptyFTZ :: FingerTreeZip m a
> emptyFTZ = FingerTreeZip
>   { unFingerTreeZip = Nothing
>   }
> 
> fmapFTZ
>   :: forall m1 m2 a1 a2
>    . ( Valued m1 a1, Valued m2 a2 )
>   => (a1 -> a2) -> FingerTreeZip m1 a1 -> FingerTreeZip m2 a2
> fmapFTZ f (FingerTreeZip x) =
>   FingerTreeZip $ case x of
>     Nothing -> Nothing
>     Just (as, x, bs) ->
>       Just (fmapFT f as, f x, fmapFT f bs)

We can turn a zippered finger tree back into a normal finger tree, by "integrating". :)

> integrateFTZ
>   :: ( Valued m a )
>   => FingerTreeZip m a -> FingerTree m a
> integrateFTZ w =
>   case unFingerTreeZip w of
>     Nothing          -> mempty
>     Just (as, x, bs) -> as <> (cons x bs)

And the signature of `splitFT` is very close to returning a `FingerTreeZip`.

> splitFTZ
>   :: ( Valued m a )
>   => (m -> Bool)
>   -> FingerTree m a
>   -> FingerTreeZip m a
> splitFTZ p = FingerTreeZip . splitFT p

Like `FingerTree`, zippered finger trees inherit a `Valued` instance:

> instance
>   ( Valued m a
>   ) => Valued m (FingerTreeZip m a)
>   where
>     value (FingerTreeZip w) = case w of
>       Nothing -> mempty
>       Just (as, x, bs) -> mconcat
>         [ value as, value x, value bs ]

Now for a couple of handy value related functions. First we will often need the accumulated monoid value at the read head.

> valueAtHeadFTZ
>   :: ( Valued m a )
>   => FingerTreeZip m a -> Maybe m
> valueAtHeadFTZ w =
>   case unFingerTreeZip w of
>     Nothing -> Nothing
>     Just (as, x, _) -> Just (value as <> value x)

Next, just as we could adjust the value monoid type on the fly for finger trees, we can for zipped trees.

> remeasureFTZ
>   :: ( Valued m1 a, Valued m2 a )
>   => FingerTreeZip m1 a -> FingerTreeZip m2 a
> remeasureFTZ (FingerTreeZip w) =
>   FingerTreeZip $ case w of
>     Nothing ->
>       Nothing
>     Just (as,x,bs) ->
>       Just ( remeasureFT as, x, remeasureFT bs )



The Tape class
==============

Note that the zippered finger tree is like a list with a pointer into a specific index, where access is cheap. Moreover the properties of finger trees allow moving the pointer left and right and seeking to the beginning or end of the list is also cheap. This is something like magnetic _tape_, where we have a single read head that can see only a small part of the tape at once and by spooling the tape can move the head around from left to right.

(The tape analogy isn't perfect because efficient splitting on finger trees gives us something like logarithmic random access, but it goes far enough.)

We'll want to implement a couple of different linear structures on top of `FingerTreeZip` by choosing a specific value monoid, but in each case we'll have this linear read-head like structure. So we'll wrap an interface for this into the `Tape` type class.

A `Tape` has three special positions where access is cheap: the _initial_ element, the _last_ element, and the _head_ where the pointer points. We can move the head to the initial or last position, and can move the read head left or right. For any position we can read, alter, or delete the value there, with two variants of these for the read head depending on what direction the head should shift after the action is done.

> class Tape t a where
>   isEmpty :: t a -> Bool
>   isAtInit :: t a -> Bool
>   isAtLast :: t a -> Bool
> 
>   mkTape :: [a] -> t a
>   mkTapeFocus :: [a] -> a -> [a] -> t a
> 
>   unTape :: t a -> [a]
> 
>   initRead :: t a -> Maybe a
>   initAlter :: (a -> a) -> t a -> t a
>   initMove :: t a -> t a
>   initInsert :: a -> t a -> t a
>   initDelete :: t a -> t a
> 
>   lastRead :: t a -> Maybe a
>   lastAlter :: (a -> a) -> t a -> t a
>   lastMove :: t a -> t a
>   lastInsert :: a -> t a -> t a
>   lastDelete :: t a -> t a
> 
>   headRead :: t a -> Maybe a
>   headAlter :: (a -> a) -> t a -> t a
>   headMoveL :: t a -> t a
>   headMoveR :: t a -> t a
>   headInsertL :: a -> t a -> t a
>   headInsertR :: a -> t a -> t a
>   headDeleteL :: t a -> t a
>   headDeleteR :: t a -> t a

Due to limitations in the semantics of type classes, we can't actually give an instance of `Tape` for `FingerTreeZip`. (The problem is that weird multi parameter type class, `Valued`.) Instead, we can give ad-hoc implementations of these functions with `Valued` constraints in the signature and use those to give `Tape` instances for individual types based on `FingerTreeZip` with the value type fixed.

> isEmptyFTZ
>   :: FingerTreeZip m a -> Bool
> isEmptyFTZ w =
>   case unFingerTreeZip w of
>     Nothing -> True
>     Just _  -> False

> mkTapeFTZ
>   :: ( Valued m a )
>   => [a] -> FingerTreeZip m a
> mkTapeFTZ xs = FingerTreeZip $
>   case uncons (fromListFT xs) of
>     Nothing -> Nothing
>     Just (a,as) -> Just (mempty, a, as)
> 
> mkTapeFocusFTZ
>   :: ( Valued m a )
>   => [a] -> a -> [a] -> FingerTreeZip m a
> mkTapeFocusFTZ as x bs =
>   FingerTreeZip $
>     Just (fromListFT as, x, fromListFT bs)

> unTapeFTZ
>   :: ( Valued m a )
>   => FingerTreeZip m a -> [a]
> unTapeFTZ w =
>   case unFingerTreeZip w of
>     Nothing -> []
>     Just (as,x,bs) -> Fold.concat
>       [ Fold.toList as, [x], Fold.toList bs ]

> isAtInitFTZ
>   :: ( Valued m a )
>   => FingerTreeZip m a -> Bool
> isAtInitFTZ w =
>   case unFingerTreeZip w of
>     Nothing -> True
>     Just (as,_,_) -> isEmptyFT as
> 
> isAtLastFTZ
>   :: ( Valued m a )
>   => FingerTreeZip m a -> Bool
> isAtLastFTZ w =
>   case unFingerTreeZip w of
>     Nothing -> True
>     Just (_,_,bs) -> isEmptyFT bs

> initReadFTZ
>   :: ( Valued m a )
>   => FingerTreeZip m a -> Maybe a
> initReadFTZ w =
>   case unFingerTreeZip w of
>     Nothing -> Nothing
>     Just (as, x, bs) -> case uncons as of
>       Nothing -> Just x
>       Just (a,_) -> Just a
> 
> initAlterFTZ
>   :: ( Valued m a )
>   => (a -> a)
>   -> FingerTreeZip m a -> FingerTreeZip m a
> initAlterFTZ f w = FingerTreeZip $
>   case unFingerTreeZip w of
>     Nothing -> Nothing
>     Just (as, x, bs) -> case uncons as of
>       Nothing -> Just (mempty, f x, bs)
>       Just (a, as') -> Just (cons (f a) as', x, bs)

> initMoveFTZ  
>   :: ( Valued m a )
>   => FingerTreeZip m a -> FingerTreeZip m a
> initMoveFTZ w = FingerTreeZip $
>   case unFingerTreeZip w of
>     Nothing -> Nothing
>     Just (as, x, bs) -> case uncons as of
>       Nothing -> Just (mempty, x, bs)
>       Just (a, as') ->
>         Just (mempty, a, as' <> (cons x bs))

> initInsertFTZ
>   :: ( Valued m a )
>   => a -> FingerTreeZip m a -> FingerTreeZip m a
> initInsertFTZ a w = FingerTreeZip $
>   case unFingerTreeZip w of
>     Nothing ->
>       Just (mempty, a, mempty)
>     Just (as, x, bs) ->
>       Just (cons a as, x, bs)
> 
> initDeleteFTZ
>   :: ( Valued m a )
>   => FingerTreeZip m a -> FingerTreeZip m a
> initDeleteFTZ w = FingerTreeZip $
>   case unFingerTreeZip w of
>     Nothing -> Nothing
>     Just (as, x, bs) -> case uncons as of
>       Nothing -> case uncons bs of
>         Nothing -> Nothing
>         Just (b,bs') -> Just (mempty, b, bs')
>       Just (_,as') -> Just (as', x, bs)

> lastReadFTZ
>   :: ( Valued m a )
>   => FingerTreeZip m a -> Maybe a
> lastReadFTZ w =
>   case unFingerTreeZip w of
>     Nothing -> Nothing
>     Just (as, x, bs) -> case unsnoc bs of
>       Nothing -> Just x
>       Just (b,_) -> Just b

> lastMoveFTZ
>   :: ( Valued m a )
>   => FingerTreeZip m a -> FingerTreeZip m a
> lastMoveFTZ w = FingerTreeZip $
>   case unFingerTreeZip w of
>     Nothing -> Nothing
>     Just (as, x, bs) -> case unsnoc bs of
>       Nothing -> Just (as, x, mempty)
>       Just (b, bs') -> 
>         Just (as <> (cons x bs'), b, mempty)

> lastAlterFTZ
>   :: ( Valued m a )
>   => (a -> a)
>   -> FingerTreeZip m a -> FingerTreeZip m a
> lastAlterFTZ f w = FingerTreeZip $
>   case unFingerTreeZip w of
>     Nothing -> Nothing
>     Just (as, x, bs) -> case unsnoc bs of
>       Nothing -> Just (as, f x, mempty)
>       Just (b,bs') -> Just (as, x, snoc (f b) bs')

> lastInsertFTZ
>   :: ( Valued m a )
>   => a -> FingerTreeZip m a -> FingerTreeZip m a
> lastInsertFTZ a w = FingerTreeZip $
>   case unFingerTreeZip w of
>     Nothing ->
>       Just (mempty, a, mempty)
>     Just (as, x, bs) ->
>       Just (as, x, snoc a bs)

> lastDeleteFTZ
>   :: ( Valued m a )
>   => FingerTreeZip m a -> FingerTreeZip m a
> lastDeleteFTZ w = FingerTreeZip $
>   case unFingerTreeZip w of
>     Nothing -> Nothing
>     Just (as, x, bs) -> case unsnoc bs of
>       Nothing -> case unsnoc as of
>         Nothing -> Nothing
>         Just (a, as') -> Just (as', a, mempty)
>       Just (_,bs') -> Just (as, x, bs')

> headReadFTZ
>   :: FingerTreeZip m a -> Maybe a
> headReadFTZ w =
>   case unFingerTreeZip w of
>     Nothing -> Nothing
>     Just (_,x,_) -> Just x
> 
> headAlterFTZ
>   :: ( Valued m a )
>   => (a -> a)
>   -> FingerTreeZip m a -> FingerTreeZip m a
> headAlterFTZ f w = FingerTreeZip $
>   case unFingerTreeZip w of
>     Nothing -> Nothing
>     Just (as, x, bs) -> Just (as, f x, bs)

> headMoveLFTZ
>   :: ( Valued m a )
>   => FingerTreeZip m a -> FingerTreeZip m a
> headMoveLFTZ w = FingerTreeZip $
>   case unFingerTreeZip w of
>     Nothing -> Nothing
>     Just (as, x, bs) -> case unsnoc as of
>       Nothing -> Just (mempty, x, bs)
>       Just (a, as') ->
>         Just (as', a, cons x bs)

> headMoveRFTZ
>   :: ( Valued m a )
>   => FingerTreeZip m a -> FingerTreeZip m a
> headMoveRFTZ w = FingerTreeZip $
>   case unFingerTreeZip w of
>     Nothing -> Nothing
>     Just (as, x, bs) -> case uncons bs of
>       Nothing -> Just (as, x, mempty)
>       Just (b, bs') ->
>         Just (snoc x as, b, bs')

> headInsertLFTZ
>   :: ( Valued m a )
>   => a -> FingerTreeZip m a -> FingerTreeZip m a
> headInsertLFTZ a w = FingerTreeZip $
>   case unFingerTreeZip w of
>     Nothing -> Just (mempty, a, mempty)
>     Just (as, x, bs) -> Just (snoc a as, x, bs)

> headInsertRFTZ
>   :: ( Valued m a )
>   => a -> FingerTreeZip m a -> FingerTreeZip m a
> headInsertRFTZ a w = FingerTreeZip $
>   case unFingerTreeZip w of
>     Nothing -> Just (mempty, a, mempty)
>     Just (as, x, bs) -> Just (as, x, cons a bs)

> headDeleteRFTZ
>   :: ( Valued m a )
>   => FingerTreeZip m a -> FingerTreeZip m a
> headDeleteRFTZ w = FingerTreeZip $
>   case unFingerTreeZip w of
>     Nothing -> Nothing
>     Just (as, x, bs) -> case uncons bs of
>       Just (b,bs') -> Just (as, x, bs')
>       Nothing -> case unsnoc as of
>         Just (a,as') -> Just (as', a, mempty)
>         Nothing -> Nothing

> headDeleteLFTZ
>   :: ( Valued m a )
>   => FingerTreeZip m a -> FingerTreeZip m a
> headDeleteLFTZ w = FingerTreeZip $
>   case unFingerTreeZip w of
>     Nothing -> Nothing
>     Just (as, x, bs) -> case unsnoc as of
>       Just (a,as') -> Just (as', x, bs)
>       Nothing -> case uncons bs of
>         Just (b,bs') -> Just (mempty, b, bs')
>         Nothing -> Nothing



Testing and debugging
=====================

> depthFTZ
>   :: ( Valued m a )
>   => FingerTreeZip m a -> Int
> depthFTZ = depthFT . integrateFTZ

> toListDebugFTZ
>   :: ( Valued m a )
>   => FingerTreeZip m a -> [(a,m)]
> toListDebugFTZ (FingerTreeZip w) =
>   case w of
>     Nothing -> []
>     Just (as, x, bs) ->
>       toListDebugFT (as <> cons x bs)

> showInternalFTZ
>   :: ( Valued m a, Show m, Show a )
>   => FingerTreeZip m a -> String
> showInternalFTZ (FingerTreeZip w) =
>   case w of
>     Nothing -> "FingerTreeZip Nothing"
>     Just (as,x,bs) ->
>       let
>         p cs = if elem ' ' cs
>           then concat ["(",cs,")"]
>           else cs
>       in
>         unwords
>           [ "Just", "("
>           , p $ (showInternalFT as) ++ ","
>           , p $ (show x) ++ ","
>           , p $ (showInternalFT bs) ++ ")"
>           ]

> validateFTZ
>   :: ( Eq m, Valued m a )
>   => FingerTreeZip m a -> Bool
> validateFTZ (FingerTreeZip w) =
>   case w of
>     Nothing ->
>       True
>     Just (as, _, bs) ->
>       (validateFT as) && (validateFT bs)
