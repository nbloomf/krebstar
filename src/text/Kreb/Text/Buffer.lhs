---
title: Buffers
---

::: contents
* [Introduction](#introduction): The problem we're solving
* [Point Manipulation](#point-manipulation): Moving the read heads around
* [Basic Mutation](#basic-mutation): Cut, copy, alter, insert
* [Queries](#queries): Learning things about buffers
* [Splitting](#splitting): Moving the read heads around (again)
* [Rendering](#rendering): Displaying buffers to a virtual screen
* [Testing and Debugging](#testing-and-debugging): For when things go wrong
:::



::: frontmatter

> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE InstanceSigs #-}
> 
> module Kreb.Text.Buffer (
>     Buffer(..)
>   , BufferOp(..)
>   , empty
>   , singleton
>   , fromList
>   , fromFingerTree
>   , toList
>   , isEmpty
>   , isSingleton
>   , resizeBuffer
>   , adjustBuffer
>   , makeVacantBuffer
>   , makePointOnlyBuffer
>   , makeCoincideBuffer
>   , makePointMarkBuffer
>   , makeMarkPointBuffer
>   , prepend
>   , append
> 
>   , isPointAtStart
>   , isPointAtEnd
>   , hasMark
>   , isMarkAtStart
>   , isMarkAtEnd
>   , movePointToStart
>   , movePointToEnd
>   , moveMarkToStart
>   , moveMarkToEnd
>   , movePointLeft
>   , movePointRight
>   , moveMarkLeft
>   , moveMarkRight
>   , clearMark
>   , leaveMark
> 
>   , readPoint
>   , insertPointLeft
>   , deletePointLeft
>   , insertAtStart
>   , deleteAtStart
>   , insertAtEnd
>   , deleteAtEnd




> {-
>   , copyRegion
>   , cutRegion
>   , insertRegion
>   , alterRegion
>   , mapBuffer
>   , mapRegion
> -}


> 
>   , getBufferWidth
>   , getBufferTabStop
>   , getBufferCharCount
>   , getBufferByteCount
>   , getBufferLineCol
>   , getBufferScreenCoords
>   , getBufferScreenOffset
>   , getPointCharCount
>   , getPointByteCount
>   , getPointLineCol
>   , getPointScreenCoords
>   , getPointScreenOffset
>   , getPointRuneId
>   , getMarkCharCount
>   , getMarkByteCount
>   , getMarkLineCol
>   , getMarkScreenCoords
> 
>   , movePoint
>   , movePointToLineCol
>   , movePointToScreenCoords
>   , movePointToScreenLine
>   , movePointToRuneId
>   , atOrAfterLineCol
>   , atOrAfterScreenCoords
>   , atOrAfterScreenLine
>   , seekScreenCoords
>   , applyBufferOp
> 
>   , hasFullScreenLine
>   , takeFirstScreenLine
>   , takeScreenLines
>   , getScreenLines
>   , attachLineNumbers
>   , attachColumnIndices
>   , renderScreenLinesWithRegion
> 
>   , validate
>   , toAnnotatedList
> ) where
> 
> import Data.Proxy
> import qualified Data.Foldable as Fold
> import Data.List (groupBy, sortOn)
> import qualified Data.Map.Strict as M
> import Control.Monad (join)
> import Debug.Trace
> 
> import           Kreb.Check
> import           Kreb.Control
> import           Kreb.Reflect
> import           Kreb.Struct.Valued
> import qualified Kreb.Struct.FingerTree as FT
> import qualified Kreb.Struct.TwoPointedList as TPL
> import qualified Kreb.Struct.RedBlackTree as RBT
> 
> import Kreb.Text.ScreenOffset
> import Kreb.Text.Rune
> import Kreb.Text.Pigment
> import Kreb.Text.MeasureText
> import Kreb.Text.Glyph
> import Kreb.Text.Cell

:::



Introduction
------------

Our primary data structure for representing and manipulating text is the _buffer_. At the semantic level we can treat buffers like lists of characters, although under the hood they use a more complex representation to make some important operations more efficient. The buffer also has one and possibly two distinguished positions, called the _point_ and the _mark_, which are used to specify where edits take effect.

The actual definition of buffers builds on two other abstractions: two-pointed lists and our text measurement type. However these details aren't exposed to client code.

> data Buffer w t d a = Buffer
>   { bufContents
>       :: TPL.TwoPointedList (MeasureText w t d) (Cell (Rune d a))
>   , bufRemnants
>       :: RBT.RedBlackTree (Rune d a)
>   }

> instance
>   ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a, Eq a, IsChar a
>   ) => Eq (Buffer w t d a)
>   where
>     (Buffer w1 _) == (Buffer w2 _) =
>       (map (toChar . fmap getRuneValue) $ Fold.toList w1)
>        == (map (toChar . fmap getRuneValue) $ Fold.toList w2)

> data BufferOp d a
>   = BufferOpIns (Rune d a)
>   | BufferOpDel (Rune d a)
>   | BufferNoOp
>   deriving (Eq, Show)

Note that `MeasureText` takes two type parameters, `w` and `t`. These are type level representations of the width of the screen in cells and the tab stop width, respectively. Our underlying finger tree implementation makes it necessary that these be type parameters; essentially they are required by the monoid instance on text measurements. Later on we'll wrap buffers inside an existential type to partially hide this detail while maintaining type safety.

We also impose one invariant on buffers that can't be represented in the type: the final item in the list of characters must be a special end-of-file sigil. This simplifies the usual insert and delete operations on text by making it possible to assert that the "cursor" is at the left edge of a cell. We will have to be careful that our operations on buffers maintain this invariant (only in this module; client code shouldn't need to worry about that).

Because buffers are "just" two pointed lists, they inherit the API of two pointed lists, giving us a decent amount of code nearly for free (modulo maintaining the eof invariant). We can measure the entire buffer:

> instance
>   ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a
>   ) => Valued (MeasureText w t d) (Buffer w t d a)
>   where
>     value = value . TPL.integrate . bufContents

We also get the usual `empty` and `singleton` constructors.

> empty
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a
> empty = Buffer
>   (TPL.singleton (eof :: Cell (Rune d a))) RBT.empty
> 
> singleton
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a, IsChar a )
>   => EventId -> a -> Buffer w t d a
> singleton eId a = Buffer
>   (TPL.fromList (map Cell (makeRunes eId [a]) ++ [ eof ])) RBT.empty

More generally, we can convert lists to buffers (and back again).

> fromList
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a, IsChar a )
>   => EventId -> [a] -> Buffer w t d a
> fromList eId xs = Buffer (TPL.fromList $ concat
>   [ map Cell $ makeRunes eId xs, [ eof :: Cell (Rune d a) ] ]) RBT.empty
> 
> fromFingerTree
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a, IsChar a )
>   => EventId -> FT.FingerTree (MeasureText w t d) a -> Buffer w t d a
> fromFingerTree eId = fromList eId . Fold.toList
> 
> toList
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> [a]
> toList (Buffer w _) =
>   map getRuneValue $ unCells $ Fold.toList w

Due to the EOF sigil, detecting when a buffer is empty or a singleton is a little more involved.

> isEmpty
>   :: forall w t d a
>    . ( IsChar a, Eq a, IsWidth w, IsTab t, IsBase d
>      , Valued (MeasureText w t d) a )
>   => Buffer w t d a -> Bool
> isEmpty (Buffer w _) =
>   case TPL.viewAtEnd w of
>     Nothing -> error "isEmpty: panic (expected eof)"
>     Just (a, as) ->
>       (a == (eof :: Cell (Rune d a))) && (TPL.isEmpty as)
> 
> isSingleton
>   :: forall w t d a
>    . ( IsChar a, Eq a, IsWidth w, IsTab t, IsBase d
>      , Valued (MeasureText w t d) a )
>   => Buffer w t d a -> Bool
> isSingleton (Buffer w _) =
>   case TPL.viewAtEnd w of
>     Nothing -> error "isSingleton: panic (expected eof)"
>     Just (a, as) ->
>       (a == (eof :: Cell (Rune d a))) && (TPL.isSingleton as)

It will also be handy to be able to resize the buffer in two different ways, depending on whether we want to specify the new parameters 'statically' or 'dynamically'.

> resizeBuffer
>   :: forall w1 t1 w2 t2 d a
>    . ( IsBase d
>      , IsWidth w1, IsTab t1, Valued (MeasureText w1 t1 d) a
>      , IsWidth w2, IsTab t2, Valued (MeasureText w2 t2 d) a )
>   => Buffer w1 t1 d a -> Buffer w2 t2 d a
> resizeBuffer (Buffer c r) = Buffer (TPL.remeasure c) r
> 
> adjustBuffer
>   :: ( IsBase d
>      , IsWidth w1, IsTab t1, Valued (MeasureText w1 t1 d) a
>      , IsWidth w2, IsTab t2, Valued (MeasureText w2 t2 d) a )
>   => Proxy w2 -> Proxy t2 -> Proxy d
>   -> Buffer w1 t1 d a -> Buffer w2 t2 d a
> adjustBuffer _ _ _ = resizeBuffer

We'll also go ahead and define some special constructors for building buffers of a very specific structure. These should only be used for testing and debugging, but we need to define them early in the module so we can use them in examples as we go. These correspond to the nontrivial constructors of two-pointed lists.

> makeVacantBuffer
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Proxy w -> Proxy t -> Proxy d -> Proxy a
>   -> Buffer w t d a
> makeVacantBuffer _ _ _ _ =
>   Buffer (TPL.singleton (eof :: Cell (Rune d a))) RBT.empty
> 
> makePointOnlyBuffer
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy d
>   -> EventId -> [a] -> a -> [a] -> Buffer w t d a
> makePointOnlyBuffer _ _ _ eId as' x' bs' =
>   let (as, x, bs) = makeRunes2 eId as' x' bs'
>   in Buffer (TPL.makePointOnly
>     (map Cell as) (Cell x) (map Cell bs ++ [eof])) RBT.empty
> 
> makeCoincideBuffer
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy d
>   -> EventId -> [a] -> a -> [a] -> Buffer w t d a
> makeCoincideBuffer _ _ _ eId as' x' bs' =
>   let (as, x, bs) = makeRunes2 eId as' x' bs'
>   in Buffer (TPL.makeCoincide
>     (map Cell as) (Cell x) (map Cell bs ++ [eof])) RBT.empty
> 
> makePointMarkBuffer
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy d
>   -> EventId -> [a] -> a -> [a] -> a -> [a] -> Buffer w t d a
> makePointMarkBuffer _ _ _ eId as' x' bs' y' cs' =
>   let (as, x, bs, y, cs) = makeRunes3 eId as' x' bs' y' cs'
>   in Buffer (TPL.makePointMark
>     (map Cell as) (Cell x) (map Cell bs)
>     (Cell y) (map Cell cs ++ [eof])) RBT.empty
> 
> makeMarkPointBuffer
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy d
>   -> EventId -> [a] -> a -> [a] -> a -> [a] -> Buffer w t d a
> makeMarkPointBuffer _ _ _ eId as' x' bs' y' cs' =
>   let (as, x, bs, y, cs) = makeRunes3 eId as' x' bs' y' cs'
>   in Buffer (TPL.makeMarkPoint
>     (map Cell as) (Cell x) (map Cell bs)
>     (Cell y) (map Cell cs ++ [eof])) RBT.empty



Point Manipulation
------------------

The point and mark of a buffer represent the read head, and moving them around is one of the most important and common tasks our code needs to handle. We inherit a decent API for this from two-pointed lists which we can basically re-expose. First some queries on the point and mark:

> isPointAtStart
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> Bool
> isPointAtStart =
>   TPL.isPointAtStart . bufContents
> 
> isPointAtEnd
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> Bool
> isPointAtEnd =
>   TPL.isPointAtEnd . bufContents
> 
> hasMark
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> Bool
> hasMark =
>   TPL.hasMark . bufContents
> 
> isMarkAtStart
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> Bool
> isMarkAtStart =
>   TPL.isMarkAtStart . bufContents
> 
> isMarkAtEnd
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> Bool
> isMarkAtEnd =
>   TPL.isMarkAtEnd . bufContents

Next some basic operators for moving either the point or the mark to one of the ends of the buffer.

> movePointToStart
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> Buffer w t d a
> movePointToStart (Buffer w del) =
>   Buffer (TPL.movePointToStart w) del
> 
> movePointToEnd
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> Buffer w t d a
> movePointToEnd (Buffer w del) =
>   Buffer (TPL.movePointToEnd w) del
> 
> moveMarkToStart
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> Buffer w t d a
> moveMarkToStart (Buffer w del) =
>   Buffer (TPL.moveMarkToStart w) del
> 
> moveMarkToEnd
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> Buffer w t d a
> moveMarkToEnd (Buffer w del) =
>   Buffer (TPL.moveMarkToEnd w) del

We also have operators for moving the point and mark one cell to the left or right.

> movePointLeft
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> Buffer w t d a
> movePointLeft (Buffer w del) =
>   Buffer (TPL.movePointLeft w) del
> 
> movePointRight
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> Buffer w t d a
> movePointRight (Buffer w del) =
>   Buffer (TPL.movePointRight w) del
> 
> moveMarkLeft
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> Buffer w t d a
> moveMarkLeft (Buffer w del) =
>   Buffer (TPL.moveMarkLeft w) del
> 
> moveMarkRight
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> Buffer w t d a
> moveMarkRight (Buffer w del) =
>   Buffer (TPL.moveMarkRight w) del

We also have utilities for setting and clearing the mark. Recall that both `clearMark` and `leaveMark` leave the point unchanged; `clearMark` removes the mark, and `leaveMark` sets or resets it to coincide with the point.

> clearMark
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> Buffer w t d a
> clearMark (Buffer c r) =
>   Buffer (TPL.clearMark c) r
> 
> leaveMark
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> Buffer w t d a
> leaveMark (Buffer c r) =
>   Buffer (TPL.leaveMark c) r



Basic Mutation
--------------

Next we need buffer versions of the region operators. We can read the character at the point:

> readPoint
>   :: Buffer w t d a -> Maybe (Cell a)
> readPoint (Buffer w _) =
>   fmap (fmap getRuneValue) $ TPL.readPoint w

And we need left-biased insert and delete at the point.

> insertPointLeft
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a, IsChar a )
>   => EventId -> a -> Buffer w t d a -> (Buffer w t d a, BufferOp d a)
> insertPointLeft eId a (Buffer w del) =
>   let
>     u = case TPL.valuesAroundPoint w of
>       Nothing    -> (Infimum, Supremum)
>       Just (x,y) -> (runeId x, runeId y)
>     v = newRuneId eId u a
>   in (Buffer (TPL.insertPointLeft (Cell v) w) del, BufferOpIns v)
> 
> deletePointLeft
>   :: ( IsChar a, Ord a, IsWidth w, IsTab t, IsBase d
>      , Valued (MeasureText w t d) a )
>   => EventId -> Buffer w t d a -> (Buffer w t d a, BufferOp d a)
> deletePointLeft eId (Buffer w del) =
>   case TPL.deletePointLeft' w of
>     Nothing -> (Buffer w del, BufferNoOp)
>     Just (w', a) -> case a of
>       Cell c -> (Buffer w' (RBT.insert c del), BufferOpDel (setEventId eId c))
>       EOF -> error "deletePointLeft: panic"
> 

> {-

> copyRegion
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> Maybe (Buffer w t d a)
> copyRegion (Buffer w) =
>   fmap (fromFingerTree . FT.inflateWith listCell)
>     $ TPL.copyRegionL w



As well we have versions of `copy`, `cut`, and `insert`.

> 
> cutRegion
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> Maybe (Buffer w t d a, Buffer w t d a)
> cutRegion (Buffer w) = do
>   (region, rest) <- TPL.cutRegionL w
>   return
>     ( fromFingerTree $ FT.inflateWith listCell region
>     , Buffer rest )
> 
> insertRegion
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> Buffer w t d a -> Buffer w t d a
> insertRegion (Buffer snippet) (Buffer w) =
>   Buffer $ TPL.insertRegionL (TPL.integrate snippet) w
> 

> -}

> alterRegion
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => (a -> a) -> Buffer w t d a -> Buffer w t d a
> alterRegion f (Buffer c r) =
>   Buffer (TPL.alterRegionL g c) r
>   where
>     g :: Cell (Rune d a) -> Cell (Rune d a)
>     g x = case x of
>       Cell a -> Cell (fmap f a)
>       _ -> x

We can also efficiently manipulate the start and end of the buffer.

> insertAtStart
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a, IsChar a )
>   => EventId -> a -> Buffer w t d a -> (Buffer w t d a, BufferOp d a)
> insertAtStart eId a (Buffer w del) =
>   let
>     u = case TPL.valuesAroundStart w of
>       Nothing    -> (Infimum, Supremum)
>       Just (x,y) -> (runeId x, runeId y)
>     v = newRuneId eId u a
>   in (Buffer (TPL.insertAtStart (Cell v) w) del, BufferOpIns v)
> 
> prepend
>   :: forall w t d a
>    . ( Ord a, IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a, IsChar a )
>   => EventId -> [a] -> Buffer w t d a
>   -> (Buffer w t d a, [BufferOp d a])
> prepend eId cs b = prepend' b (reverse cs) []
>   where
>     prepend' x as ops = case as of
>       [] -> (x, ops)
>       z:zs ->
>         let (y, op) = insertAtStart eId z x
>         in prepend' y zs (op:ops)
> 
> deleteAtStart
>   :: ( IsChar a, Ord a, IsWidth w, IsTab t, IsBase d
>      , Valued (MeasureText w t d) a )
>   => EventId -> Buffer w t d a -> (Buffer w t d a, BufferOp d a)
> deleteAtStart eId (Buffer w del) =
>   case TPL.viewAtEnd w of
>     Nothing -> error "deleteAtStart: panic (empty buffer)"
>     Just (u, us) ->
>       case TPL.deleteAtStart' us of
>         Nothing -> (Buffer w del, BufferNoOp)
>         Just (w', a) -> case a of
>           EOF -> error "deletePointLeft: panic"
>           Cell c ->
>             ( Buffer (TPL.insertAtEnd u w') (RBT.insert c del)
>             , BufferOpDel (setEventId eId c)
>             )
> 
> insertAtEnd
>   :: forall w t d a
>    . ( Ord a, IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a, IsChar a )
>   => EventId -> a -> Buffer w t d a -> (Buffer w t d a, BufferOp d a)
> insertAtEnd eId a (Buffer w del) =
>   case TPL.viewAtEnd w of
>     Nothing -> error "insertAtEnd: panic (empty buffer)"
>     Just (z, us) ->
>       let
>         u = case TPL.viewAtEnd us of
>           Nothing    -> (Infimum, Supremum)
>           Just (x,y) -> (runeId (value x :: MeasureText w t d), Supremum)
>         v = newRuneId eId u a
>       in
>         ( Buffer (TPL.insertAtEnd z $ TPL.insertAtEnd (Cell v) us) del
>         , BufferOpIns v
>         )
> 
> append
>   :: forall w t d a
>    . ( Ord a, IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a, IsChar a )
>   => EventId -> [a] -> Buffer w t d a
>   -> (Buffer w t d a, [BufferOp d a])
> append eId cs b = append' b cs []
>   where
>     append' x as ops = case as of
>       [] -> (x, ops)
>       z:zs ->
>         let (y, op) = insertAtEnd eId z x
>         in append' y zs (op:ops)
> 
> deleteAtEnd
>   :: ( IsChar a, Ord a, IsWidth w, IsTab t, IsBase d
>      , Valued (MeasureText w t d) a )
>   => EventId -> Buffer w t d a -> (Buffer w t d a, BufferOp d a)
> deleteAtEnd eId (Buffer w del) =
>   case TPL.viewAtEnd w of
>     Nothing -> error "deleteAtEnd: panic (empty buffer)"
>     Just (u, us) ->
>       case TPL.viewAtEnd us of
>         Nothing -> (Buffer w del, BufferNoOp)
>         Just (a, w') -> case a of
>           EOF -> error "deletePointLeft: panic"
>           Cell c ->
>             ( Buffer (TPL.insertAtEnd u w') (RBT.insert c del)
>             , BufferOpDel (setEventId eId c)
>             )

> {-

We can also map over the entries in a buffer.

> mapBuffer
>   :: ( IsWidth w, IsTab t, IsBase d
>      , Valued (MeasureText w t d) a1
>      , Valued (MeasureText w t d) a2 )
>   => (a1 -> a2) -> Buffer w t d a1 -> Buffer w t d a2
> mapBuffer f (Buffer c r) =
>   Buffer (TPL.fmapTPL (fmap (fmap f)) c) r
> 
> mapRegion
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => (a -> a) -> Buffer w t d a -> Buffer w t d a
> mapRegion f =
>   Buffer . TPL.alterRegionL (fmap (fmap f)) . unBuffer

> -}

Queries
-------

More than any of the data structures we've seen so far, buffers have _properties_. We now define an interface through which other modules can query these properties; for consistency's sake these all start with `get`. First some functions for reifying the type level parameters: width and tab stop.

> getBufferWidth
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> Int
> getBufferWidth _ = toWidth (Proxy :: Proxy w)
> 
> getBufferTabStop
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> Int
> getBufferTabStop _ = toTab (Proxy :: Proxy t)

Next we have queries on the measure of the entire buffer.

> getBufferCharCount
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> Int
> getBufferCharCount (Buffer buf _) =
>   let m = value buf :: MeasureText w t d in
>   charCount m
> 
> getBufferByteCount
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> Int
> getBufferByteCount (Buffer buf _) =
>   let m = value buf :: MeasureText w t d in
>   byteCount m
> 
> getBufferLineCol
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> LineCol
> getBufferLineCol buf =
>   let m = value buf :: MeasureText w t d in
>   logicalCoords m
> 
> getBufferScreenCoords
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> (Int, Int)
> getBufferScreenCoords (Buffer buf _) =
>   let m = value buf :: MeasureText w t d in
>   applyScreenOffset (screenCoords m) (0,0)
> 
> getBufferScreenOffset
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> (Int, Int)
> getBufferScreenOffset (Buffer buf _) =
>   let m = value buf :: MeasureText w t d in
>   applyScreenOffset (screenCoords m <> screenOffset m) (0,0)

Likewise we can query the value at the point.

> getPointCharCount
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> Int
> getPointCharCount (Buffer w _) =
>   case TPL.valueUpToPoint w of
>     Nothing -> 0
>     Just m -> charCount m
> 
> getPointByteCount
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> Int
> getPointByteCount (Buffer w _) =
>   case TPL.valueUpToPoint w of
>     Nothing -> 0
>     Just m -> byteCount m
> 
> getPointLineCol
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> LineCol
> getPointLineCol (Buffer w _) =
>   case TPL.valueUpToPoint w of
>     Nothing -> mempty
>     Just m -> logicalCoords m
>  
> getPointScreenCoords
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> (Int, Int)
> getPointScreenCoords (Buffer w _) =
>   case TPL.valueUpToPoint w of
>     Nothing -> (0,0)
>     Just m -> applyScreenOffset (screenCoords m) (0,0)
> 
> getPointScreenOffset
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> (Int, Int)
> getPointScreenOffset (Buffer w _) =
>   case TPL.valueUpToPoint w of
>     Nothing -> (0,0)
>     Just m -> applyScreenOffset (screenCoords m <> screenOffset m) (0,0)
> 
> getPointRuneId
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> Augmented (RuneId d)
> getPointRuneId (Buffer w _) =
>   case TPL.valueBeforePoint w of
>     Nothing -> Infimum
>     Just m -> runeId m

And for good measure, at the mark as well.

> getMarkCharCount
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> Int
> getMarkCharCount (Buffer w _) =
>   case TPL.valueUpToMark w of
>     Nothing -> 0
>     Just m -> charCount m
> 
> getMarkByteCount
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> Int
> getMarkByteCount (Buffer w _) =
>   case TPL.valueUpToMark w of
>     Nothing -> 0
>     Just m -> byteCount m
> 
> getMarkLineCol
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> LineCol
> getMarkLineCol (Buffer w _) =
>   case TPL.valueUpToMark w of
>     Nothing -> mempty
>     Just m -> logicalCoords m
>  
> getMarkScreenCoords
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> (Int, Int)
> getMarkScreenCoords (Buffer w _) =
>   case TPL.valueUpToMark w of
>     Nothing -> (0,0)
>     Just m -> applyScreenOffset (screenCoords m) (0,0)



Splitting
---------

Recall that finger trees, the structure underlying our buffers, admit an efficient _splitting_ operation, which we can take advantage of to move the point and mark to specific locations in the buffer. First we define a utility function, not exposed outside this module, that attempts to split a buffer on an arbitrary predicate.

> movePoint
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => (MeasureText w t d -> Bool)
>   -> Buffer w t d a -> Maybe (Buffer w t d a)
> movePoint pointP (Buffer w r) =
>   Buffer <$> TPL.splitPoint pointP w <*> pure r

For ergonomics' sake we'll expose specialized splitting functions. First at a given line and column position:

> movePointToLineCol
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a, Eq a )
>   => LineCol -> Buffer w t d a -> Buffer w t d a
> movePointToLineCol lc buf =
>   case movePoint (atOrAfterLineCol lc) buf of
>     Nothing -> movePointToEnd buf
>     Just xs -> xs
> 
> atOrAfterLineCol
>   :: LineCol -> MeasureText w t d -> Bool
> atOrAfterLineCol lc m =
>   lc < (logicalCoords m) <> (logicalOffset m)

We should also test some simple cases to make sure we understand how this splitting works.

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "abc\nd" 'e' "f\nghi"
> --   y = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "" 'a' "bc\ndef\nghi"
> -- in y == movePointToLineCol (LineCol 0 0) x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2 nat10  (EventId 0 "t")
> --     "abc\nd" 'e' "f\nghi"
> --   y = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "abc\ndef\ng" 'h' "i"
> -- in y == movePointToLineCol (LineCol 2 1) x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "abc\nd" 'e' "f\nghi"
> --   y = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "abc" '\n' "def\nghi"
> -- in y == movePointToLineCol (LineCol 0 4) x
> -- :}
> -- True

:::

Next we'd like to split at a given pair of screen coordinates. It's straightforward how this should work if we try to split at a pair of coordinates corresponding to a character in the buffer (split there). It's also (slightly less but still fairly) straightforward how to handle coordinates that fall off the end of the buffer; we'll just clamp them to the EOF sigil. But there's a universe of pairs whose $y$ coordinates correspond to _lines_ on the screen, but whose $x$ coordinates march off the right edge. What should these do?

One option is to just fail. But I think a better option is to try our best to return a reasonable result: in this case, to move the point to the last position on the screen line. In some sense that is "as close as we can get" in this case.

> movePointToScreenCoords
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a, Eq a )
>   => (Int, Int) -> Buffer w t d a -> Buffer w t d a
> movePointToScreenCoords pos buf =
>   if pos == (0,0)
>     then movePointToStart buf
>     else case movePoint (atOrAfterScreenCoords pos) buf of
>       Nothing -> movePointToEnd buf
>       Just xs -> xs
> 
> atOrAfterScreenCoords
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => (Int, Int) -> MeasureText w t d -> Bool
> atOrAfterScreenCoords (u,v) m =
>   let
>     (h,k) = applyScreenOffset (screenCoords m <> screenOffset m) (0,0)
>   in (v < k) || ((v == k) && (u < h))

We can test our understanding with some examples.

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "abc\nde" 'f' "\nghi"
> --   y = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "" 'a' "bc\ndef\nghi"
> -- in y == movePointToScreenCoords (0,0) x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "abc\nde" 'f' "\nghi"
> --   y = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "a" 'b' "c\ndef\nghi"
> -- in y == movePointToScreenCoords (1,0) x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "abc\ndefg" 'h' "i\njkl"
> --   y = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "abc\nde" 'f' "ghi\njkl"
> -- in y == movePointToScreenCoords (2,1) x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "ab" 'c' "defgh"
> --   y = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "abcdefg" 'h' ""
> -- in y == movePointToScreenCoords (7,0) x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "ab" 'c' "defghijklmnop"
> --   y = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "abcdefghij" 'k' "lmnop"
> -- in y == movePointToScreenCoords (2,1) x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "abc\nde\nf" 'g' "h"
> --   y = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "abc\nde" '\n' "fgh"
> -- in y == movePointToScreenCoords (5,1) x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "abcdefghijklmn" 'o' "pqrst"
> --   y = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "abcdefg" 'h' "ijklmnopqrst"
> -- in y == movePointToScreenCoords (10,0) x
> -- :}
> -- True

:::

Next we define a utility which moves the point to the first character of a given screen line -- which is crucial for rendering buffers to the screen. This is a little trickier than moving to a specific line and character or pair of screen coordinates, because it involves a _relative_ position and nailing down exactly what it means to say that a character is at the start of a new screen line.

Suppose the final screen coordinate in the buffer is $(W,H)$ and we want to move to the first position of screen line $y$. If $y$ is in the interval $[0,H]$, then some character has screen coordinate $(0,y)$ and we can find it with an ordinary split. Similarly simple, if $y$ is at least $H+2$ then no such character exists, and the EOF sigil can't help us. The delicate case is when we want to move to the first position of screen line $H+1$. This case depends on (1) the final character in the buffer and (2) the width of the final screen line in the buffer (that is, $W$). If the final character is a newline, we'd like to consider the EOF sigil to be the start of the next screen line. If the final character is not a newline, then we consider the EOF sigil to be on the last line (not the start of the next line) _unless_ the final character's effective width plus $W$ exceeds the screen width.

That's a mouthful!

> movePointToScreenLine
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a, Eq a, IsChar a )
>   => Int -> Buffer w t d a -> Maybe (Buffer w t d a)
> movePointToScreenLine k buf =
>   let
>     (_,h) = getBufferScreenOffset buf
>     bW = toWidth (Proxy :: Proxy w)
>   in if k > 1 + h
>     then Nothing
>     else if k <= 0
>       then Just $ movePointToStart buf
>       else do
>         z <- movePoint (atOrAfterScreenLine k) buf
>         let
>           (u,v) = getPointScreenOffset z
>         if (k < 1 + h) || ((v == 1 + h) && ((u == bW - 1)))
>           then Just z
>           else Nothing
> 
> atOrAfterScreenLine
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => Int -> MeasureText w t d -> Bool
> atOrAfterScreenLine k m =
>   let
>     offset = (screenCoords m) <> (screenOffset m)
>     (u,v) = applyScreenOffset offset (0,0)
>   in (v > k) || ((v == k) && (u >= 1)) || (hasEOF m)

And some example cases:

::: doctest

> -- $
> -- Split at line 0 < h
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "abc\nde" 'f' "\nghi"
> --   y = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "" 'a' "bc\ndef\nghi"
> -- in Just y == movePointToScreenLine 0 x
> -- :}
> -- True
> --
> -- Split at line 1 < h
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "abc\nde\nf" 'g' "h"
> --   y = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "abc\n" 'd' "e\nfgh"
> -- in Just y == movePointToScreenLine 1 x
> -- :}
> -- True
> --
> -- Split at line 2 < h
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "abc\n" 'd' "e\nfgh"
> --   y = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "abc\nde\n" 'f' "gh"
> -- in Just y == movePointToScreenLine 2 x
> -- :}
> -- True
> --
> -- Split at line h+1 (last cell is an interior char)
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "" 'a' ""
> -- in Nothing == movePointToScreenLine 1 x
> -- :}
> -- True
> --
> -- Split at line h+1 (last cell is a newline)
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "" '\n' ""
> --   y = movePointToEnd x
> -- in Just y == movePointToScreenLine 1 x
> -- :}
> -- True
> --
> -- Split at line h+1 (last cell is an end char)
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "" 'a' "aaaaaaa"
> --   y = movePointToEnd x
> -- in Just y == movePointToScreenLine 1 x
> -- :}
> -- True
> --
> -- Split at line h+1 (last cell is an end tab)
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "" '\t' "\t\t\t"
> --   y = movePointToEnd x
> -- in Just y == movePointToScreenLine 1 x
> -- :}
> -- True
> --
> -- Split at line 1 == h (last cell is a newline)
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "" '\n' "\n"
> --   y = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "\n" '\n' ""
> -- in Just y == movePointToScreenLine 1 x
> -- :}
> -- True

:::

With `movePointToScreenCoords` in hand, we can also figure out the coordinates "nearest" to a given pair which actually appear in the buffer.

> seekScreenCoords
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a, Eq a )
>   => (Int, Int) -> Buffer w t d a -> (Int, Int)
> seekScreenCoords z =
>   getPointScreenCoords . movePointToScreenCoords z

And some tests for our intuition:

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "abc\nd" 'e' "f\ngh"
> -- in seekScreenCoords (0,0) x
> -- :}
> -- (0,0)
> --
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "abc\nd" 'e' "f\ngh"
> -- in seekScreenCoords (2,0) x
> -- :}
> -- (2,0)
> --
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "abc\nd" 'e' "f\ngh"
> -- in seekScreenCoords (3,0) x
> -- :}
> -- (3,0)
> --
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "abc\nd" 'e' "f\ngh"
> -- in seekScreenCoords (4,0) x
> -- :}
> -- (3,0)

:::

Finally, we'll also need to split the buffer at a given rune ID.

> movePointToRuneId
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a, Eq a )
>   => Augmented (RuneId d) -> Buffer w t d a -> Buffer w t d a
> movePointToRuneId rId buf =
>   case movePoint (atOrAfterRuneId rId) buf of
>     Nothing -> movePointToEnd buf
>     Just xs -> xs
> 
> atOrAfterRuneId
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => Augmented (RuneId d) -> MeasureText w t d -> Bool
> atOrAfterRuneId rId m =
>   rId < (runeId m)

And with this splitting in hand, we can apply reified buffer operations.

> applyBufferOp
>   :: forall w t d a
>    . ( IsChar a, IsWidth w, IsTab t, IsBase d
>      , Valued (MeasureText w t d) a, Eq a, Ord a )
>   => BufferOp d a -> Buffer w t d a -> Buffer w t d a
> applyBufferOp op buf = case op of
>   BufferNoOp -> buf
> 
>   BufferOpIns rune ->
>     if RBT.member rune (bufRemnants buf)
>       then buf
>       else
>         let
>           buf' = movePointToRuneId (Augmented (getRuneId rune)) buf
>           pId = getPointRuneId buf'
>           Buffer contents remnants = buf'
>         in if pId == Augmented (getRuneId rune)
>           then buf
>           else Buffer (TPL.insertPointLeft (Cell rune) contents) remnants
> 
>   BufferOpDel rune ->
>     if RBT.member rune (bufRemnants buf)
>       then buf
>       else 
>         let
>           buf' = movePointToRuneId (Augmented (getRuneId rune)) buf
>           pId = getPointRuneId buf'
>           Buffer contents remnants = buf'
>         in if pId /= Augmented (getRuneId rune)
>           then Buffer contents (RBT.insert rune remnants)
>           else Buffer (TPL.deletePointLeft contents) (RBT.insert rune remnants)

::: doctest

> -- $
> -- >>> :{
> -- -- Delete and insert the same rune, but in different orders
> -- let
> --   rune = newRuneId (EventId 0 "foo") (Infimum, Supremum) 'a'
> --   op1 = BufferOpDel rune
> --   op2 = BufferOpIns rune
> --   e = makeVacantBuffer nat8 nat2 nat3 (Proxy :: Proxy Char)
> --   zig = applyBufferOp op1 (applyBufferOp op2 e)
> --   zag = applyBufferOp op2 (applyBufferOp op1 e)
> -- in zig == zag
> -- :}
> -- True
> --
> -- $
> -- >>> :{
> -- -- Delete and insert runes that differ only by a color in different orders
> -- let
> --   rune1 = newRuneId (EventId 0 "foo") (Infimum, Supremum) $
> --     Glyph 'A' (gray24 0) (gray24 0)
> --   rune2 = newRuneId (EventId 0 "foo") (Infimum, Supremum) $
> --     Glyph 'A' (rgb6 0 0 0) (rgb6 0 0 0)
> --   op1 = BufferOpIns rune1
> --   op2 = BufferOpDel rune2
> --   e = makeVacantBuffer nat8 nat2 nat3 (Proxy :: Proxy (Glyph Char))
> --   zig = applyBufferOp op1 (applyBufferOp op2 e)
> --   zag = applyBufferOp op2 (applyBufferOp op1 e)
> -- in zig == zag
> -- :}
> -- True

:::





Rendering
---------

Buffers are a data structure for storing and manipulating text, but they're not very helpful for _viewing_ the text. We'll also need some code to convert the internal representation of a buffer into a format suitable for rendering on a screen.

We're making a major simplifying assumption that comes in handy here: text will be drawn in a monospaced typeface, meaning we can pretend that the display window is divided into a rectangular grid of character positions and each character occupies a whole number of grid cells.

The buffer is naturally divided into _screen lines_ -- these are contiguous chunks of text that will render at the same $y$ coordinate. We've set up the `MeasureText` type so that these are easy to find. All but the last of these screen lines will be _full_, meaning that every cell is occupied (possibly by a newline or tab). The last screen line is not full; this is where the EOF sigil hangs out.

To flesh out the theory of buffers, we'll include here a function which detects whether or not the buffer has a full screen line.

> hasFullScreenLine
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> Bool
> hasFullScreenLine w =
>   let
>     m = value w :: MeasureText w t d
>     (_,h) = applyScreenOffset (screenCoords m <> screenOffset m) (0,0)
>   in h > 0

This function is primarily used in testing to help scaffold the consistency of our main rendering code. Here are some examples.

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "" 'a' "bc"
> -- in hasFullScreenLine x
> -- :}
> -- False
> --
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "" 'a' "bc\n"
> -- in hasFullScreenLine x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "" 'a' "bcdefgh"
> -- in hasFullScreenLine x
> -- :}
> -- True

:::

To render the buffer we'll first need to extract the visible screen lines. Working toward that goal, we'll start with something simpler: extracting the _first_ screen line from a buffer. Note the return type; we can always extract _something_ from a valid buffer, even if it's just the EOF sigil. The second entry in the return type indicates whether this happened -- it's `Nothing` if the EOF appears in the first entry, and `Just` otherwise.

> takeFirstScreenLine
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a, Eq a, IsChar a )
>   => Buffer w t d a
>   -> ( FT.FingerTree (MeasureText w t d) (Cell a)
>      , Maybe (Buffer w t d a) )
> takeFirstScreenLine w =
>   let
>     m = value w :: MeasureText w t d
>     (_,h) = applyScreenOffset (screenCoords m <> screenOffset m) (0,0)
>   in if h == 0
>     then ( FT.fmapFT (fmap getRuneValue) $ TPL.integrate $ bufContents w, Nothing)
>     else
>       let
>         -- This pattern must match.
>         Just (Buffer (TPL.PointOnly (as, x, bs)) _) =
>           movePointToScreenLine 1 (clearMark w)
>       in ( FT.fmapFT (fmap getRuneValue) as, Just $ Buffer (TPL.PointOnly (mempty, x, bs)) RBT.empty )

It is crucial that we understand exactly how this function works, since the rest of the rendering code is built on it. Here are some examples covering some of the different ways a buffer can be populated.

::: doctest

> -- $
> -- >>> -- 'Normal' text with a newline
> -- >>> :set -XFlexibleContexts
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "abc\ndef" 'g' "\nhi"
> --   y = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "" 'd' "efg\nhi"
> --   z = FT.fromList $ map Cell
> --     "abc\n"
> -- in (z, Just y) == takeFirstScreenLine x
> -- :}
> -- True
> --
> -- >>> -- No full screen lines
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "a" 'b' "c"
> --   y = FT.fromList
> --     [ Cell 'a', Cell 'b', Cell 'c', EOF ]
> -- in (y, Nothing) == takeFirstScreenLine x
> -- :}
> -- True
> --
> -- >>> -- All newlines
> -- >>> :set -XFlexibleContexts
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "\n\n\n" '\n' "\n"
> --   y = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "" '\n' "\n\n\n"
> --   z = FT.fromList $ map Cell
> --     "\n"
> -- in (z, Just y) == takeFirstScreenLine x
> -- :}
> -- True
> --
> -- >>> -- Full screen line with no newlines
> -- >>> :set -XFlexibleContexts
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "abcdefghij" 'k' "lmn"
> --   y = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "" 'i' "jklmn"
> --   z = FT.fromList $ map Cell
> --     "abcdefgh"
> -- in (z, Just y) == takeFirstScreenLine x
> -- :}
> -- True

:::

After taking the first screen line, the next step in complexity is to take some number of screen lines from the front of the buffer. This is a pretty straightforward application of an accumulating recursive function.

> takeScreenLines
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a, Eq a, IsChar a )
>   => Int -> Buffer w t d a
>   -> ( [ FT.FingerTree (MeasureText w t d) (Cell a) ]
>      , Maybe (Buffer w t d a) )
> takeScreenLines n = accum 0 []
>   where
>     accum
>       :: Int
>       -> [ FT.FingerTree (MeasureText w t d) (Cell a) ]
>       -> Buffer w t d a
>       -> ( [ FT.FingerTree (MeasureText w t d) (Cell a) ]
>          , Maybe (Buffer w t d a) )
>     accum k as buf =
>       if (k >= n)
>         then (reverse as, Just buf)
>         else case takeFirstScreenLine buf of
>           (a, Nothing) ->
>             (reverse (a:as), Nothing)
>           (a, Just buf') ->
>             accum (k+1) (a:as) buf'

Interesting examples of this are a little more verbose:

::: doctest

> -- $
> -- >>> :set -XFlexibleContexts
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "abc\ndef" 'g' "\nhi"
> --   y = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "" 'h' "i"
> --   zs = 
> --     [ FT.fromList $ map Cell
> --         "abc\n"
> --     , FT.fromList $ map Cell
> --         "defg\n"
> --     ]
> -- in (zs, Just y) == takeScreenLines 2 x
> -- :}
> -- True
> --
> -- $
> -- >>> :set -XFlexibleContexts
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "abc\nd" 'e' "f"
> --   zs = 
> --     [ FT.fromList $ map Cell
> --         "abc\n"
> --     , FT.fromList
> --         [ Cell 'd', Cell 'e', Cell 'f', EOF ]
> --     ]
> -- in (zs, Nothing) == takeScreenLines 3 x
> -- :}
> -- True

:::

Now we're prepared to extract a 'screenful' of screen lines from anywhere in the buffer with `getScreenLines`. This function returns the accumulated `MeasureText` up to the extracted lines; we'll need this to compute the (logical) line numbers.

> getScreenLines
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a, Eq a, IsChar a )
>   => Int -- top screen line
>   -> Int -- view height
>   -> Buffer w t d a
>   -> ( MeasureText w t d
>      , [ FT.FingerTree (MeasureText w t d) (Cell a) ] )
> getScreenLines t h buf =
>   case movePointToScreenLine t buf of
>     Nothing -> ( value buf, [] )
>     Just (Buffer w _) -> case w of
>       TPL.Vacant -> (mempty, [])
>       TPL.PointOnly (as, x, bs) ->
>         let
>           (us, _) = takeScreenLines h
>             (Buffer (TPL.PointOnly (mempty, x, bs)) RBT.empty)
>         in (value as, us)
>       TPL.Coincide (as, x, bs) ->
>         let
>           (us, _) = takeScreenLines h
>             (Buffer (TPL.PointOnly (mempty, x, bs)) RBT.empty)
>         in (value as, us)
>       TPL.PointMark (as, x, bs, y, cs) ->
>         let
>           (us, _) = takeScreenLines h
>             (Buffer (TPL.PointOnly (mempty, x, bs <> FT.cons y cs)) RBT.empty)
>         in (value as, us)
>       TPL.MarkPoint (as, x, bs, y, cs) ->
>         let
>           (us, _) = takeScreenLines h
>             (Buffer (TPL.PointOnly (mempty, y, cs)) RBT.empty)
>         in (value (as <> FT.cons x bs), us)

Next we can attach the logical line numbers to the screen lines. (_Logical_ here means lines as separated by newline characters; these can be broken across several screen lines.)

> attachLineNumbers
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a, Eq a, IsChar a )
>   => ( MeasureText w t d
>      , [ FT.FingerTree (MeasureText w t d) (Cell a) ] )
>   -> [ ( Maybe Int, FT.FingerTree (MeasureText w t d) (Cell a) ) ]
> attachLineNumbers (ctx, xs) = case xs of
>   [] -> []
>   u:us ->
>     let
>       LineCol k h = logicalCoords ctx <> logicalOffset ctx
>       v = if h == 0 then Just k else Nothing
>     in (v,u) : attachLineNumbers ( ctx <> value u, us )

We will also need to know the expected column index of each rendered character later on -- this is so we can render tabs correctly.

> attachColumnIndices
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a, Eq a, IsChar a )
>   => FT.FingerTree (MeasureText w t d) (Cell a)
>   -> [(a, Int)]
> attachColumnIndices xs =
>   let
>     ys :: FT.FingerTree (MeasureText w t d) a
>     ys = FT.inflateWith listCell xs
> 
>     f :: (a, MeasureText w t d) -> (a, Int)
>     f (a, m) =
>       let (w, _) = applyScreenOffset (screenCoords m) (0,0)
>       in (a, w)
>   in
>     map f $ FT.toAnnotatedList ys

Putting it all together, `renderScreenLinesWithRegion` extracts (1) a list of screen lines with column indices attached to each character, and (2) a list of logical line numbers.

> renderScreenLinesWithRegion
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a, Eq a, IsChar a )
>   => (a -> a) -- apply to the region
>   -> Int      -- top screen line
>   -> Int      -- view height
>   -> Buffer w t d a
>   -> ([Maybe Int], [[(a, Int)]])
> renderScreenLinesWithRegion f t h buf =
>   unzip
>     $ map (\(u, v) -> (u, attachColumnIndices v))
>     $ attachLineNumbers
>     $ getScreenLines t h
>     $ alterRegion f buf

Testing this "by hand" is awkward, but we can do it.

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "" 'a' ""
> --   nums = [Just 0]
> --   cells = [[('a',0)]]
> -- in (nums, cells) ==
> --     renderScreenLinesWithRegion id 0 1 x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "" 'a' "b"
> --   nums = [Just 0]
> --   cells = [[('a',0),('b',1)]]
> -- in (nums, cells) ==
> --     renderScreenLinesWithRegion id 0 1 x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "" '\n' ""
> --   nums = [Just 0]
> --   cells = [[('\n',0)]]
> -- in (nums, cells) ==
> --     renderScreenLinesWithRegion id 0 1 x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2 nat10 (EventId 0 "t")
> --     "" '\t' "a"
> --   nums = [Just 0]
> --   cells = [[('\t',0),('a',2)]]
> -- in (nums, cells) ==
> --     renderScreenLinesWithRegion id 0 1 x
> -- :}
> -- True

:::



Testing and Debugging
---------------------

As usual, we wrap up this module with some helper code for writing tests. First we need class instances for working with our property testing library.

> instance
>   ( IsWidth w, IsTab t, IsChar a, IsBase d, Eq a
>   , Arb a, Valued (MeasureText w t d) a
>   ) => Arb (Buffer w t d a)
>   where
>     arb = do
>       cs <- arb
>       let
>         meas = (runeId :: MeasureText w t d -> Augmented (RuneId d)) . (value :: Rune d a -> MeasureText w t d)
>         xs = TPL.fromList $ map Cell $ sortOn meas cs
>       return $ Buffer (TPL.insertAtEnd (eof :: Cell (Rune d a)) xs) RBT.empty
> 
> instance
>   ( IsWidth w, IsTab t, IsChar a, IsBase d, Eq a
>   , Prune a, Valued (MeasureText w t d) a
>   ) => Prune (Buffer w t d a)
>   where
>     prune (Buffer x _) =
>       case TPL.viewAtEnd x of
>         Nothing -> error "prune: panic (expected eof)"
>         Just (c, cs) ->
>           [Buffer (TPL.insertAtEnd c z) RBT.empty | z <- prune cs]
> 
> instance
>   ( IsBase d, IsChar a, Eq a, Arb a
>   ) => Arb (BufferOp d a)
>   where
>     arb = pickFrom3
>       ( BufferOpIns <$> arb
>       , BufferOpDel <$> arb
>       , pure BufferNoOp
>       )
> 
> instance
>   ( IsBase d, IsChar a, Eq a, Prune a
>   ) => Prune (BufferOp d a)
>   where
>     prune x = case x of
>       BufferNoOp -> []
>       BufferOpIns r -> map BufferOpIns $ prune r
>       BufferOpDel r -> map BufferOpDel $ prune r

Next, recall that buffers need to satisfy some invariants: first of all they are built on finger trees, which have invariants of their own, but moreover the buffer must end with an EOF sigil. We expose a helper to check for this.

> validate
>   :: ( IsChar a, IsWidth w, IsTab t, IsBase d
>      , Valued (MeasureText w t d) a, Eq a )
>   => Buffer w t d a -> Bool
> validate (Buffer w _) =
>   case TPL.viewAtEnd w of
>     Nothing ->
>       False
>     Just (u, _) -> 
>       (u == eof) && (TPL.validate w)

We also expose a function that converts a buffer into a list with all the gritty value details exposed.

> toAnnotatedList
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> [(Cell (Rune d a), MeasureText w t d)]
> toAnnotatedList = TPL.toAnnotatedList . bufContents

Finally we give a `Show` instance to match our structure-aware constructors. This is filed with the debugging methods because we won't need it in "production".

> instance
>   ( Show a, IsWidth w, IsTab t, IsBase d, IsChar a, Ord a
>   ) => Show (Buffer w t d a)
>   where
>     show (Buffer w r) =
>       let
>         wid = showWidth (Proxy :: Proxy w)
>         tab = showTab (Proxy :: Proxy t)
>       in case w of
>         TPL.Vacant -> concat
>           [ "makeVacantBuffer"
>           , wid, " ", tab
>           , "\ndeleted: ", show $ RBT.toList r
>           ]
>         TPL.PointOnly (as, x, bs) -> concat
>           [ "makePointOnlyBuffer "
>           , wid, " ", tab, " "
>           , show $ Fold.toList as, " "
>           , show x, " "
>           , show $ Fold.toList bs
>           , "\ndeleted: ", show $ RBT.toList r
>           ]
>         TPL.Coincide (as, x, bs) -> concat
>           [ "makeCoincide "
>           , wid, " ", tab, " "
>           , show $ Fold.toList as, " "
>           , show x, " "
>           , show $ Fold.toList bs
>           , "\ndeleted: ", show $ RBT.toList r
>           ]
>         TPL.PointMark (as, x, bs, y, cs) -> concat
>           [ "makePointMarkBuffer "
>           , wid, " ", tab, " "
>           , show $ Fold.toList as, " "
>           , show x, " "
>           , show $ Fold.toList bs, " "
>           , show y, " "
>           , show $ Fold.toList cs
>           , "\ndeleted: ", show $ RBT.toList r
>           ]
>         TPL.MarkPoint (as, x, bs, y, cs) -> concat
>           [ "makeMarkPointBuffer "
>           , wid, " ", tab, " "
>           , show $ Fold.toList as, " "
>           , show x, " "
>           , show $ Fold.toList bs, " "
>           , show y, " "
>           , show $ Fold.toList cs
>           , "\ndeleted: ", show $ RBT.toList r
>           ]
