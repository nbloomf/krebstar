---
title: Buffers
---

::: contents
* [Introduction](#introduction): The problem we're solving
* [Point Manipulation](#point-manipulation): Moving the read heads around
* [Basic Mutation](#basic-mutation): Cut, copy, alter, insert
* [Queries](#queries): Learning things about buffers
* [Splitting](#splitting): Moving the read heads around (again)

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
>   , empty
>   , singleton
>   , fromList
>   , fromFingerTree
>   , toList
>   , isEmpty
>   , isSingleton
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
>   , copyRegion
>   , cutRegion
>   , insertRegion
>   , alterRegion
>   , insertAtStart
>   , viewAtStart
>   , deleteAtStart
>   , insertAtEnd
>   , viewAtEnd
>   , deleteAtEnd
>   , mapBuffer
>   , mapRegion
> 
>   , getBufferWidth
>   , getBufferTabStop
>   , getBufferCharCount
>   , getBufferByteCount
>   , getBufferLineCol
>   , getBufferScreenCoords
>   , getPointCharCount
>   , getPointByteCount
>   , getPointLineCol
>   , getPointScreenCoords
>   , getMarkCharCount
>   , getMarkByteCount
>   , getMarkLineCol
>   , getMarkScreenCoords
> 
>   , movePointToLineCol
>   , movePointToScreenCoords
>   , movePointToScreenLine
>   , atOrAfterLineCol
>   , atOrAfterScreenCoords
>   , atOrAfterScreenLine




>   -- * Constructors
> 

>   , adjustBuffer
>   , resizeBuffer

>   -- * Queries
> 
>   , seekScreenCoords
> 

> 
>   -- * Rendering
>   , BufferRenderSettings(..)
>   , defaultBufferRenderSettings
>   , takeLine
>   , takeLines
>   , splitLines
>   , getLineNumbers
>   , renderBuffer
> 
>   -- * Debugging
>   , makePointOnlyBuffer
>   , makeCoincideBuffer
>   , makePointMarkBuffer
>   , makeMarkPointBuffer

>   , toAnnotatedList
> 
>   , validate
> ) where
> 
> import Data.Proxy
> import qualified Data.Foldable as Fold
> import Data.List (groupBy)
> import qualified Data.Map.Strict as M
> import Control.Monad (join)
> import Debug.Trace
> 
> import           Kreb.Check
> import           Kreb.Reflect
> import           Kreb.Struct.Valued
> import qualified Kreb.Struct.FingerTree as FT
> import qualified Kreb.Struct.TwoPointedList as TPL
> 
> import Kreb.Text.ScreenOffset
> import Kreb.Text.MeasureText
> import Kreb.Text.Glyph
> import Kreb.Text.Cell

:::



Introduction
------------

Our primary data structure for representing and manipulating text is the _buffer_. At the semantic level we can treat buffers like lists of characters, although under the hood they use a more complex representation to make some important operations more efficient. The buffer also has one and possibly two distinguished positions, called the _point_ and the _mark_, which are used to specify where edits take effect.

The actual definition of buffers builds on two other abstractions: two-pointed lists and our text measurement type. However these details aren't exposed to client code.

> newtype Buffer w t a = Buffer
>   { unBuffer :: TPL.TwoPointedList (MeasureText w t) (Cell a)
>   } deriving Eq

Note that `MeasureText` takes two type parameters, `w` and `t`. These are type level representations of the width of the screen in cells and the tab stop width, respectively. Our underlying finger tree implementation makes it necessary that these be type parameters; essentially they are required by the monoid instance on text measurements. Later on we'll wrap buffers inside an existential type to partially hide this detail while maintaining type safety.

We also impose one invariant on buffers that can't be represented in the type: the final item in the list of characters must be a special end-of-file sigil. This simplifies the usual insert and delete operations on text by making it possible to assert that the "cursor" is at the left edge of a cell. We will have to be careful that our operations on buffers maintain this invariant (only in this module; client code shouldn't need to worry about that).

Because buffers are "just" two pointed lists, they inherit the API of two pointed lists, giving us a decent amount of code nearly for free (modulo maintaining the eof invariant). We can measure the entire buffer:

> instance
>   ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>   ) => Valued (MeasureText w t) (Buffer w t a)
>   where
>     value = value . TPL.integrate . unBuffer

We also get the usual `empty` and `singleton` constructors.

> empty
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a
> empty = Buffer $
>   TPL.singleton (eof :: Cell a)
> 
> singleton
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => a -> Buffer w t a
> singleton a = Buffer $
>   TPL.makeFromList [ Cell a, eof ]

More generally, we can convert lists to buffers (and back again).

> fromList
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => [a] -> Buffer w t a
> fromList xs = Buffer $ TPL.makeFromList $ concat
>   [ map Cell xs, [ eof :: Cell a ] ]
> 
> fromFingerTree
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => FT.FingerTree (MeasureText w t) a -> Buffer w t a
> fromFingerTree xs = Buffer $ TPL.fromFingerTree $
>   FT.snoc (eof :: Cell a) (FT.fmapFT Cell xs)
> 
> toList
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> [a]
> toList (Buffer w) =
>   unCells $ Fold.toList w

Due to the EOF sigil, detecting when a buffer is empty or a singleton is a little more involved.

> isEmpty
>   :: forall w t a
>    . ( Eq a, IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Bool
> isEmpty (Buffer w) =
>   case TPL.viewAtEnd w of
>     Nothing -> error "isEmpty: panic (expected eof)"
>     Just (a, as) ->
>       (a == (eof :: Cell a)) && (TPL.isEmpty as)
> 
> isSingleton
>   :: forall w t a
>    . ( Eq a, IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Bool
> isSingleton (Buffer w) =
>   case TPL.viewAtEnd w of
>     Nothing -> error "isSingleton: panic (expected eof)"
>     Just (a, as) ->
>       (a == (eof :: Cell a)) && (TPL.isSingleton as)

We'll also go ahead and define some special constructors for building buffers of a very specific structure. These should only be used for testing and debugging, but we need to define them early in the module so we can use them in examples as we go. These correspond to the nontrivial constructors of two-pointed lists.

> makePointOnlyBuffer
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Proxy w -> Proxy t
>   -> [a] -> a -> [a] -> Buffer w t a
> makePointOnlyBuffer _ _ as x bs =
>   Buffer $ TPL.makePointOnly
>     (map Cell as) (Cell x) (map Cell bs ++ [eof])
> 
> makeCoincideBuffer
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Proxy w -> Proxy t
>   -> [a] -> a -> [a] -> Buffer w t a
> makeCoincideBuffer _ _ as x bs =
>   Buffer $ TPL.makeCoincide
>     (map Cell as) (Cell x) (map Cell bs ++ [eof])
> 
> makePointMarkBuffer
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Proxy w -> Proxy t
>   -> [a] -> a -> [a] -> a -> [a] -> Buffer w t a
> makePointMarkBuffer _ _ as x bs y cs =
>   Buffer $ TPL.makePointMark
>     (map Cell as) (Cell x) (map Cell bs)
>     (Cell y) (map Cell cs ++ [eof])
> 
> makeMarkPointBuffer
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Proxy w -> Proxy t
>   -> [a] -> a -> [a] -> a -> [a] -> Buffer w t a
> makeMarkPointBuffer _ _ as x bs y cs =
>   Buffer $ TPL.makeMarkPoint
>     (map Cell as) (Cell x) (map Cell bs)
>     (Cell y) (map Cell cs ++ [eof])



Point Manipulation
------------------

The point and mark of a buffer represent the read head, and moving them around is one of the most important and common tasks our code needs to handle. We inherit a decent API for this from two-pointed lists which we can basically re-expose. First some queries on the point and mark:

> isPointAtStart
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Bool
> isPointAtStart =
>   TPL.isPointAtStart . unBuffer
> 
> isPointAtEnd
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Bool
> isPointAtEnd =
>   TPL.isPointAtEnd . unBuffer
> 
> hasMark
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Bool
> hasMark =
>   TPL.hasMark . unBuffer
> 
> isMarkAtStart
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Bool
> isMarkAtStart =
>   TPL.isMarkAtStart . unBuffer
> 
> isMarkAtEnd
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Bool
> isMarkAtEnd =
>   TPL.isMarkAtEnd . unBuffer

Next some basic operators for moving either the point or the mark to one of the ends of the buffer.

> movePointToStart
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Buffer w t a
> movePointToStart (Buffer w) =
>   Buffer $ TPL.movePointToStart w
> 
> movePointToEnd
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Buffer w t a
> movePointToEnd (Buffer w) =
>   Buffer $ TPL.movePointToEnd w
> 
> moveMarkToStart
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Buffer w t a
> moveMarkToStart (Buffer w) =
>   Buffer $ TPL.moveMarkToStart w
> 
> moveMarkToEnd
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Buffer w t a
> moveMarkToEnd (Buffer w) =
>   Buffer $ TPL.moveMarkToEnd w

We also have operators for moving the point and mark one cell to the left or right.

> movePointLeft
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Buffer w t a
> movePointLeft (Buffer w) =
>   Buffer $ TPL.movePointLeft w
> 
> movePointRight
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Buffer w t a
> movePointRight (Buffer w) =
>   Buffer $ TPL.movePointRight w
> 
> moveMarkLeft
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Buffer w t a
> moveMarkLeft (Buffer w) =
>   Buffer $ TPL.moveMarkLeft w
> 
> moveMarkRight
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Buffer w t a
> moveMarkRight (Buffer w) =
>   Buffer $ TPL.moveMarkRight w

We also have utilities for setting and clearing the mark. Recall that both `clearMark` and `leaveMark` leave the point unchanged; `clearMark` removes the mark, and `leaveMark` sets or resets it to coincide with the point.

> clearMark
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Buffer w t a
> clearMark (Buffer w) =
>   Buffer $ TPL.clearMark w
> 
> leaveMark
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Buffer w t a
> leaveMark =
>   Buffer . TPL.leaveMark . unBuffer



Basic Mutation
--------------

Next we need buffer versions of the region operators. We can read the character at the point:

> readPoint
>   :: Buffer w t a -> Maybe (Cell a)
> readPoint (Buffer w) =
>   TPL.readPoint w

And we need left-biased insert and delete at the point.

> insertPointLeft
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => a -> Buffer w t a -> Buffer w t a
> insertPointLeft a (Buffer w) =
>   Buffer $ TPL.insertPointLeft (Cell a) w
> 
> deletePointLeft
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Buffer w t a
> deletePointLeft (Buffer w) =
>   Buffer $ TPL.deletePointLeft w

As well we have versions of `copy`, `cut`, and `insert`.

> copyRegion
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Maybe (Buffer w t a)
> copyRegion (Buffer w) =
>   fmap (fromFingerTree . FT.inflateWith listCell)
>     $ TPL.copyRegionL w
> 
> cutRegion
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Maybe (Buffer w t a, Buffer w t a)
> cutRegion (Buffer w) = do
>   (region, rest) <- TPL.cutRegionL w
>   return
>     ( fromFingerTree $ FT.inflateWith listCell region
>     , Buffer rest )
> 
> insertRegion
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Buffer w t a -> Buffer w t a
> insertRegion (Buffer snippet) (Buffer w) =
>   Buffer $ TPL.insertRegionL (TPL.integrate snippet) w
> 
> alterRegion
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => (a -> a) -> Buffer w t a -> Buffer w t a
> alterRegion f =
>   Buffer . TPL.alterRegionL g . unBuffer
>   where
>     g :: Cell a -> Cell a
>     g x = case x of
>       Cell a -> Cell (f a)
>       _ -> x

We can also efficiently manipulate the start and end of the buffer.

> insertAtStart
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => a -> Buffer w t a -> Buffer w t a
> insertAtStart a =
>   Buffer . TPL.insertAtStart (Cell a) . unBuffer
> 
> viewAtStart
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Maybe (Cell a, Buffer w t a)
> viewAtStart (Buffer w) =
>   case TPL.viewAtEnd w of
>     Nothing -> error "viewAtStart: panic (empty buffer)"
>     Just (u, us) -> do
>       (a, as) <- TPL.viewAtStart us
>       return (a, Buffer $ TPL.insertAtEnd u as)
> 
> deleteAtStart
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Buffer w t a
> deleteAtStart (Buffer w) =
>   case TPL.viewAtEnd w of
>     Nothing -> error "deleteAtStart: panic (empty buffer)"
>     Just (u, us) ->
>       Buffer $ TPL.insertAtEnd u $ TPL.deleteAtStart us
> 
> insertAtEnd
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => a -> Buffer w t a -> Buffer w t a
> insertAtEnd a (Buffer w) =
>   case TPL.viewAtEnd w of
>     Nothing -> error "insertAtEnd: panic (empty buffer)"
>     Just (u, us) -> Buffer $ TPL.insertAtEnd u $
>       TPL.insertAtEnd (Cell a) us
> 
> viewAtEnd
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Maybe (Cell a, Buffer w t a)
> viewAtEnd (Buffer w) =
>   case TPL.viewAtEnd w of
>     Nothing -> error "viewAtEnd: panic (empty buffer)"
>     Just (u, us) -> do
>       (a, as) <- TPL.viewAtEnd us
>       return (a, Buffer $ TPL.insertAtEnd u as)
> 
> deleteAtEnd
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Buffer w t a
> deleteAtEnd (Buffer w) =
>   case TPL.viewAtEnd w of
>     Nothing -> error "deleteAtEnd: panic (empty buffer)"
>     Just (u, us) ->
>       Buffer $ TPL.insertAtEnd u $ TPL.deleteAtEnd us

We can also map over the entries in a buffer.

> mapBuffer
>   :: ( IsWidth w, IsTab t
>      , Valued (MeasureText w t) a1
>      , Valued (MeasureText w t) a2 )
>   => (a1 -> a2) -> Buffer w t a1 -> Buffer w t a2
> mapBuffer f =
>   Buffer . TPL.fmapList (fmap f) . unBuffer
> 
> mapRegion
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => (a -> a) -> Buffer w t a -> Buffer w t a
> mapRegion f =
>   Buffer . TPL.alterRegionL (fmap f) . unBuffer



Queries
-------

More than any of the data structures we've seen so far, buffers have _properties_. We now define an interface through which other modules can query these properties; for consistency's sake these all start with `get`. First some functions for reifying the type level parameters: width and tab stop.

> getBufferWidth
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Int
> getBufferWidth _ = toWidth (Proxy :: Proxy w)
> 
> getBufferTabStop
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Int
> getBufferTabStop _ = toTab (Proxy :: Proxy t)

Next we have queries on the measure of the entire buffer.

> getBufferCharCount
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Int
> getBufferCharCount (Buffer buf) =
>   let m = value buf :: MeasureText w t in
>   charCount m
> 
> getBufferByteCount
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Int
> getBufferByteCount (Buffer buf) =
>   let m = value buf :: MeasureText w t in
>   byteCount m
> 
> getBufferLineCol
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> LineCol
> getBufferLineCol buf =
>   let m = value buf :: MeasureText w t in
>   logicalCoords m
> 
> getBufferScreenCoords
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> (Int, Int)
> getBufferScreenCoords (Buffer buf) =
>   let m = value buf :: MeasureText w t in
>   applyScreenOffset (screenCoords m) (0,0)

Likewise we can query the value at the point.

> getPointCharCount
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Int
> getPointCharCount (Buffer w) =
>   case TPL.valueUpToPoint w of
>     Nothing -> 0
>     Just m -> charCount m
> 
> getPointByteCount
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Int
> getPointByteCount (Buffer w) =
>   case TPL.valueUpToPoint w of
>     Nothing -> 0
>     Just m -> byteCount m
> 
> getPointLineCol
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> LineCol
> getPointLineCol (Buffer w) =
>   case TPL.valueUpToPoint w of
>     Nothing -> mempty
>     Just m -> logicalCoords m
>  
> getPointScreenCoords
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> (Int, Int)
> getPointScreenCoords (Buffer w) =
>   case TPL.valueUpToPoint w of
>     Nothing -> (0,0)
>     Just m -> applyScreenOffset (screenCoords m) (0,0)

And for good measure, at the mark as well.

> getMarkCharCount
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Int
> getMarkCharCount (Buffer w) =
>   case TPL.valueUpToMark w of
>     Nothing -> 0
>     Just m -> charCount m
> 
> getMarkByteCount
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Int
> getMarkByteCount (Buffer w) =
>   case TPL.valueUpToMark w of
>     Nothing -> 0
>     Just m -> byteCount m
> 
> getMarkLineCol
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> LineCol
> getMarkLineCol (Buffer w) =
>   case TPL.valueUpToMark w of
>     Nothing -> mempty
>     Just m -> logicalCoords m
>  
> getMarkScreenCoords
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> (Int, Int)
> getMarkScreenCoords (Buffer w) =
>   case TPL.valueUpToMark w of
>     Nothing -> (0,0)
>     Just m -> applyScreenOffset (screenCoords m) (0,0)



Splitting
---------

Recall that finger trees, the structure underlying our buffers, admit an efficient _splitting_ operation, which we can take advantage of to move the point and mark to specific locations in the buffer. First we define a utility function, not exposed outside this module, that attempts to split a buffer on an arbitrary predicate.

> splitPoint
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => (MeasureText w t -> Bool)
>   -> Buffer w t a -> Maybe (Buffer w t a)
> splitPoint pointP (Buffer w) =
>   Buffer <$> TPL.splitPoint pointP w

For ergonomics' sake we'll expose specialized splitting functions. First at a given line and column position:

> movePointToLineCol
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a, Eq a )
>   => LineCol -> Buffer w t a -> Buffer w t a
> movePointToLineCol lc buf =
>   case splitPoint (atOrAfterLineCol lc) buf of
>     Nothing -> movePointToEnd buf
>     Just xs -> xs
> 
> atOrAfterLineCol
>   :: LineCol -> MeasureText w t -> Bool
> atOrAfterLineCol lc m =
>   lc < (logicalCoords m) <> (logicalOffset m)

We should also test some simple cases to make sure we understand how this splitting works.

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2
> --     "abc\nd" 'e' "f\nghi"
> --   y = makePointOnlyBuffer nat8 nat2
> --     "" 'a' "bc\ndef\nghi"
> -- in y == movePointToLineCol (LineCol 0 0) x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2
> --     "abc\nd" 'e' "f\nghi"
> --   y = makePointOnlyBuffer nat8 nat2
> --     "abc\ndef\ng" 'h' "i"
> -- in y == movePointToLineCol (LineCol 2 1) x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2
> --     "abc\nd" 'e' "f\nghi"
> --   y = makePointOnlyBuffer nat8 nat2
> --     "abc" '\n' "def\nghi"
> -- in y == movePointToLineCol (LineCol 0 4) x
> -- :}
> -- True

:::

Next we'd like to split at a given pair of screen coordinates. It's straightforward how this should work if we try to split at a pair of coordinates corresponding to a character in the buffer (split there). It's also (slightly less but still fairly) straightforward how to handle coordinates that fall off the end of the buffer; we'll just clamp them to the EOF sigil. But there's a universe of pairs whose $y$ coordinates correspond to _lines_ on the screen, but whose $x$ coordinates march off the right edge. What should these do?

One option is to just fail. But I think a better option is to try our best to return a reasonable result: in this case, to move the point to the last position on the screen line. In some sense that is "as close as we can get" in this case.

> movePointToScreenCoords
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a, Eq a )
>   => (Int, Int) -> Buffer w t a -> Buffer w t a
> movePointToScreenCoords pos buf =
>   case splitPoint (atOrAfterScreenCoords pos) buf of
>     Nothing -> movePointToEnd buf
>     Just xs -> xs
> 
> atOrAfterScreenCoords
>   :: forall w t
>    . ( IsWidth w, IsTab t )
>   => (Int, Int) -> MeasureText w t -> Bool
> atOrAfterScreenCoords (u,v) m =
>   let
>     (h,k) = applyScreenOffset (screenCoords m <> screenOffset m) (0,0)
>   in (v < k) || ((v == k) && (u < h))

We can test our understanding with some examples.

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2
> --     "abc\nde" 'f' "\nghi"
> --   y = makePointOnlyBuffer nat8 nat2
> --     "" 'a' "bc\ndef\nghi"
> -- in y == movePointToScreenCoords (0,0) x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2
> --     "abc\nde" 'f' "\nghi"
> --   y = makePointOnlyBuffer nat8 nat2
> --     "a" 'b' "c\ndef\nghi"
> -- in y == movePointToScreenCoords (1,0) x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2
> --     "abc\ndefg" 'h' "i\njkl"
> --   y = makePointOnlyBuffer nat8 nat2
> --     "abc\nde" 'f' "ghi\njkl"
> -- in y == movePointToScreenCoords (2,1) x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2
> --     "ab" 'c' "defgh"
> --   y = makePointOnlyBuffer nat8 nat2
> --     "abcdefg" 'h' ""
> -- in y == movePointToScreenCoords (7,0) x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2
> --     "abc\nde\nf" 'g' "h"
> --   y = makePointOnlyBuffer nat8 nat2
> --     "abc\nde" '\n' "fgh"
> -- in y == movePointToScreenCoords (5,1) x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2
> --     "abcdefghijklmn" 'o' "pqrst"
> --   y = makePointOnlyBuffer nat8 nat2
> --     "abcdefg" 'h' "ijklmnopqrst"
> -- in y == movePointToScreenCoords (10,0) x
> -- :}
> -- True

:::

Next we define a utility which moves the point to the first character of a given screen line -- which is crucial for rendering buffers to the screen. This is a little trickier than moving to a specific line and character or pair of screen coordinates, because it involves a _relative_ position and nailing down exactly what it means to say that a character is at the start of a new screen line.

Suppose the final screen coordinate in the buffer is $(W,H)$ and we want to move to the first position of screen line $y$. If $y$ is in the interval $[0,H]$, then some character has screen coordinate $(0,y)$ and we can find it with an ordinary split. Similarly simple, if $y$ is at least $H+2$ then no such character exists, and the EOF sigil can't help us. The delicate case is when we want to move to the first position of screen line $H+1$. This case depends on (1) the final character in the buffer and (2) the width of the final screen line in the buffer (that is, $W$). If the final character is a newline, we'd like to consider the EOF sigil to be the start of the next screen line. If the final character is not a newline, then we consider the EOF sigil to be on the last line (not the start of the next line) _unless_ the final character's effective width plus $W$ exceeds the screen width.

That's a mouthful!

> movePointToScreenLine
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a, Eq a, IsChar a )
>   => Int -> Buffer w t a -> Maybe (Buffer w t a)
> movePointToScreenLine k buf =
>   let (w,h) = getBufferScreenCoords buf
>   in if k > 1 + h
>     then Nothing
>     else if k <= 0
>       then Just $ movePointToStart buf
>       else do
>         z <- splitPoint (atOrAfterScreenLine k) buf
>         if k < 1 + h
>           then return z
>           else case fmap (fmap toChar) (readPoint buf) of
>             Nothing -> error "movePointToScreenLine: panic (expected eof)"
>             Just (Cell '\n') -> Just $ movePointRight z
>             Just (Cell c) ->
>               let
>                 offset :: ScreenOffset w t
>                 offset = screenOffset $ value c
>                 (_,h1) = applyScreenOffset offset (w,h)
>               in if (h1 == 1 + h)
>                 then Just $ movePointRight z
>                 else Nothing
> 
> atOrAfterScreenLine
>   :: forall w t
>    . ( IsWidth w, IsTab t )
>   => Int -> MeasureText w t -> Bool
> atOrAfterScreenLine k m =
>   let
>     offset = screenCoords m
>     (_,v) = applyScreenOffset offset (0,0)
>   in (v >= k) || (hasEOF m)

And some example cases:

::: doctest

> -- $
> -- Split at line 0 < h
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2
> --     "abc\nde" 'f' "\nghi"
> --   y = makePointOnlyBuffer nat8 nat2
> --     "" 'a' "bc\ndef\nghi"
> -- in Just y == movePointToScreenLine 0 x
> -- :}
> -- True
> --
> -- Split at line 1 < h
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2
> --     "abc\nde\nf" 'g' "h"
> --   y = makePointOnlyBuffer nat8 nat2
> --     "abc\n" 'd' "e\nfgh"
> -- in Just y == movePointToScreenLine 1 x
> -- :}
> -- True
> --
> -- Split at line 2 < h
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2
> --     "abc\n" 'd' "e\nfgh"
> --   y = makePointOnlyBuffer nat8 nat2
> --     "abc\nde\n" 'f' "gh"
> -- in Just y == movePointToScreenLine 2 x
> -- :}
> -- True
> --
> -- Split at line h+1 (last cell is an interior char)
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2
> --     "" 'a' ""
> -- in Nothing == movePointToScreenLine 1 x
> -- :}
> -- True
> --
> -- Split at line h+1 (last cell is a newline)
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2
> --     "" '\n' ""
> --   y = movePointToEnd x
> -- in Just y == movePointToScreenLine 1 x
> -- :}
> -- True
> --
> -- Split at line h+1 (last cell is an end char)
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2
> --     "" 'a' "aaaaaaa"
> --   y = movePointToEnd x
> -- in Just y == movePointToScreenLine 1 x
> -- :}
> -- True
> --
> -- Split at line h+1 (last cell is an end tab)
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2
> --     "" '\t' "\t\t\t"
> --   y = movePointToEnd x
> -- in Just y == movePointToScreenLine 1 x
> -- :}
> -- True
> --
> -- Split at line 1 == h (last cell is a newline)
> -- >>> :{
> -- let
> --   x = makePointOnlyBuffer nat8 nat2
> --     "" '\n' "\n"
> --   y = makePointOnlyBuffer nat8 nat2
> --     "\n" '\n' ""
> -- in Just y == movePointToScreenLine 1 x
> -- :}
> -- True

:::












> instance
>   ( Show a, IsWidth w, IsTab t, IsChar a
>   ) => Show (Buffer w t a)
>   where
>     show (Buffer w) =
>       let
>         wid = showWidth (Proxy :: Proxy w)
>         tab = showTab (Proxy :: Proxy t)
>       in case w of
>         TPL.Vacant -> "rawVacantBuffer"
>         TPL.PointOnly (as, x, bs) -> concat
>           [ "rawPointOnly "
>           , wid, " ", tab, " "
>           , show $ Fold.toList as, " "
>           , show x, " "
>           , show $ Fold.toList bs
>           ]
>         TPL.Coincide (as, x, bs) -> concat
>           [ "rawCoincide "
>           , wid, " ", tab, " "
>           , show $ Fold.toList as, " "
>           , show x, " "
>           , show $ Fold.toList bs
>           ]
>         TPL.PointMark (as, x, bs, y, cs) -> concat
>           [ "rawPointMark "
>           , wid, " ", tab, " "
>           , show $ Fold.toList as, " "
>           , show x, " "
>           , show $ Fold.toList bs, " "
>           , show y, " "
>           , show $ Fold.toList cs
>           ]
>         TPL.MarkPoint (as, x, bs, y, cs) -> concat
>           [ "rawMarkPoint "
>           , wid, " ", tab, " "
>           , show $ Fold.toList as, " "
>           , show x, " "
>           , show $ Fold.toList bs, " "
>           , show y, " "
>           , show $ Fold.toList cs
>           ]






It will also be handy to be able to resize the buffer in two different ways, depending on whether we want to specify the new parameters statically or dynamically.

> resizeBuffer
>   :: ( IsWidth w1, IsTab t1, Valued (MeasureText w1 t1) a
>      , IsWidth w2, IsTab t2, Valued (MeasureText w2 t2) a )
>   => Buffer w1 t1 a -> Buffer w2 t2 a
> resizeBuffer (Buffer x) = Buffer (TPL.remeasure x)
> 
> adjustBuffer
>   :: ( IsWidth w1, IsTab t1, Valued (MeasureText w1 t1) a
>      , IsWidth w2, IsTab t2, Valued (MeasureText w2 t2) a )
>   => Proxy w2 -> Proxy t2
>   -> Buffer w1 t1 a -> Buffer w2 t2 a
> adjustBuffer _ _ = resizeBuffer











> -- Find coordinates "nearest" to a given pair that are
> -- present in the buffer.
> seekScreenCoords
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a, Eq a )
>   => (Int, Int) -> Buffer w t a -> (Int, Int)
> seekScreenCoords z =
>   getPointScreenCoords . movePointToScreenCoords z


> data BufferRenderSettings
>   = BufferRenderSettings
>   deriving (Eq, Show)

> defaultBufferRenderSettings
>   :: BufferRenderSettings
> defaultBufferRenderSettings =
>   BufferRenderSettings






Rendering
=========

> takeLine
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a, Eq a, IsChar a )
>   => FT.FingerTree (MeasureText w t) (Cell a)
>   -> Maybe
>       ( FT.FingerTree (MeasureText w t) (Cell a)
>       , FT.FingerTree (MeasureText w t) (Cell a) )
> takeLine xs =
>   if FT.isEmpty xs
>     then Nothing
>     else case FT.split (atOrAfterScreenLine 1) xs of
>       Nothing -> Just (xs, mempty)
>       Just (as, x, bs) ->
>         if FT.isEmpty bs
>           then case FT.unsnoc as of
>             Nothing -> Just (FT.snoc x as, bs)
>             Just (EOF, _) -> error "panic (3)" 
>             Just (Cell u,us) -> case toChar u of
>               '\n' -> Just (as, FT.cons x bs)
>               _    ->
>                 let
>                   m = value as :: MeasureText w t
>                   (_,y) = applyScreenOffset (screenOffset m) (0,0)
>                 in if y == 0
>                   then Just (as, FT.cons x bs)
>                   else Just (FT.snoc x as, bs)
>           else Just (as, FT.cons x bs)
> 
> takeLines
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a, Eq a, IsChar a )
>   => Int
>   -> FT.FingerTree (MeasureText w t) (Cell a)
>   -> ( [ FT.FingerTree (MeasureText w t) (Cell a) ]
>      , FT.FingerTree (MeasureText w t) (Cell a) )
> takeLines n xs = f 0 [] xs
>   where
>     f :: Int
>       -> [ FT.FingerTree (MeasureText w t) (Cell a) ]
>       -> FT.FingerTree (MeasureText w t) (Cell a)
>       -> ( [ FT.FingerTree (MeasureText w t) (Cell a) ]
>          , FT.FingerTree (MeasureText w t) (Cell a) )
>     f k us vs =
>       if (k >= n) || FT.isEmpty vs
>         then (reverse us, vs)
>         else if FT.isSingleton vs
>           then (reverse (vs : us), mempty)
>           else case takeLine vs of
>             Nothing ->
>               (reverse (vs : us), mempty)
>             Just (as, bs) ->
>               f (k+1) (as:us) bs
> 
> splitLines
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a, Eq a, IsChar a )
>   => Int -- top screen line
>   -> Int -- view height
>   -> Buffer w t a
>   -> ( FT.FingerTree (MeasureText w t) (Cell a)
>      , [ FT.FingerTree (MeasureText w t) (Cell a) ]
>      , FT.FingerTree (MeasureText w t) (Cell a) )
> splitLines t h buf =
>   case movePointToScreenLine t buf of
>     Nothing ->
>       let Buffer w = buf
>       in (TPL.integrate w, [], mempty)
>     Just z ->
>       let Buffer w = z
>       in case w of
>         TPL.Vacant ->
>           (mempty, [], mempty)
>         TPL.PointOnly (as, x, bs) ->
>           let (us, vs) = takeLines h (FT.cons x bs)
>           in (as, us, vs)
>         TPL.Coincide (as, x, bs) ->
>           let (us, vs) = takeLines h (FT.cons x bs)
>           in (as, us, vs)
>         TPL.PointMark (as, x, bs, y, cs) ->
>           let (us, vs) = takeLines h (FT.cons x bs <> FT.cons y cs)
>           in (as, us, vs)
>         TPL.MarkPoint (as, x, bs, y, cs) ->
>           let (us, vs) = takeLines h (FT.cons y cs)
>           in (as <> FT.cons x bs, us, vs)

> getLineNumbers
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a, Eq a, IsChar a )
>   => MeasureText w t
>   -> [ FT.FingerTree (MeasureText w t) (Cell a) ]
>   -> [ ( FT.FingerTree (MeasureText w t) (Cell a), Maybe Int ) ]
> getLineNumbers m xs = f m xs []
>   where
>     f :: MeasureText w t
>       -> [ FT.FingerTree (MeasureText w t) (Cell a) ]
>       -> [ ( FT.FingerTree (MeasureText w t) (Cell a), Maybe Int ) ]
>       -> [ ( FT.FingerTree (MeasureText w t) (Cell a), Maybe Int ) ]
>     f i as bs = case as of
>       [] -> reverse bs
>       us:uss ->
>         let
>           k = case FT.uncons us of
>             Nothing -> Nothing
>             Just (c, _) ->
>               let
>                 j' = i <> value c
>                 LineCol lj cj = logicalCoords j'
>               in
>                 if cj == 0
>                   then Just lj
>                   else Nothing
> 
>           j = i <> value us
> 
>         in f j uss ((us, k) : bs)

> renderBuffer
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , IsChar a, Eq a )
>   => BufferRenderSettings
>   -> (a -> a)
>   -> Int -- top screen line
>   -> Int -- view height
>   -> Buffer w t a
>   -> ([Maybe Int], [[(a, Int)]])
> renderBuffer _ f t h buf =
>   let (as, xs, _) = splitLines t h $ alterRegion f buf
>   in
>     unzip $
>       map (\(z,i) -> (i, map (\(Cell a,m) -> (a, fst $ applyScreenOffset (screenCoords m) (0,0))) $ filter (\(x, _) -> case x of EOF -> False; _ -> True) $ FT.toAnnotatedList z)) $
>       getLineNumbers (value as) xs









Testing and Debugging
---------------------

As usual, we wrap up this module with some helper code for writing tests. First we need class instances for working with our property testing library.

> instance
>   ( IsWidth w, IsTab t, IsChar a, Eq a
>   , Arb a, Valued (MeasureText w t) a
>   ) => Arb (Buffer w t a)
>   where
>     arb = do
>       xs <- arb
>       return $ Buffer (TPL.insertAtEnd (eof :: Cell a) xs)
> 
> instance
>   ( IsWidth w, IsTab t, IsChar a, Eq a
>   , Prune a, Valued (MeasureText w t) a
>   ) => Prune (Buffer w t a)
>   where
>     prune (Buffer x) =
>       case TPL.viewAtEnd x of
>         Nothing -> error "prune: panic (expected eof)"
>         Just (c, cs) ->
>           [Buffer (TPL.insertAtEnd c z) | z <- prune cs]

Next, recall that buffers need to satisfy some invariants: first of all they are built on finger trees, which have invariants of their own, but moreover the buffer must end with an EOF sigil. We expose a helper to check for this.

> validate
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a, Eq a )
>   => Buffer w t a -> Bool
> validate (Buffer w) =
>   case TPL.viewAtEnd w of
>     Nothing ->
>       False
>     Just (u, _) -> 
>       (u == eof) && (TPL.validate w)

And lastly we expose a function that converts a buffer into a list with all the gritty value details exposed.

> toAnnotatedList
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> [(Cell a, MeasureText w t)]
> toAnnotatedList = TPL.toAnnotatedList . unBuffer
