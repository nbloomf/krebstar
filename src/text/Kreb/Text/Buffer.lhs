---
title: Kreb.Text.Buffer
---



Contents
--------

* [Introduction](#introduction)
* [Exposed API](#exposed-api)
* [Cell](#cell)
* [Buffers](#buffers)
    * [Constructors and destructors](#constructors-and-destructors)
    * [Queries](#queries)
    * [Splitting](#splitting)
    * [Rendering](#rendering)
* [Sized buffers](#sized-buffers)



Introduction
============

> {-# LANGUAGE
>     ExistentialQuantification
>   , MultiParamTypeClasses
>   , QuantifiedConstraints
>   , UndecidableInstances
>   , ScopedTypeVariables
>   , FlexibleInstances
>   , FlexibleContexts
>   , InstanceSigs
>   , Rank2Types
> #-}



Exposed API
===========

> module Kreb.Text.Buffer (
>   -- * Buffers
>     Buffer(..)
>   , Cell()
>   , eof
>   , cell
>   , unCells
> 
>   -- * Constructors
>   , empty
>   , makeListBuffer

>   , defBuffer
> 
>   , charBuffer
>   , charBufferFocus

>   , clearMark
>   , readPoint
>   , movePointLeft
>   , movePointRight
>   , movePointToStart
>   , insertPointLeftBuffer
>   , deletePointLeftBuffer
> 
>   -- * Queries
>   , isEmpty
>   , isPointAtEnd
> 
>   , getBufferWidth
>   , getBufferTabStop
>   , getBufferBytes
>   , getBufferLineCol
>   , getBufferScreenCoords
>   , getBufferString
> 
>   , seekScreenCoords
> 
>   , getBufferHeadLineCol
>   , getBufferHeadScreenCoords
> 
>   -- * Navigation
>   , splitBufferAtLineCol
>   , splitBufferAtScreenCoords
>   , splitBufferAtScreenLine
> 
>   , atScreenLine
>   , atLineCol
>   , atScreenCoords
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
>   -- * Sized Buffers
>   , SizedBuffer(..)
>   , emptySizedBuffer
>   , makeSizedBuffer
>   , shapeSizedBuffer
>   , alterSizedBuffer
>   , alterSizedBufferF
>   , querySizedBuffer
> 
>   -- * Debugging
>   , makePointOnlyBuffer
>   , makeCoincideBuffer
>   , makePointMarkBuffer
>   , makeMarkPointBuffer
> 
>   , rawVacant
>   , rawPointOnly
>   , rawCoincide
>   , rawPointMark
>   , rawMarkPoint
> 
>   , toListDebugBuffer
>   , showInternalBuffer
>   , validateBuffer
>   , mkFT
> ) where
> 
> import Data.Proxy
> import Data.Foldable
> import Data.List (groupBy)
> import qualified Data.Map.Strict as M
> import Control.Monad (join)
> import Debug.Trace
> 
> import Kreb.Check
> import Kreb.Reflect
> import Kreb.Struct.FingerTree
> import qualified Kreb.Struct.TwoPointedList as TPL
> 
> import Kreb.Text.ScreenOffset
> import Kreb.Text.MeasureText
> import Kreb.Text.Glyph
> import Kreb.Text.Cell








Buffers
=======

A _buffer_ is just a zipped finger tree with value monoid `MeasureText w t` for some width paramter `w` and tab stop parameter `t`.

> newtype Buffer w t a = Buffer
>   { unBuffer :: TPL.TwoPointedList (MeasureText w t) (Cell a)
>   } deriving Eq
> 
> instance
>   ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>   ) => Valued (MeasureText w t) (Buffer w t a)
>   where
>     value = value . TPL.integrate . unBuffer
> 
> instance
>   ( IsWidth w, IsTab t, IsChar a, Eq a
>   , Arb a, Valued (MeasureText w t) a
>   ) => Arb (Buffer w t a)
>   where
>     arb = Buffer <$> arb
> 
> instance
>   ( IsWidth w, IsTab t, IsChar a, Eq a
>   , Prune a, Valued (MeasureText w t) a
>   ) => Prune (Buffer w t a)
>   where
>     prune (Buffer x) = Buffer <$> prune x

> empty
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a
> empty = Buffer $
>   TPL.singleton (eof :: Cell a)





> isPointAtEnd
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Bool
> isPointAtEnd (Buffer w) =
>   TPL.isPointAtEnd w

> movePointLeft
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Buffer w t a
> movePointLeft (Buffer w) =
>   Buffer $ TPL.movePointLeft w

> movePointRight
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Buffer w t a
> movePointRight (Buffer w) =
>   Buffer $ TPL.movePointRight w

> movePointToStart
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Buffer w t a
> movePointToStart (Buffer w) =
>   Buffer $ TPL.movePointToStart w

> movePointToEnd
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Buffer w t a
> movePointToEnd (Buffer w) =
>   Buffer $ TPL.movePointToEnd w

> clearMark
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Buffer w t a
> clearMark (Buffer w) =
>   Buffer $ TPL.clearMark w

> readPoint
>   :: Buffer w t a -> Maybe (Cell a)
> readPoint (Buffer w) =
>   TPL.readPoint w

> insertPointLeftBuffer
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => a -> Buffer w t a -> Buffer w t a
> insertPointLeftBuffer a (Buffer w) =
>   Buffer $ TPL.insertPointLeft (Cell a) w

> deletePointLeftBuffer
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Buffer w t a
> deletePointLeftBuffer (Buffer w) =
>   Buffer $ TPL.deletePointLeft w

> isEmpty
>   :: forall w t a
>    . ( Eq a, IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Bool
> isEmpty (Buffer w) = TPL.isEmpty w



Constructors and destructors
----------------------------

Our buffer constructors come in two flavors: constructors prefixed with `def` take the type parameters as inputs, while those prefixed with `mk` do not.



> makeListBuffer
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => [a] -> Buffer w t a
> makeListBuffer xs = Buffer $ TPL.makeFromList $ concat
>   [ map Cell xs, [ eof :: Cell a ] ]

Constructing buffers with a specific structure for testing.

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

> rawVacant
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Proxy w -> Proxy t
>   -> Buffer w t a
> rawVacant _ _ =
>   Buffer TPL.Vacant
> 
> rawPointOnly
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Proxy w -> Proxy t
>   -> [Cell a] -> Cell a -> [Cell a]
>   -> Buffer w t a
> rawPointOnly _ _ as x bs =
>   Buffer $ TPL.makePointOnly as x bs
> 
> rawCoincide
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Proxy w -> Proxy t
>   -> [Cell a] -> Cell a -> [Cell a]
>   -> Buffer w t a
> rawCoincide _ _ as x bs =
>   Buffer $ TPL.makeCoincide as x bs
> 
> rawPointMark
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Proxy w -> Proxy t
>   -> [Cell a] -> Cell a -> [Cell a] -> Cell a -> [Cell a]
>   -> Buffer w t a
> rawPointMark _ _ as x bs y cs =
>   Buffer $ TPL.makePointMark as x bs y cs
> 
> rawMarkPoint
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Proxy w -> Proxy t
>   -> [Cell a] -> Cell a -> [Cell a] -> Cell a -> [Cell a]
>   -> Buffer w t a
> rawMarkPoint _ _ as x bs y cs =
>   Buffer $ TPL.makeMarkPoint as x bs y cs






The `def` variants will mostly be used for testing or in the shell.


> 
> defBuffer
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a, Eq a )
>   => Proxy w -> Proxy t -> [a] -> Buffer w t a
> defBuffer _ _ = makeListBuffer

> charBuffer
>   :: ( IsWidth w, IsTab t
>      , Valued (MeasureText w t) a, IsChar a, Eq a )
>   => Proxy w -> Proxy t -> [Char]
>   -> Buffer w t a
> charBuffer _ _ as = makeListBuffer (map fromChar as)

> charBufferFocus
>   :: ( IsWidth w, IsTab t
>      , Valued (MeasureText w t) a, IsChar a, Eq a )
>   => Proxy w -> Proxy t -> Proxy a -> [Char] -> Char -> [Char]
>   -> Buffer w t a
> charBufferFocus pw pt _ as x bs = makePointOnlyBuffer pw pt
>   (map fromChar as) (fromChar x) (map fromChar bs)

And we can give a handy `Show` instance:

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
>           , show $ toList as, " "
>           , show x, " "
>           , show $ toList bs
>           ]
>         TPL.Coincide (as, x, bs) -> concat
>           [ "rawCoincide "
>           , wid, " ", tab, " "
>           , show $ toList as, " "
>           , show x, " "
>           , show $ toList bs
>           ]
>         TPL.PointMark (as, x, bs, y, cs) -> concat
>           [ "rawPointMark "
>           , wid, " ", tab, " "
>           , show $ toList as, " "
>           , show x, " "
>           , show $ toList bs, " "
>           , show y, " "
>           , show $ toList cs
>           ]
>         TPL.MarkPoint (as, x, bs, y, cs) -> concat
>           [ "rawMarkPoint "
>           , wid, " ", tab, " "
>           , show $ toList as, " "
>           , show x, " "
>           , show $ toList bs, " "
>           , show y, " "
>           , show $ toList cs
>           ]






Queries
-------

Functions for getting the basic parameters of a buffer:

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
> 
> getBufferBytes
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Int
> getBufferBytes (Buffer buf) =
>   let m = value buf :: MeasureText w t in
>   byteCount m
> 
> getBufferScreenCoords
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> (Int, Int)
> getBufferScreenCoords (Buffer buf) =
>   let m = value buf :: MeasureText w t in
>   applyScreenOffset (screenCoords m) (0,0)
> 
> getBufferLineCol
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> LineCol
> getBufferLineCol (Buffer buf) =
>   let m = value buf :: MeasureText w t in
>   logicalOffset m
> 
> getBufferString
>   :: ( IsWidth w, IsTab t, Eq a, IsChar a, Valued (MeasureText w t) a )
>   => Buffer w t a -> String
> getBufferString (Buffer w) =
>   map toChar $ unCells $ TPL.toList w



> getBufferHeadPosition
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> MeasureText w t
> getBufferHeadPosition (Buffer w) =
>   case TPL.valueAtPoint w of
>     Nothing -> mempty
>     Just val -> val
> 
> getBufferHeadLineCol
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> LineCol
> getBufferHeadLineCol =
>   logicalCoords . getBufferHeadPosition
> 
> getBufferHeadScreenCoords
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> (Int, Int)
> getBufferHeadScreenCoords buf =
>   let offset = screenCoords $ getBufferHeadPosition buf in
>   applyScreenOffset offset (0,0)



> -- Find coordinates "nearest" to a given pair that are
> -- present in the buffer.
> seekScreenCoords
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a, Eq a )
>   => (Int, Int) -> Buffer w t a -> (Int, Int)
> seekScreenCoords z =
>   getBufferHeadScreenCoords . splitBufferAtScreenCoords z


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





Sized Buffers
=============

The `Buffer` type is parameterized by two type level natural numbers. This is necessary to make the monoid instance for `MeasureText` work out nicely (and statically), but will be cumbersome once we're juggling several text buffers at once. Consumers of this code need to know that a buffer has a well defined width and tab stop, but don't really want to care about the _types_ used to express this.

What we want is to be able to hide the `w` and `t` type parameters. We can do this with the following _existential type_, `SizedBuffer`.

> data SizedBuffer a = forall w t.
>   ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>   ) => SizedBuffer (Buffer w t a)

Note that the type variables `w` and `t` are bound on the right hand side of the definition, so they cannot "leak out". Code consuming `SizedBuffers` cannot know about or do anything with `w` and `t` beyond what is allowed by the `IsWidth` and `IsTab` constraints. This is really powerful! `Buffer` gets the benefits of very expressive types, and `SizedBuffer` acts like a guard giving the outside world the ability to ignore those types.

> instance
>   ( Eq a, IsChar a
>   , forall w t. (IsWidth w, IsTab t)
>       => Valued (MeasureText w t) a
>   ) => Eq (SizedBuffer a)
>   where
>     (SizedBuffer b1) == (SizedBuffer b2) = and
>       [ (getBufferWidth b1)
>           == (getBufferWidth b2)
>       , (getBufferTabStop b1)
>           == (getBufferTabStop b2)
>       , (adjustBuffer nat8 nat2 b1)
>           == (adjustBuffer nat8 nat2 b2)
>       ]
> 
> instance
>   ( Show a, IsChar a
>   ) => Show (SizedBuffer a)
>   where
>     show (SizedBuffer buf) = 
>       "SizedBuffer (" ++ show buf ++ ")"

> emptySizedBuffer
>   :: Int -> Int -> SizedBuffer Glyph
> emptySizedBuffer width tab =
>   withWidth width $ \(_ :: Proxy w) ->
>     withTab tab $ \(_ :: Proxy t) ->
>       SizedBuffer (empty :: Buffer w t Glyph)
> 
> makeSizedBuffer
>   :: Int -> Int -> String -> SizedBuffer Glyph
> makeSizedBuffer width tab str =
>   withWidth width $ \(_ :: Proxy w) ->
>     withTab tab $ \(_ :: Proxy t) ->
>       SizedBuffer (makeListBuffer (map fromChar str) :: Buffer w t Glyph)
> 
> shapeSizedBuffer
>   :: Int -> Int
>   -> SizedBuffer Glyph
>   -> SizedBuffer Glyph
> shapeSizedBuffer width tab (SizedBuffer buf) =
>   withWidth width $ \(_ :: Proxy w) ->
>     withTab tab $ \(_ :: Proxy t) ->
>       SizedBuffer (resizeBuffer buf :: Buffer w t Glyph)
> 
> alterSizedBuffer
>   :: (forall w t. (IsWidth w, IsTab t, Valued (MeasureText w t) a)
>       => Buffer w t a -> Buffer w t a)
>   -> SizedBuffer a
>   -> SizedBuffer a
> alterSizedBuffer f (SizedBuffer buf) = SizedBuffer (f buf)
> 
> alterSizedBufferF
>   :: ( Functor f )
>   => (forall w t. (IsWidth w, IsTab t, Valued (MeasureText w t) a)
>       => Buffer w t a -> f (Buffer w t a))
>   -> SizedBuffer a
>   -> f (SizedBuffer a)
> alterSizedBufferF f (SizedBuffer buf) = SizedBuffer <$> (f buf)
> 
> querySizedBuffer
>   :: (forall w t. (IsWidth w, IsTab t, Valued (MeasureText w t) a)
>       => Buffer w t a -> b)
>   -> SizedBuffer a
>   -> b
> querySizedBuffer f (SizedBuffer buf) = f buf





Splitting
=========

> splitBuffer
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => (MeasureText w t -> Bool)
>   -> Maybe (MeasureText w t -> Bool)
>   -> Buffer w t a -> Maybe (Buffer w t a)
> splitBuffer pointP q (Buffer w) =
>   Buffer <$> TPL.split pointP q w



> splitBufferAtLineCol
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a, Eq a )
>   => LineCol -> Buffer w t a -> Buffer w t a
> splitBufferAtLineCol lc buf =
>   case splitBuffer (atLineCol lc) Nothing buf of
>     Nothing -> clearMark $ movePointToEnd buf
>     Just xs -> xs
> 
> atLineCol
>   :: LineCol -> MeasureText w t -> Bool
> atLineCol lc m =
>   lc <= logicalCoords m



> splitBufferAtScreenCoords
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a, Eq a )
>   => (Int, Int) -> Buffer w t a -> Buffer w t a
> splitBufferAtScreenCoords pos buf =
>   case splitBuffer (atScreenCoords pos) Nothing buf of
>     Nothing -> clearMark $ movePointToEnd buf
>     Just xs ->
>       let
>         offset = screenCoords $ getBufferHeadPosition xs
>         pos' = applyScreenOffset offset (0,0)
>       in
>         if pos == pos'
>           then xs
>           else movePointLeft xs
> 
> atScreenCoords
>   :: forall w t
>    . ( IsWidth w, IsTab t )
>   => (Int, Int) -> MeasureText w t -> Bool
> atScreenCoords (u,v) m =
>   let
>     (h,k) = applyScreenOffset (screenCoords m) (0,0)
>   in (v < k) || ((v == k) && (u <= h))




> splitBufferAtScreenLine
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a, Eq a, IsChar a )
>   => Int -> Buffer w t a -> Maybe (Buffer w t a)
> splitBufferAtScreenLine k buf =
>   if k == 0
>     then Just $ clearMark $ movePointToStart buf
>     else splitBuffer (atScreenLine k) Nothing buf
> 
> atScreenLine
>   :: forall w t
>    . ( IsWidth w, IsTab t )
>   => Int -> MeasureText w t -> Bool
> atScreenLine k m =
>   let
>     offset = screenCoords m
>     (_,v) = applyScreenOffset offset (0,0)
>   in v >= k






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
>   => FingerTree (MeasureText w t) (Cell a)
>   -> Maybe
>       ( FingerTree (MeasureText w t) (Cell a)
>       , FingerTree (MeasureText w t) (Cell a) )
> takeLine xs =
>   if isEmptyFT xs
>     then Nothing
>     else case splitFT (atScreenLine 1) xs of
>       Nothing -> Just (xs, mempty)
>       Just (as, x, bs) ->
>         if isEmptyFT bs
>           then case unsnoc as of
>             Nothing -> Just (snoc x as, bs)
>             Just (EOF, _) -> error "panic (3)" 
>             Just (Cell u,us) -> case toChar u of
>               '\n' -> Just (as, cons x bs)
>               _    ->
>                 let
>                   m = value as :: MeasureText w t
>                   (_,y) = applyScreenOffset (screenOffset m) (0,0)
>                 in if y == 0
>                   then Just (as, cons x bs)
>                   else Just (snoc x as, bs)
>           else Just (as, cons x bs)
> 
> takeLines
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a, Eq a, IsChar a )
>   => Int
>   -> FingerTree (MeasureText w t) (Cell a)
>   -> ( [ FingerTree (MeasureText w t) (Cell a) ]
>      , FingerTree (MeasureText w t) (Cell a) )
> takeLines n xs = f 0 [] xs
>   where
>     f :: Int
>       -> [FingerTree (MeasureText w t) (Cell a)]
>       -> FingerTree (MeasureText w t) (Cell a)
>       -> ( [ FingerTree (MeasureText w t) (Cell a) ]
>          , FingerTree (MeasureText w t) (Cell a) )
>     f k us vs =
>       if (k >= n) || isEmptyFT vs
>         then (reverse us, vs)
>         else if isLeafFT vs
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
>   -> ( FingerTree (MeasureText w t) (Cell a)
>      , [ FingerTree (MeasureText w t) (Cell a) ]
>      , FingerTree (MeasureText w t) (Cell a) )
> splitLines t h buf =
>   case splitBufferAtScreenLine t buf of
>     Nothing ->
>       let Buffer w = buf
>       in (TPL.integrate w, [], mempty)
>     Just z ->
>       let Buffer w = z
>       in case w of
>         TPL.Vacant ->
>           (mempty, [], mempty)
>         TPL.PointOnly (as,x,bs) ->
>           let (us, vs) = takeLines h (cons x bs)
>           in (as, us, vs)

> getLineNumbers
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a, Eq a, IsChar a )
>   => MeasureText w t
>   -> [ FingerTree (MeasureText w t) (Cell a) ]
>   -> [ ( FingerTree (MeasureText w t) (Cell a), Maybe Int ) ]
> getLineNumbers m xs = f m xs []
>   where
>     f :: MeasureText w t
>       -> [ FingerTree (MeasureText w t) (Cell a) ]
>       -> [ ( FingerTree (MeasureText w t) (Cell a), Maybe Int ) ]
>       -> [ ( FingerTree (MeasureText w t) (Cell a), Maybe Int ) ]
>     f i as bs = case as of
>       [] -> reverse bs
>       us:uss ->
>         let
>           k = case uncons us of
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
>   -> Int -- top screen line
>   -> Int -- view height
>   -> Buffer w t a
>   -> ([Maybe Int], [[(a, Int)]])
> renderBuffer _ t h buf =
>   let (as, xs, _) = splitLines t h buf
>   in
>     unzip $
>       map (\(z,i) -> (i, map (\(Cell a,m) -> (a, fst $ applyScreenOffset (screenCoords m) (0,0))) $ filter (\(x, _) -> case x of EOF -> False; _ -> True) $ toListDebugFT z)) $
>       getLineNumbers (value as) xs



Testing and debugging
=====================

> mkFT
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a, Eq a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> [Cell Char] -> FingerTree (MeasureText w t) (Cell a)
> mkFT _ _ _ = fromListFT . map (fmap fromChar)

> toListDebugBuffer
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> [(Cell a, MeasureText w t)]
> toListDebugBuffer = TPL.toListDebug . unBuffer

> showInternalBuffer
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a, Show a, IsChar a )
>   => Buffer w t a -> String
> showInternalBuffer (Buffer xs) = unwords
>   [ "Buffer", TPL.showInternal xs ]

> validateBuffer
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Bool
> validateBuffer = TPL.validate . unBuffer
