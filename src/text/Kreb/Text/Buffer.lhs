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
>   , mkEmptyBuffer
>   , defEmptyBuffer
>   , mkBuffer
>   , defBuffer
>   , mkBufferFocus
>   , mkBufferFocus'
>   , mkBufferFocus''
>   , defBufferFocus
>   , defBufferFocus''
> 
>   , charBuffer
>   , charBufferFocus
> 
>   , unBufferFocus
> 
>   -- * Queries
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
>   , shapeSizedBuffer
>   , alterSizedBuffer
>   , alterSizedBufferF
>   , querySizedBuffer
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
> import Kreb.Reflect
> import Kreb.Struct
> 
> import Kreb.Text.ScreenOffset
> import Kreb.Text.MeasureText
> import Kreb.Text.Glyph





Cell
====

> data Cell a
>   = Cell a
>   | EOF
>   deriving Eq
> 
> instance Functor Cell where
>   fmap f x = case x of
>     Cell c -> Cell (f c)
>     EOF -> EOF
> 
> instance
>   ( IsChar a
>   ) => Show (Cell a)
>   where
>     show x = case x of
>       Cell c -> concat
>         [ "(fromChar ", show (toChar c), ")" ]
>       EOF -> "eof"
> 
> cell :: (IsChar a) => Char -> Cell a
> cell = Cell . fromChar
> 
> eof :: Cell a
> eof = EOF
> 
> unCell :: Cell a -> Maybe a
> unCell x = case x of
>   Cell c -> Just c
>   EOF -> Nothing
> 
> unCells :: [Cell a] -> [a]
> unCells xs = case xs of
>   [] -> []
>   c:cs -> case c of
>     Cell a -> a : unCells cs
>     EOF -> unCells cs
> 
> instance
>   ( IsChar a
>   ) => IsChar (Cell a)
>   where
>     toChar x = case x of
>       Cell c -> toChar c
>       EOF -> '@'
>     fromChar = Cell . fromChar
> 
> instance
>   ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>   ) => Valued (MeasureText w t) (Cell a)
>   where
>     value x = case x of
>       Cell a -> value a
>       EOF -> MeasureText
>         { charCount          = 0
>         , byteCount          = 0
>         , logicalOffset      = LineCol 0 1
>         , logicalCoords      = LineCol 0 0
>         , screenOffset       = mkNoNewlines [(1, Fixed1)]
>         , screenCoords       = mkNoNewlines []
>         , hasEOF             = True
>         , hasTrailingNewline = False
>         }





Buffers
=======

A _buffer_ is just a zipped finger tree with value monoid `MeasureText w t` for some width paramter `w` and tab stop parameter `t`.

> newtype Buffer w t a = Buffer
>   { unBuffer :: FingerTreeZip (MeasureText w t) (Cell a)
>   } deriving Eq
> 
> instance
>   ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>   ) => Valued (MeasureText w t) (Buffer w t a)
>   where
>     value = value . integrateFTZ . unBuffer

Immediately we can give `Buffer` a `Tape` instance:

> liftBuffer
>   :: (    FingerTreeZip (MeasureText w t) (Cell a)
>        -> FingerTreeZip (MeasureText w t) (Cell a)
>      )
>   -> Buffer w t a -> Buffer w t a
> liftBuffer f = Buffer . f . unBuffer
> 
> instance
>   ( IsWidth w, IsTab t, Valued (MeasureText w t) a, Eq a
>   ) => Tape (Buffer w t) a
>   where
>     isEmpty :: Buffer w t a -> Bool
>     isEmpty = isEmptyFTZ . unBuffer
> 
>     mkTape :: [a] -> Buffer w t a
>     mkTape as =
>       Buffer $ mkTapeFTZ $ (map Cell as) ++ [eof]
> 
>     mkTapeFocus
>       :: [a] -> a -> [a]
>       -> Buffer w t a
>     mkTapeFocus as x bs =
>       Buffer $ mkTapeFocusFTZ
>         (map Cell as) (Cell x) ((map Cell bs) ++ [eof])
> 
>     unTape :: Buffer w t a -> [a]
>     unTape = unCells . unTapeFTZ . unBuffer
> 
>     isAtInit :: Buffer w t a -> Bool
>     isAtInit = isAtInitFTZ . unBuffer
> 
>     isAtLast :: Buffer w t a -> Bool
>     isAtLast = isAtLastFTZ . unBuffer
> 
>     initRead :: Buffer w t a -> Maybe a
>     initRead = join . fmap unCell . initReadFTZ . unBuffer
> 
>     initAlter :: (a -> a) -> Buffer w t a -> Buffer w t a
>     initAlter f =
>       liftBuffer $ initAlterFTZ (fmap f)
> 
>     initMove :: Buffer w t a -> Buffer w t a
>     initMove =
>       liftBuffer $ initMoveFTZ
> 
>     initInsert :: a -> Buffer w t a -> Buffer w t a
>     initInsert a =
>       liftBuffer $ initInsertFTZ (Cell a)
> 
>     initDelete :: Buffer w t a -> Buffer w t a
>     initDelete =
>       liftBuffer $ initDeleteFTZ
> 
>     lastRead :: Buffer w t a -> Maybe a
>     lastRead = join . fmap unCell . lastReadFTZ . unBuffer
> 
>     lastAlter :: (a -> a) -> Buffer w t a -> Buffer w t a
>     lastAlter f =
>       liftBuffer $ lastAlterFTZ (fmap f)
> 
>     lastMove :: Buffer w t a -> Buffer w t a
>     lastMove =
>       liftBuffer $ lastMoveFTZ
> 
>     lastInsert :: a -> Buffer w t a -> Buffer w t a
>     lastInsert a =
>       liftBuffer $ lastInsertFTZ (Cell a)
> 
>     lastDelete :: Buffer w t a -> Buffer w t a
>     lastDelete =
>       liftBuffer $ lastDeleteFTZ
> 
>     headRead :: Buffer w t a -> Maybe a
>     headRead = join . fmap unCell . headReadFTZ . unBuffer
> 
>     headAlter :: (a -> a) -> Buffer w t a -> Buffer w t a
>     headAlter f =
>       liftBuffer $ headAlterFTZ (fmap f)
> 
>     headMoveL :: Buffer w t a -> Buffer w t a
>     headMoveL =
>       liftBuffer $ headMoveLFTZ
> 
>     headMoveR :: Buffer w t a -> Buffer w t a
>     headMoveR =
>       liftBuffer $ headMoveRFTZ
> 
>     headInsertL :: a -> Buffer w t a -> Buffer w t a
>     headInsertL a = 
>       liftBuffer $ headInsertLFTZ (Cell a)
> 
>     headInsertR :: a -> Buffer w t a -> Buffer w t a
>     headInsertR a =
>       liftBuffer $ headInsertRFTZ (Cell a)
> 
>     headDeleteL :: Buffer w t a -> Buffer w t a
>     headDeleteL =
>       liftBuffer $ headDeleteLFTZ
> 
>     headDeleteR :: Buffer w t a -> Buffer w t a
>     headDeleteR =
>       liftBuffer $ headDeleteRFTZ



Constructors and destructors
----------------------------

Our buffer constructors come in two flavors: constructors prefixed with `def` take the type parameters as inputs, while those prefixed with `mk` do not.

> mkEofBuffer
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a
> mkEofBuffer = Buffer $
>   mkTapeFocusFTZ [] (eof :: Cell a) []

> mkEmptyBuffer
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a
> mkEmptyBuffer = Buffer
>   { unBuffer = emptyFTZ
>   }
> 
> mkBuffer
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a, Eq a )
>   => [a] -> Buffer w t a
> mkBuffer = mkTape
> 
> mkBufferFocus
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a, Eq a )
>   => [a] -> a -> [a] -> Buffer w t a
> mkBufferFocus = mkTapeFocus
> 
> mkBufferFocus''
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => [Cell a] -> Cell a -> [Cell a]
>   -> Buffer w t a
> mkBufferFocus'' as x bs =
>   Buffer $ FingerTreeZip $ Just (fromListFT as, x, fromListFT bs)

The `def` variants will mostly be used for testing or in the shell.

> defEmptyBuffer
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a, Eq a )
>   => Proxy w -> Proxy t -> Buffer w t a
> defEmptyBuffer _ _ = mkEmptyBuffer
> 
> defBuffer
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a, Eq a )
>   => Proxy w -> Proxy t -> [a] -> Buffer w t a
> defBuffer _ _ = mkBuffer
> 
> defBufferFocus
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a, Eq a )
>   => Proxy w -> Proxy t -> [a] -> a -> [a] -> Buffer w t a
> defBufferFocus _ _ = mkBufferFocus

> defBufferFocus''
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a, Eq a )
>   => Proxy w -> Proxy t -> [Cell a] -> Cell a -> [Cell a]
>   -> Buffer w t a
> defBufferFocus'' _ _ = mkBufferFocus''

> mkBufferFocus'
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> a -> Buffer w t a -> Buffer w t a
> mkBufferFocus' (Buffer as) x (Buffer bs) = Buffer $
>   FingerTreeZip $ Just (integrateFTZ as, Cell x, integrateFTZ bs)

> charBuffer
>   :: ( IsWidth w, IsTab t
>      , Valued (MeasureText w t) a, IsChar a, Eq a )
>   => Proxy w -> Proxy t -> [Char]
>   -> Buffer w t a
> charBuffer _ _ as = mkBuffer (map fromChar as)

> charBufferFocus
>   :: ( IsWidth w, IsTab t
>      , Valued (MeasureText w t) a, IsChar a, Eq a )
>   => Proxy w -> Proxy t -> Proxy a -> [Char] -> Char -> [Char]
>   -> Buffer w t a
> charBufferFocus _ _ _ as x bs = mkBufferFocus
>   (map fromChar as) (fromChar x) (map fromChar bs)

> unBufferFocus
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Maybe ([Cell a], Cell a, [Cell a])
> unBufferFocus buf =
>   case unFingerTreeZip $ unBuffer buf of
>     Nothing -> Nothing
>     Just (as, x, bs) ->
>       Just (toList as, x, toList bs)

And we can give a handy `Show` instance:

> instance
>   ( Show a, IsWidth w, IsTab t, IsChar a
>   ) => Show (Buffer w t a)
>   where
>     show w =
>       case unFingerTreeZip $ unBuffer w of
>         Nothing -> "mkEmptyBuffer"
>         Just (as, x, bs) -> concat
>           [ "mkBufferFocus'' "
>           , showWidth (Proxy :: Proxy w), " "
>           , showTab (Proxy :: Proxy t), " "
>           , show $ toList as, " ("
>           , show x, ") ", show $ toList bs
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
> getBufferString = map toChar . unTape



> getBufferHeadPosition
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> MeasureText w t
> getBufferHeadPosition buf =
>   case unFingerTreeZip $ unBuffer buf of
>     Nothing -> mempty
>     Just (as, x, _) -> value as <> value x
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
> resizeBuffer (Buffer x) = Buffer (remeasureFTZ x)
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
>       SizedBuffer (mkEofBuffer :: Buffer w t Glyph)
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
>   -> Buffer w t a -> Maybe (Buffer w t a)
> splitBuffer p buf =
>   let xs = integrateFTZ $ unBuffer buf in
>   case splitFT p xs of
>     Nothing -> Nothing
>     Just zs -> Just $ Buffer $ FingerTreeZip $ Just zs



> splitBufferAtLineCol
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a, Eq a )
>   => LineCol -> Buffer w t a -> Buffer w t a
> splitBufferAtLineCol lc buf =
>   case splitBuffer (atLineCol lc) buf of
>     Nothing -> lastMove buf
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
>   case splitBuffer (atScreenCoords pos) buf of
>     Nothing -> lastMove buf
>     Just xs ->
>       let
>         offset = screenCoords $ getBufferHeadPosition xs
>         pos' = applyScreenOffset offset (0,0)
>       in
>         if pos == pos'
>           then xs
>           else headMoveL xs
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
>     then Just $ initMove buf
>     else splitBuffer (atScreenLine k) buf
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
>       in (integrateFTZ w, [], mempty)
>     Just z ->
>       let Buffer (FingerTreeZip w) = z
>       in case w of
>         Nothing ->
>           (mempty, [], mempty)
>         Just (as,x,bs) ->
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
>   -> ([Maybe Int], [[a]])
> renderBuffer _ t h buf =
>   let (as, xs, _) = splitLines t h buf
>   in
>     unzip $
>       map (\(z,i) -> (i, unCells $ toList z)) $
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
> toListDebugBuffer = toListDebugFTZ . unBuffer

> showInternalBuffer
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a, Show a, IsChar a )
>   => Buffer w t a -> String
> showInternalBuffer (Buffer xs) = unwords
>   [ "Buffer", showInternalFTZ xs ]

> validateBuffer
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a )
>   => Buffer w t a -> Bool
> validateBuffer = validateFTZ . unBuffer
