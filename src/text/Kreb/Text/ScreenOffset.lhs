---
title: Kreb.Text.ScreenOffset
---



Contents
--------

* [Introduction](#introduction)
* [Exposed API](#exposed-api)
* [Spans](#spans)
* [Type parameters](#type-parameters)
* [Screen offsets](#screen-offsets)
* [A monoid action for screen offsets](#a-monoid-action-for-screen-offsets)



Introduction
============

Looking ahead, we'll eventually implement text buffers as a zipped finger tree of characters. To make this work we need a suitable `Monoid` instance to serve as the 'measurement' for text. But what should go in that monoid? Some natural choices are character count and byte count. Another choice is to carry line and column information so we can efficiently seek to a specific position in the text, although it's not immediately obvious that that is a monoid.

Yet another option is to have every character carry information about its position _on the screen_. This allows efficient seeking to a given screen position. Unfortunately now the monoid instance on our measurement will depend on the screen width and the tab width, the choice of which we'd like to leave up to the user. So we need our monoid instance to take two type level number parameters, but also need for these types to be invisible to the consuming code. This is precisely the problem that implicit configurations are designed to solve.

In this module we will develop a monoid of _screen offsets_.

> {-# LANGUAGE
>     ScopedTypeVariables
>   , Rank2Types
> #-}



Exposed API
===========

> module Kreb.Text.ScreenOffset (
>     ScreenOffset(..)
>   , Span(..)
>   , mkNoNewlines
>   , mkWithNewlines
>   , defNoNewlines
>   , defWithNewlines
> 
>   -- * Numeric type parameters
>   , IsWidth(..)
>   , withWidth
>   , IsTab(..)
>   , withTab

>   , applyScreenOffset
>   , applyBlockOffset

>   -- ** Utilities
>   , takeChunk
>   , takeChunks
>   , spanWidth
> ) where

> import Data.Proxy

> import Kreb.Check (Arb(..), Prune(..), Positive(..), listOf, pickFrom4)

> import Kreb.Reflect
> import Kreb.Struct



Spans
=====

Now we're starting to get to the gritty details of text in terminal windows.

Unicode characters have a _width_ attribute, for the number of 'cells' they occupy in the terminal window. For instance, ordinary latin text characters have width 1, combining characters have width 0, and East Asian script characters have width 2. Fortunately though that's it; every character that has a specific width has width 0, 1, or 2. The only exception is the _tab_ character, whose width is dynamic; the width of tab is however many cells away it is from the next tab stop, so is dependent on where the tab appears on the screen.

We will represent the possible character widths using the `Span` type.

> data Span
>   = Fixed0
>   | Fixed1
>   | Fixed2
>   | Stretchy
>   deriving (Eq, Show)
> 
> instance Arb Span where
>   arb = pickFrom4
>     ( return Fixed0
>     , return Fixed1
>     , return Fixed2
>     , return Stretchy
>     )
> 
> instance Prune Span where
>   prune _ = []

There's actually one more special case -- newlines, whose width is however many cells away it is from the end of the terminal window. But we'll handle these differently in a moment.

We will be using run length encoded lists of spans, and it will be very useful to know the total width of such a list.

> spanWidth
>   :: Int -> RunLengthEncoding Span -> Int
> spanWidth tab rle = w 0 rle
>   where
>     w :: Int -> RunLengthEncoding Span -> Int
>     w m xs = case firstRun xs of
>       Nothing -> m
>       Just (Run (k,a), bs) -> case a of
>         Fixed0 -> w m bs
>         Fixed1 -> w (m + k) bs
>         Fixed2 -> w (m + 2*k) bs
>         Stretchy ->
>           let n = tab * (1 + quot m tab) in
>           if k == 1
>             then w n bs
>             else w (n + tab*(k-1)) bs



Type parameters
===============

We'll need to encode the width and tab parameters of a text buffer in its type. To do this we'll be using the `ReflectNat` machinery.

> class IsWidth p where
>   toWidth   :: Proxy p -> Int
>   showWidth :: Proxy p -> String
> 
> instance
>   ( ReflectNat p
>   ) => IsWidth (Nat p)
>   where
>     toWidth _ = reflectNat (Proxy :: Proxy p)
>     showWidth _ = show (undefined :: Nat p)
> 
> withWidth
>   :: Int
>   -> (forall p. (IsWidth p) => Proxy p -> a)
>   -> a
> withWidth k cont = reifyNat k $
>   \(Proxy :: Proxy p) -> cont (Proxy :: Proxy (Nat p))



> class IsTab p where
>   toTab   :: Proxy p -> Int
>   showTab :: Proxy p -> String
> 
> instance
>   ( ReflectNat p
>   ) => IsTab (Nat p)
>   where
>     toTab _ = reflectNat (Proxy :: Proxy p)
>     showTab _ = show (undefined :: Nat p)
> 
> withTab
>   :: Int
>   -> (forall p. (IsTab p) => Proxy p -> a)
>   -> a
> withTab k cont = reifyNat k $
>   \(Proxy :: Proxy p) -> cont (Proxy :: Proxy (Nat p))



Screen offsets
==============

The `ScreenOffset` type models the position of a given character on the screen. More precisely, it models a _cell offset_; some number of newlines followed by a run length encoded list of spans. I imagine `ScreenOffset` in terms of moveable type. We have a given list of pieces of type (the `Span`s) that need to be wrapped to a page (the terminal window) of a given width.

> data ScreenOffset w t
>   = NoNewlines
>       (RunLengthEncoding Span)
>   | WithNewlines
>       (RunLengthEncoding Span)
>       Int
>       (RunLengthEncoding Span)
>   deriving Eq

> instance
>   ( IsWidth w, IsTab t
>   ) => Arb (ScreenOffset w t)
>   where
>     arb = do
>       let
>         run = do
>           Positive k <- arb
>           a <- arb
>           return (k,a)
>       p <- arb
>       if p
>         then
>           mkNoNewlines
>             <$> listOf run
>         else do
>           Positive m <- arb
>           mkWithNewlines
>             <$> listOf run <*> pure m <*> listOf run
> 
> instance
>   ( IsWidth w, IsTab t
>   ) => Prune (ScreenOffset w t)
>   where
>     prune x = case x of
>       NoNewlines zs -> do
>         cs <- prune zs
>         return $ NoNewlines cs
>       WithNewlines xs k ys ->
>         [ NoNewlines xs, NoNewlines ys ] ++
>         do
>           as <- prune xs
>           m <- [1..k]
>           bs <- prune ys
>           return $ WithNewlines as m bs

We need a monoid instance for `ScreenOffset`, which intuitively works something like this. Given two screen offsets, if the second has no line offset, then we can just concatenate the second block offset to the first. But this may cause the block offset to march off the right edge of the window, so we need to wrap it first (possibly adjusting the line offset as we go). If the second screen offset does have a line offset, we again need to wrap the first block offset to figure out how many lines it occupies. So in both cases _wrapping_ a block offset is a crucial operation.

To be a little more specific about what is meant by wrapping, imagine we have a physical sequence of blocks of width 0, 1, or 2 (Forget tabs for a moment) and a grid of some fixed integer width. By wrapping we mean to line up the blocks, in order, fitting as many as we can on a line before shifting down and starting over.

We can break the problem of wrapping lists of spans into subproblems. First consider how to break off the _first_ full line of spans -- we can think about this recursively. If there is no space left on the current line, pull as many width 0 spans off of the block list as we can. If there is space left on the current line, take the next block in the queue. If it fits, place it and move on, and if it doesn't we're done.

The `takeChunk` function performs this operation. The details are a little hairier because (1) we have to account for tabs too and (2) the queue of spans is run length encoded for efficiency reasons, and we can use this to optimize here as well but it makes the arithmetic much fiddlier.

> takeChunk
>   :: Int -> Int -> RunLengthEncoding Span
>   -> Maybe (RunLengthEncoding Span, RunLengthEncoding Span)
> takeChunk width tab rle =
>   if isEmptyRLE rle
>     then Nothing
>     else greedy 0 mempty rle
>   where
>     greedy
>       :: Int
>       -> RunLengthEncoding Span
>       -> RunLengthEncoding Span
>       -> Maybe (RunLengthEncoding Span, RunLengthEncoding Span)
>     greedy m as bs =
>       if m >= width
> 
>         -- chunk is full; make sure remainder is not empty.
>         then case firstRun bs of
>           Nothing -> Just (as, mempty)
>           Just (Run (k, a), ds) ->
>             case a of
>               Fixed0 -> greedy m (as <> fromFreqList [(k,a)]) ds
>               _      -> Just (as, bs)
> 
>         -- chunk is not full yet
>         else case firstRun bs of
>           Nothing -> Nothing
>           Just (Run (k,a), ds) ->
>             
>             case a of
>               Fixed0 -> greedy m (as <> fromFreqList [(k,a)]) ds
> 
>               Fixed1 -> let capacity = width - m in
>                 if k <= capacity
>                   then greedy (m+k) (as <> fromFreqList [(k,a)]) ds
>                   else Just
>                     ( as <> fromFreqList [(capacity,a)]
>                     , fromFreqList [(k-capacity,a)] <> ds )
> 
>               Fixed2 -> let capacity = width - m in
>                 if capacity < 2
>                   then Just (as, bs)
>                   else if (2*k) <= capacity
>                     then greedy (m + 2*k)
>                       (as <> fromFreqList [(k,a)]) ds
>                     else let q = capacity `quot` 2 in
>                       greedy (m + 2*q)
>                         (as <> fromFreqList [(q,a)])
>                         (fromFreqList [(k-q,a)] <> ds)
> 
>               Stretchy ->
>                 -- next tab stop
>                 let n = tab * (1 + quot m tab) in
>                 if n >= width
>                   then greedy n
>                     (as <> fromFreqList [(1,a)])
>                     (fromFreqList [(k-1,a)] <> ds)
>                   else let capacity = width - n in
>                     if (tab*(k-1)) <= capacity
>                       then greedy (n + tab*(k-1))
>                         (as <> fromFreqList [(k,a)]) ds
>                       else let q = capacity `quot` tab in
>                         greedy (n + tab*q)
>                           (as <> fromFreqList [(q+1,a)])
>                           (fromFreqList [(k-q-1,a)] <> ds)

Then `takeChunks` repeatedly tries to take the _first_ chunk, bailing out when it can't. Note that the taken chunks must all be 'full' lines.

> takeChunks
>   :: Int -> Int -> RunLengthEncoding Span
>   -> ([RunLengthEncoding Span], RunLengthEncoding Span)
> takeChunks width tab rle =
>   if (width <= 0) || (tab <= 0)
>     then ([], rle)
>     else greedy [] rle
>   where
>     greedy
>       :: [RunLengthEncoding Span]
>       -> RunLengthEncoding Span
>       -> ([RunLengthEncoding Span], RunLengthEncoding Span)
>     greedy xs z =
>       case takeChunk width tab z of
>         Nothing -> (reverse xs, z)
>         Just (as, bs) -> greedy (as:xs) bs

Now we can define a semigroup instance on screen offsets using `takeChunks` to determine the number of lines occupied by a list of spans.

> instance
>   ( IsWidth w, IsTab t
>   ) => Semigroup (ScreenOffset w t)
>   where
>     x <> y = case (x,y) of
>       (NoNewlines a, NoNewlines b) ->
>         NoNewlines (a <> b)
>       (NoNewlines a, WithNewlines b1 v b2) ->
>         WithNewlines (a <> b1) v b2
>       (WithNewlines a1 u a2, NoNewlines b) ->
>         WithNewlines a1 u (a2 <> b)
>       (WithNewlines a1 u a2, WithNewlines b1 v b2) ->
>         let
>           w = toWidth (Proxy :: Proxy w)
>           t = toTab (Proxy :: Proxy t)
>           (c1,c2) = takeChunks w t (a2 <> b1)
>           k = length c1
>         in
>           WithNewlines a1 (u + k + v) b2
> 
> instance
>   ( IsWidth w, IsTab t
>   ) => Monoid (ScreenOffset w t)
>   where
>     mempty = NoNewlines mempty

We're finally prepared to define a helper function for constructing screen offsets. The monoid instance is needed for this so that the value is put into canonical form at definition time.

> mkNoNewlines
>   :: ( IsWidth w, IsTab t )
>   => [(Int, Span)]
>   -> ScreenOffset w t
> mkNoNewlines xs =
>   NoNewlines (fromFreqList xs)
> 
> mkWithNewlines
>   :: ( IsWidth w, IsTab t )
>   => [(Int, Span)] -> Int -> [(Int, Span)]
>   -> ScreenOffset w t
> mkWithNewlines as k bs =
>   WithNewlines (fromFreqList as) k (fromFreqList bs)

> defNoNewlines
>   :: ( IsWidth w, IsTab t )
>   => Proxy w -> Proxy t
>   -> [(Int, Span)]
>   -> ScreenOffset w t
> defNoNewlines _ _ = mkNoNewlines
> 
> defWithNewlines
>   :: ( IsWidth w, IsTab t )
>   => Proxy w -> Proxy t
>   -> [(Int, Span)] -> Int -> [(Int, Span)]
>   -> ScreenOffset w t
> defWithNewlines _ _ = mkWithNewlines

> instance
>   ( IsWidth w, IsTab t
>   ) => Show (ScreenOffset w t)
>   where
>     show x = case x of
>       NoNewlines xs -> concat
>         [ "defNoNewlines "
>         , showWidth (Proxy :: Proxy w), " "
>         , showTab (Proxy :: Proxy t), " "
>         , show $ toFreqList xs
>         ]
>       WithNewlines as k bs -> concat
>         [ "defWithNewlines "
>         , showWidth (Proxy :: Proxy w), " "
>         , showTab (Proxy :: Proxy t), " "
>         , show $ toFreqList as, " "
>         , show k, " "
>         , show $ toFreqList bs
>         ]



A monoid action for screen offsets
==================================

The purpose of screen offsets is to let us express _relative_ positions in the terminal window, so we can (for instance) have a chunk of text and know quickly at which position the _following_ chunk of text will start. First a helper for span lists:

> applyBlockOffset
>   :: Int -> Int -> RunLengthEncoding Span
>   -> (Int, Int) -> (Int, Int)
> applyBlockOffset width tab rle x =
>   foldl f x rle
>   where
>     f (c,l) b = case b of
>       Fixed0 -> (c,l)
> 
>       Fixed1 ->
>         if c+1 >= width
>           then (0, l+1)
>           else (c+1, l)
> 
>       Fixed2 ->
>        case compare (c+2) width of
>          LT -> (c+2, l)
>          EQ -> (0,   l+1)
>          GT -> (2,   l+1)
> 
>       Stretchy ->
>         let n = tab * (1 + quot c tab) in
>         if n >= width
>           then (0, l+1)
>           else (n, l)

And now the most important function of this module after the monoid instance -- applying a screen offset to a pair of screen coordinates.

> applyScreenOffset
>   :: forall w t
>    . ( IsWidth w, IsTab t )
>   => ScreenOffset w t
>   -> (Int, Int) -> (Int, Int)
> applyScreenOffset x pos =
>   let
>     w = toWidth (Proxy :: Proxy w)
>     t = toTab (Proxy :: Proxy t)
>   in
>     case x of
>       NoNewlines cs ->
>         applyBlockOffset w t cs pos
>       WithNewlines as k bs ->
>         let (_,v) = applyBlockOffset w t as pos
>         in applyBlockOffset w t bs (0,k+v)

There's no standard type class for this, but in the tests we'll verify that `applyScreenOffset` is a monoid action on the set of valid screen coordinates.
