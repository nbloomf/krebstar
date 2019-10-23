> {-# LANGUAGE ExistentialQuantification #-}
> {-# LANGUAGE QuantifiedConstraints #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE Rank2Types #-}

> module Kreb.Text.SizedBuffer where

> import Data.Proxy

> import Kreb.Reflect
> import Kreb.Struct.Valued
> import Kreb.Text.ScreenOffset
> import Kreb.Text.Glyph
> import Kreb.Text.MeasureText
> import Kreb.Text.Buffer




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
>       SizedBuffer (fromList (map fromChar str) :: Buffer w t Glyph)
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

