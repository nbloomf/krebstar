---
title: Sized Buffers
---

::: contents
* [A Problem](#a-problem): Have types gone too far?
* [Working with sized buffers](#working-with-sized-buffers): Interacting with a typeless type
:::



::: frontmatter

> {-# LANGUAGE ExistentialQuantification #-}
> {-# LANGUAGE QuantifiedConstraints #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE TypeSynonymInstances #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE Rank2Types #-}
> 
> module Kreb.Text.SizedBuffer (
>     SizedBuffer()
>   , emptySizedBuffer
>   , makeSizedBuffer
>   , shapeSizedBuffer
>   , alterSizedBuffer
>   , alterSizedBuffer'
>   , alterSizedBufferOp
>   , alterSizedBufferF
>   , alterSizedBufferF'
>   , querySizedBuffer
> 
>   , Base()
> ) where
> 
> import Data.Proxy
> 
> import Kreb.Reflect
> import Kreb.Arith
> import Kreb.Struct.Class.Valued
> import Kreb.Text.BufferOp
> import Kreb.Text.ScreenOffset
> import Kreb.Text.Grapheme
> import Kreb.Text.MeasureText
> import Kreb.Text.Buffer

:::



A Problem
---------

The `Buffer` type is parameterized by two type level natural numbers. This is necessary to make the monoid instance for `MeasureText` work out nicely (and statically), but is also cumbersome to work with -- we've seen this already with the awkward type signatures and `natN` parameters we needed to use when testing buffers by hand, and it will only get worse when we start juggling several text buffers at once. Consumers of this code need to know that a buffer has a well defined width and tab stop, but don't really want to care about the _types_ used to express this.

What we want is to be able to hide the `w` and `t` type parameters -- and this is precisely the problem existentially quantified types can solve. We can wrap the `Buffer` type in the following _existential type_, `SizedBuffer`.

> data SizedBuffer = forall w t.
>   ( IsWidth w, IsTab t
>   ) => SizedBuffer (Buffer w t Base)

> type Base = Nat Nat16

Note that the type variables `w` and `t` are bound on the right hand side of the definition, so they cannot "leak out". Code consuming `SizedBuffers` cannot know about or do anything with `w` and `t` beyond what is allowed by the `IsWidth` and `IsTab` constraints. This is really powerful! `Buffer` can take full advantage of the benefits of very expressive types, and `SizedBuffer` acts like a guard giving the outside world the ability to ignore those types. It's _almost_ like we have dependent types -- types depending on terms -- albeit in a very restricted form.

The type constraint inside `SizedBuffer`, on its face, makes it tricky to write fully polymorphic code over the `a` variable. But in practice all the `Valued` instances are universally quantified over `w` and `t` in the correct classes, so we can bundle this constraint in a way that refers only to `a` explicitly. I'll call this class `Face` as a reference to typefaces, because in a sense it captures the notion of a character-like type which can be measured, and typefaces are subject to lots of different measurements.

We can also give `Eq` and `Show` instances for sized buffers, but these are only really useful for testing and development. In practice we don't care that much about 'identity' on sized buffers, and the closest we'll get to 'showing' a buffer is rendering it.

> instance Eq SizedBuffer where
>   (SizedBuffer b1) == (SizedBuffer b2) = and
>     [ (getBufferWidth b1)
>         == (getBufferWidth b2)
>     , (getBufferTabStop b1)
>         == (getBufferTabStop b2)
>     , (toString b1)
>         == (toString b2)
>     ]
> 
> instance Show SizedBuffer where
>   show (SizedBuffer buf) = 
>     "SizedBuffer (" ++ show buf ++ ")"



Working with sized buffers
--------------------------

Existential types neatly solve the problem of how to express data with a hidden type parameter. However we still need a way to interact with the information so hidden. Unsurprisingly this requires some (not too bad) type hackery, which is mercifully contained entirely inside this module. Exactly how this works depends on whether or not we are touching the hidden type parameters.

First we consider operations which involve fixing or changing the hidden parameters. In this case we use the helper functions `withWidth` and `withTab` to turn value parameters into types.

> emptySizedBuffer
>   :: Int -> Int -> SizedBuffer
> emptySizedBuffer width tab =
>   withWidth width $ \(_ :: (IsWidth w) => Proxy w) ->
>     withTab tab $ \(_ :: (IsTab t) => Proxy t) ->
>       SizedBuffer (emptyBuffer :: Buffer w t Base)

We're doing some weird stuff with type annotations here! But again, all the type-level weirdness is isolated to this module; consumers won't need to worry about it.

::: doctest

 > -- $
 > -- >>> :{
 > -- let
 > --   x :: SizedBuffer Char
 > --   x = emptySizedBuffer 8 2
 > -- in print x
 > -- :}
 > -- SizedBuffer (makePointOnlyBuffer nat8 nat2 [] eof []
 > -- deleted: [])
 > --
 > -- >>> :{
 > -- let
 > --   x, y :: SizedBuffer Char
 > --   x = emptySizedBuffer 8 2
 > --   y = emptySizedBuffer 10 3
 > -- in x == y
 > -- :}
 > -- False

:::

Next we have the problem of constructing a nonempty sized buffer. We'll define this in terms of the underlying `fromList` function on buffers -- in particular, this utility takes a list as input.

> makeSizedBuffer
>   :: EventId -> Int -> Int -> String -> SizedBuffer
> makeSizedBuffer eId width tab as =
>   withWidth width $ \(_ :: Proxy w) ->
>     withTab tab $ \(_ :: Proxy t) ->
>       SizedBuffer (fromString eId as :: Buffer w t Base)

The final utility for messing with the hidden type parameters changes the size of the underlying buffer.

> shapeSizedBuffer
>   :: Int -> Int -> SizedBuffer -> SizedBuffer
> shapeSizedBuffer width tab (SizedBuffer (buf :: Buffer w0 t0 Base)) =
>   withWidth width $ \(_ :: Proxy w) ->
>     withTab tab $ \(_ :: Proxy t) ->
>       SizedBuffer (resizeBuffer buf :: Buffer w t Base)

Next we have functions which don't mess with the hidden parameters. In this case there's really only two things we can do: _alter_ the underlying buffer or _query_ the buffer to retrieve a value.

To alter the buffer, we take a buffer-altering function. Note the quantified type signature of the altering function.

> alterSizedBuffer
>   :: (forall w t d. (IsWidth w, IsTab t, IsBase d)
>       => Buffer w t d -> Buffer w t d)
>   -> SizedBuffer
>   -> SizedBuffer
> alterSizedBuffer f (SizedBuffer buf) =
>   SizedBuffer (f buf)

We also define a version taking an alter function returning a functor-wrapped valued.

> alterSizedBufferF
>   :: forall f
>    . ( Functor f )
>   => (forall w t d. (IsWidth w, IsTab t, IsBase d)
>       => Buffer w t d -> f (Buffer w t d))
>   -> SizedBuffer
>   -> f SizedBuffer
> alterSizedBufferF f (SizedBuffer buf) =
>   fmap SizedBuffer (f buf)

> alterSizedBuffer'
>   :: forall u
>    . (forall w t d. (IsWidth w, IsTab t)
>       => Buffer w t Base -> (Buffer w t Base, u))
>   -> SizedBuffer
>   -> (SizedBuffer, u)
> alterSizedBuffer' f (SizedBuffer buf) =
>   let (buf', u) = f buf
>   in (SizedBuffer buf', u)

> alterSizedBufferOp
>   :: forall u
>    . (forall w t d. (IsWidth w, IsTab t)
>       => Buffer w t Base -> (Buffer w t Base, BufferOp Base))
>   -> SizedBuffer
>   -> (SizedBuffer, BaseBufferOp)
> alterSizedBufferOp f (SizedBuffer buf) =
>   let (buf', u) = f buf
>   in (SizedBuffer buf', BaseBufferOp u)

> alterSizedBufferF'
>   :: forall u f
>    . ( Functor f )
>   => (forall w t d. (IsWidth w, IsTab t)
>       => Buffer w t Base -> f (Buffer w t Base, u))
>   -> SizedBuffer
>   -> f (SizedBuffer, u)
> alterSizedBufferF' g (SizedBuffer buf) =
>   let r = g buf
>   in fmap (\(x,y) -> (SizedBuffer x, y)) r

Finally, a utility which extracts a value from a sized buffer.

> querySizedBuffer
>   :: forall a
>    . (forall w t d. (IsWidth w, IsTab t, IsBase d)
>       => Buffer w t d -> a)
>   -> SizedBuffer
>   -> a
> querySizedBuffer f (SizedBuffer buf) =
>   f buf

The utilities designed in this module don't expose any new functionality for sized buffers, and they don't have to. Instead they provide a small set of ways to manipulate sized buffers without needing to worry about type weirdness.
