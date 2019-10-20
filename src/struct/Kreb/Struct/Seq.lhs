---
title: Kreb.Struct.Seq
---



Introduction
============

So far we've developed the [finger tree](src/Ned/Data/FingerTree.html) data type, and taken its [derivative](src/Ned/Data/OnePointedList.html) to get a linear structure with a distinguished pointer in the middle. We'll use this as the basis for a couple of more specialized structures. The first of these is `Seq`, which uses our existing `Count` monoid to describe a list with efficient indexing.

> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE InstanceSigs #-}
> 
> 
> module Kreb.Struct.Seq (
>     Seq()
>   , empty
>   , fmapSeq
>   , singleton

>   , readPoint
>   , alterPoint
> 
>   , debugShowSeq
> ) where
> 
> import           Kreb.Check
> import           Kreb.Struct.Valued
> import qualified Kreb.Struct.OnePointedList as OPL



The Seq Type
============

There's not much special about `Seq`; it's actually just a wrapper for finger trees over `Count`.

> newtype Seq a = Seq
>   { unSeq :: OPL.OnePointedList Count a
>   } deriving (Eq, Show)
> 
> empty :: Seq a
> empty = Seq OPL.empty
> 
> fmapSeq
>   :: ( Valued Count a1, Valued Count a2 )
>   => (a1 -> a2) -> Seq a1 -> Seq a2
> fmapSeq f (Seq x) = Seq $ OPL.fmapList f x

> instance
>   ( Arb a, Valued Count a
>   ) => Arb (Seq a)
>   where
>     arb = Seq <$> arb
> 
> instance
>   ( Prune a, Valued Count a
>   ) => Prune (Seq a)
>   where
>     prune (Seq x) = map Seq $ prune x



Tape
----

Most of the code for `Seq` has already been written. All we need is a helper for lifting operations on finger trees to `Seq` values:

> liftSeq
>   :: (OPL.OnePointedList Count a -> OPL.OnePointedList Count a)
>   -> Seq a -> Seq a
> liftSeq f = Seq . f . unSeq

And now the heavy lifting can be offloaded to `OnePointedList`. Note that, since `Seq` is defined as a newtype, this layer of indirection has no runtime overhead.

> singleton
>   :: ( Valued Count a )
>   => a -> Seq a
> singleton = Seq . OPL.singleton

> alterPoint
>   :: ( Valued Count a )
>   => (a -> a) -> Seq a -> Seq a
> alterPoint f (Seq xs) =
>   Seq $ OPL.alterPoint f xs

> readPoint
>   :: ( Valued Count a )
>   => Seq a -> Maybe a
> readPoint (Seq xs) =
>   OPL.readPoint xs




> debugShowSeq
>   :: ( Valued Count a )
>   => (a -> String) -> Seq a -> String
> debugShowSeq p xs =
>   "Seq\n===\n" ++ unlines [] -- TODO: fix this -- (map p $ unTape xs)
