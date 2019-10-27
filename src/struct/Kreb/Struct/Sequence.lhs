---
title: Sequences
---

::: contents
* [Introduction](#introduction): 
:::



::: frontmatter

> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE InstanceSigs #-}
> 
> module Kreb.Struct.Sequence (
>     Sequence()
>   , empty
>   , singleton
>   , fromList

>   , isEmpty
>   , isSingleton
>   , isAtInit
>   , isAtLast

>   , readPoint
>   , readInit
>   , readLast

>   , movePointLeft
>   , movePointRight
>   , moveToInit
>   , moveToLast

>   , alterInit
>   , alterLast
>   , alterPoint

>   , insertInit
>   , deleteInit
>   , insertLast
>   , deleteLast

>   , insertPointLeft
>   , deletePointLeft
>   , insertPointRight
>   , deletePointRight



>   , fmapSequence
> 
>   , debugShowSequence
> ) where
> 
> import Data.Foldable
> 
> import           Kreb.Check
> import           Kreb.Struct.Valued
> import qualified Kreb.Struct.OnePointedList as OPL

:::



Introduction
------------

So far we've developed the [finger tree](src/Ned/Data/FingerTree.html) data type, and taken its [derivative](src/Ned/Data/OnePointedList.html) to get a linear structure with a distinguished pointer in the middle. We'll use this as the basis for a couple of more specialized structures. The first of these is `Seq`, which uses our existing `Count` monoid to describe a list with efficient indexing.

> newtype Sequence a = Sequence
>   { unSequence :: OPL.OnePointedList Count a
>   } deriving (Eq, Show)



> empty :: Sequence a
> empty = Sequence OPL.empty

> singleton
>   :: ( Valued Count a )
>   => a -> Sequence a
> singleton = Sequence . OPL.singleton

> fromList
>   :: ( Valued Count a )
>   => [a] -> Sequence a
> fromList = Sequence . OPL.makeFromList

> makePointSequence
>   :: ( Valued Count a )
>   => [a] -> a -> [a]
>   -> Sequence a
> makePointSequence as x bs =
>   Sequence $ OPL.makePoint as x bs

> instance Foldable Sequence where
>   toList =
>     toList . unSequence
> 
>   foldr f e =
>     foldr f e . unSequence
> 
>   foldl f e =
>     foldl f e . unSequence



> fmapSequence
>   :: ( Valued Count a1, Valued Count a2 )
>   => (a1 -> a2) -> Sequence a1 -> Sequence a2
> fmapSequence f (Sequence x) = Sequence $ OPL.fmapList f x



> isEmpty
>   :: Sequence a -> Bool
> isEmpty =
>   OPL.isEmpty . unSequence


> isSingleton
>   :: ( Valued Count a )
>   => Sequence a -> Bool
> isSingleton =
>   OPL.isSingleton . unSequence

> isAtInit
>   :: ( Valued Count a )
>   => Sequence a -> Bool
> isAtInit =
>   OPL.isAtInit . unSequence

> isAtLast
>   :: ( Valued Count a )
>   => Sequence a -> Bool
> isAtLast =
>   OPL.isAtLast . unSequence

> readInit
>   :: ( Valued Count a )
>   => Sequence a -> Maybe a
> readInit =
>   OPL.readInit . unSequence

> readLast
>   :: ( Valued Count a )
>   => Sequence a -> Maybe a
> readLast =
>   OPL.readLast . unSequence

> readPoint
>   :: ( Valued Count a )
>   => Sequence a -> Maybe a
> readPoint =
>   OPL.readPoint . unSequence

> movePointLeft
>   :: ( Valued Count a )
>   => Sequence a -> Sequence a
> movePointLeft =
>   Sequence . OPL.movePointLeft . unSequence

> movePointRight
>   :: ( Valued Count a )
>   => Sequence a -> Sequence a
> movePointRight =
>   Sequence . OPL.movePointRight . unSequence

> moveToInit
>   :: ( Valued Count a )
>   => Sequence a -> Sequence a
> moveToInit =
>   Sequence . OPL.moveToInit . unSequence

> moveToLast
>   :: ( Valued Count a )
>   => Sequence a -> Sequence a
> moveToLast =
>   Sequence . OPL.moveToLast . unSequence

> insertInit
>   :: ( Valued Count a )
>   => a -> Sequence a -> Sequence a
> insertInit a =
>   Sequence . OPL.insertInit a . unSequence

> deleteInit
>   :: ( Valued Count a )
>   => Sequence a -> Sequence a
> deleteInit =
>   Sequence . OPL.deleteInit . unSequence

> insertLast
>   :: ( Valued Count a )
>   => a -> Sequence a -> Sequence a
> insertLast a =
>   Sequence . OPL.insertLast a . unSequence

> deleteLast
>   :: ( Valued Count a )
>   => Sequence a -> Sequence a
> deleteLast =
>   Sequence . OPL.deleteLast . unSequence

> insertPointLeft
>   :: ( Valued Count a )
>   => a -> Sequence a -> Sequence a
> insertPointLeft a =
>   Sequence . OPL.insertPointLeft a . unSequence

> deletePointLeft
>   :: ( Valued Count a )
>   => Sequence a -> Sequence a
> deletePointLeft =
>   Sequence . OPL.deletePointLeft . unSequence

> insertPointRight
>   :: ( Valued Count a )
>   => a -> Sequence a -> Sequence a
> insertPointRight a =
>   Sequence . OPL.insertPointRight a . unSequence

> deletePointRight
>   :: ( Valued Count a )
>   => Sequence a -> Sequence a
> deletePointRight =
>   Sequence . OPL.deletePointRight . unSequence

> alterInit
>   :: ( Valued Count a )
>   => (a -> a)
>   -> Sequence a -> Sequence a
> alterInit f =
>   Sequence . OPL.alterInit f . unSequence

> alterLast
>   :: ( Valued Count a )
>   => (a -> a)
>   -> Sequence a -> Sequence a
> alterLast f =
>   Sequence . OPL.alterLast f . unSequence

> alterPoint
>   :: ( Valued Count a )
>   => (a -> a)
>   -> Sequence a -> Sequence a
> alterPoint f =
>   Sequence . OPL.alterPoint f . unSequence











Tape
----

Most of the code for `Seq` has already been written. All we need is a helper for lifting operations on finger trees to `Seq` values:

> liftSequence
>   :: (OPL.OnePointedList Count a -> OPL.OnePointedList Count a)
>   -> Sequence a -> Sequence a
> liftSequence f = Sequence . f . unSequence

And now the heavy lifting can be offloaded to `OnePointedList`. Note that, since `Seq` is defined as a newtype, this layer of indirection has no runtime overhead.








> debugShowSequence
>   :: ( Valued Count a )
>   => (a -> String) -> Sequence a -> String
> debugShowSequence p xs =
>   "Seq\n===\n" ++ unlines [] -- TODO: fix this -- (map p $ unTape xs)





> instance
>   ( Arb a, Valued Count a
>   ) => Arb (Sequence a)
>   where
>     arb = Sequence <$> arb
> 
> instance
>   ( Prune a, Valued Count a
>   ) => Prune (Sequence a)
>   where
>     prune (Sequence x) = map Sequence $ prune x
