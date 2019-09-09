---
title: Kreb.Struct.Seq
---



Contents
--------

* [Introduction](#introduction)
* [The Seq Type](#the-seq-type)
    * [Tape](#tape)



Introduction
============

So far we've developed the [finger tree](src/Ned/Data/FingerTree.html) data type, and taken its [derivative](src/Ned/Data/FingerTreeZip.html) to get a linear structure with a distinguished pointer in the middle. We'll use this as the basis for a couple of more specialized structures. The first of these is `Seq`, which uses our existing `Count` monoid to describe a list with efficient indexing.

> {-# LANGUAGE
>     MultiParamTypeClasses
>   , UndecidableInstances
>   , FlexibleInstances
>   , FlexibleContexts
>   , InstanceSigs
> #-}
> 
> 
> module Kreb.Struct.Seq (
>     Seq()
>   , emptySeq
>   , fmapSeq
> 
>   , debugShowSeq
> ) where
> 
> import Kreb.Check (Arb(..), Prune(..))
> 
> import Kreb.Struct.FingerTree
> import Kreb.Struct.FingerTreeZip



The Seq Type
============

There's not much special about `Seq`; it's actually just a wrapper for finger trees over `Count`.

> newtype Seq a = Seq
>   { unSeq :: FingerTreeZip Count a
>   } deriving (Eq, Show)
> 
> emptySeq :: Seq a
> emptySeq = Seq
>   { unSeq = emptyFTZ
>   }
> 
> fmapSeq
>   :: ( Valued Count a1, Valued Count a2 )
>   => (a1 -> a2) -> Seq a1 -> Seq a2
> fmapSeq f (Seq x) = Seq $ fmapFTZ f x

> instance
>   ( Arb a, Valued Count a
>   ) => Arb (Seq a)
>   where
>     arb = mkTapeFocus
>       <$> arb <*> arb <*> arb
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
>   :: (FingerTreeZip Count a -> FingerTreeZip Count a)
>   -> Seq a -> Seq a
> liftSeq f = Seq . f . unSeq

And now the heavy lifting can be offloaded to `FingerTreeZip`. Note that, since `Seq` is defined as a newtype, this layer of indirection has no runtime overhead.

> instance
>   ( Valued Count a
>   ) => Tape Seq a
>   where
>     isEmpty :: Seq a -> Bool
>     isEmpty = isEmptyFTZ . unSeq
> 
>     mkTape :: [a] -> Seq a
>     mkTape = Seq . mkTapeFTZ
> 
>     mkTapeFocus :: [a] -> a -> [a] -> Seq a
>     mkTapeFocus as x bs =
>       Seq $ mkTapeFocusFTZ as x bs
> 
>     unTape :: Seq a -> [a]
>     unTape = unTapeFTZ . unSeq
> 
>     isAtInit :: Seq a -> Bool
>     isAtInit = isAtInitFTZ . unSeq
> 
>     isAtLast :: Seq a -> Bool
>     isAtLast = isAtLastFTZ . unSeq
> 
>     initRead :: Seq a -> Maybe a
>     initRead = initReadFTZ . unSeq
> 
>     initAlter :: (a -> a) -> Seq a -> Seq a
>     initAlter f =
>       liftSeq $ initAlterFTZ f
> 
>     initMove :: Seq a -> Seq a
>     initMove =
>       liftSeq $ initMoveFTZ
> 
>     initInsert :: a -> Seq a -> Seq a
>     initInsert a =
>       liftSeq $ initInsertFTZ a
> 
>     initDelete :: Seq a -> Seq a
>     initDelete =
>       liftSeq $ initDeleteFTZ
> 
>     lastRead :: Seq a -> Maybe a
>     lastRead = lastReadFTZ . unSeq
> 
>     lastAlter :: (a -> a) -> Seq a -> Seq a
>     lastAlter f =
>       liftSeq $ lastAlterFTZ f
> 
>     lastMove :: Seq a -> Seq a
>     lastMove =
>       liftSeq $ lastMoveFTZ
> 
>     lastInsert :: a -> Seq a -> Seq a
>     lastInsert a =
>       liftSeq $ lastInsertFTZ a
> 
>     lastDelete :: Seq a -> Seq a
>     lastDelete =
>       liftSeq $ lastDeleteFTZ
> 
>     headRead :: Seq a -> Maybe a
>     headRead = headReadFTZ . unSeq
> 
>     headAlter :: (a -> a) -> Seq a -> Seq a
>     headAlter f =
>       liftSeq $ headAlterFTZ f
> 
>     headMoveL :: Seq a -> Seq a
>     headMoveL =
>       liftSeq $ headMoveLFTZ
> 
>     headMoveR :: Seq a -> Seq a
>     headMoveR =
>       liftSeq $ headMoveRFTZ
> 
>     headInsertL :: a -> Seq a -> Seq a
>     headInsertL a = 
>       liftSeq $ headInsertLFTZ a
> 
>     headInsertR :: a -> Seq a -> Seq a
>     headInsertR a =
>       liftSeq $ headInsertRFTZ a
> 
>     headDeleteL :: Seq a -> Seq a
>     headDeleteL =
>       liftSeq $ headDeleteLFTZ
> 
>     headDeleteR :: Seq a -> Seq a
>     headDeleteR =
>       liftSeq $ headDeleteRFTZ



> debugShowSeq
>   :: ( Valued Count a )
>   => (a -> String) -> Seq a -> String
> debugShowSeq p xs =
>   "Seq\n===\n" ++ unlines (map p $ unTape xs)
