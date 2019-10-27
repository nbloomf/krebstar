---
title: Rose Trees
---

> module Kreb.Struct.RoseTree (

> ) where

> import Kreb.Struct.Sequence


> data RoseTree a
>   = Empty
>   | Branch a (Sequence a)
>   deriving (Eq, Show)

