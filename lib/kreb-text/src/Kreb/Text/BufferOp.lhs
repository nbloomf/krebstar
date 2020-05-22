> {-# LANGUAGE ExistentialQuantification #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE StandaloneDeriving #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE StandaloneDeriving #-}
> {-# LANGUAGE OverloadedStrings #-}

> module Kreb.Text.BufferOp where

> import Data.Proxy

> import qualified Kreb.Format as Fmt
> import           Kreb.Format (display, (<+>))
> import Kreb.Prop
> import Kreb.Reflect
> import Kreb.Arith
> import Kreb.Text.MeasureText

> data BufferOp d where
>   BufferNoOp
>     :: BufferOp d
>   BufferOpIns
>     :: (IsBase d)
>     => Loc d EventId Char
>     -> BufferOp d
>   BufferOpDel
>     :: (IsBase d)
>     => Loc d EventId Char
>     -> BufferOp d
> 
> deriving instance (IsBase d) => Eq (BufferOp d)
> deriving instance (IsBase d) => Ord (BufferOp d)
> deriving instance (IsBase d) => Show (BufferOp d)

> instance Fmt.Display (BufferOp d) where
>   display x = case x of
>     BufferNoOp    -> "BufferNoOp"
>     BufferOpIns z -> "BufferOpIns" <+> display z
>     BufferOpDel z -> "BufferOpDel" <+> display z

> invertBufferOp
>   :: ( IsBase d )
>   => EventId -> BufferOp d -> BufferOp d
> invertBufferOp eId op = case op of
>   BufferNoOp    -> BufferNoOp
>   BufferOpIns x -> BufferOpDel (setEventId eId x)
>   BufferOpDel x -> BufferOpIns (setEventId eId x)

> data BaseBufferOp = forall d.
>   ( IsBase d
>   ) => BaseBufferOp (BufferOp d)

> showDebugBaseBufferOp
>   :: BaseBufferOp -> String
> showDebugBaseBufferOp (BaseBufferOp op) =
>   case op of
>     BufferNoOp    -> "nop"
>     BufferOpIns x -> "ins" ++ show x
>     BufferOpDel x -> "del" ++ show x

> alterBaseBufferOp
>   :: forall a
>    . (forall d. ( IsBase d ) => BufferOp d -> BufferOp d)
>   -> BaseBufferOp -> BaseBufferOp
> alterBaseBufferOp f (BaseBufferOp x) =
>   BaseBufferOp (f x)

> invertBaseBufferOps
>   :: EventId -> [BaseBufferOp] -> [BaseBufferOp]
> invertBaseBufferOps eId =
>   map (alterBaseBufferOp (invertBufferOp eId)) . reverse

> witnessBufferOp
>   :: forall d1 d2 a
>    . ( IsBase d1, IsBase d2 )
>   => Proxy d2 -> BufferOp d1 -> BufferOp d2
> witnessBufferOp p op =
>   case op of
>     BufferNoOp    -> BufferNoOp
>     BufferOpIns x -> BufferOpIns (witnessLoc p x)
>     BufferOpDel x -> BufferOpDel (witnessLoc p x)
