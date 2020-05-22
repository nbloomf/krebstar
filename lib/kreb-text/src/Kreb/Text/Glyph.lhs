> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE InstanceSigs #-}
> 
> module Kreb.Text.Glyph (
>    Glyph(..)
>  , plainGlyph



>  , plainRune
>  , dimRune
>  , phantomCursorRune
> ) where

> import Kreb.Prop
> import Kreb.Struct

> import Kreb.Text.ScreenOffset
> import Kreb.Text.Rune
> import Kreb.Text.MeasureText
> import Kreb.Text.Pigment


> data Glyph a = Glyph
>  { glyph :: a
>  , fgColor :: Pigment
>  , bgColor :: Pigment
>  } deriving (Eq, Ord, Show)

> plainGlyph
>   :: a -> Glyph a
> plainGlyph c = Glyph c vividWhite dullBlack

> instance
>   ( Valued a
>   ) => Valued (Glyph a)
>   where
>     type Value (Glyph a) = Value a
> 
>     value :: Glyph a -> Value a
>     value (Glyph c _ _) = value c
> 
> instance
>   ( IsChar a
>   ) => IsChar (Glyph a) where
>  toChar (Glyph c _ _) = toChar c
>  fromChar = plainGlyph . fromChar

> instance
>   ( Arb a
>   ) => Arb (Glyph a)
>   where
>     arb = Glyph <$> arb <*> arb <*> arb
> 
> instance
>   ( Prune a
>   ) => Prune (Glyph a)
>   where
>     prune (Glyph c u v) =
>       map (\x -> Glyph x u v) $ prune c






> data Rune
>  = Rune String Pigment Pigment
>  deriving (Eq, Show)



> plainRune :: Char -> Glyph String
> plainRune c =
>  Glyph [c] vividWhite dullBlack

> dimRune :: Char -> Glyph String
> dimRune c =
>  Glyph [c] dullWhite dullBlack

> phantomCursorRune :: Glyph String
> phantomCursorRune =
>  Glyph [' '] dullBlack dullWhite









