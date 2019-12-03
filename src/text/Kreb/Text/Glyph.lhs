> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FlexibleContexts #-}
> 
> module Kreb.Text.Glyph (
>    Glyph(..)
>  , plainGlyph

>  , GlyphRenderSettings(..)
>  , defaultGlyphRenderSettings
>  , renderGlyph



>  , plainRune
>  , dimRune
>  , phantomCursorRune
> ) where

> import Kreb.Check
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
>  ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a
>  ) => Valued (MeasureText w t d) (Glyph a)
>  where
>    value (Glyph c _ _) = value c
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






> data GlyphRenderSettings
>  = GlyphRenderSettings
>    { _newlineGlyph :: [Glyph String]
>    , _tabGlyph     :: Int -> Int -> [Glyph String]
>    }

> defaultGlyphRenderSettings :: GlyphRenderSettings
> defaultGlyphRenderSettings = GlyphRenderSettings
>  { _newlineGlyph = [Glyph " " vividWhite dullBlack]
>  , _tabGlyph = \tab col ->
>      let
>        k = tab - rem col tab
>      in replicate k $ Glyph " " vividWhite dullBlack
>  }

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

> renderGlyph :: (IsChar a) => GlyphRenderSettings -> Int -> (Glyph a, Int) -> [Glyph String]
> renderGlyph settings tab (Glyph c f b, col) = case toChar c of
>  '\n' -> _newlineGlyph settings
>  '\t' -> _tabGlyph settings tab col
>  _    -> [Glyph [toChar c] f b]







