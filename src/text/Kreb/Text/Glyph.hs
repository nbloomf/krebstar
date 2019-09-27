{-# LANGUAGE
    MultiParamTypeClasses
#-}

module Kreb.Text.Glyph (
    Glyph()
  , mkGlyph

  , GlyphRenderSettings(..)
  , defaultGlyphRenderSettings
  , renderGlyph

  , Rune(..)
  , RuneColor(..)
  , Hue(..)
  , Brightness(..)

  , plainRune
) where

import Kreb.Check
import Kreb.Struct

import Kreb.Text.ScreenOffset
import Kreb.Text.MeasureText

data GlyphRenderSettings
  = GlyphRenderSettings
    { _newlineGlyph :: Rune
    , _tabGlyph     :: Rune
    } deriving (Eq, Show)

defaultGlyphRenderSettings :: GlyphRenderSettings
defaultGlyphRenderSettings = GlyphRenderSettings
  { _newlineGlyph = Rune " " (RuneColor HueWhite BrightnessDull) (RuneColor HueBlack BrightnessDull)
  , _tabGlyph = Rune "    " (RuneColor HueWhite BrightnessDull) (RuneColor HueBlack BrightnessDull)
  }

data Rune
  = Rune String RuneColor RuneColor
  deriving (Eq, Show)

data RuneColor
  = RuneColor Hue Brightness
  deriving (Eq, Show)

data Hue
  = HueBlack
  | HueRed
  | HueGreen
  | HueYellow
  | HueBlue
  | HueMagenta
  | HueCyan
  | HueWhite
  deriving (Eq, Show)

data Brightness
  = BrightnessDull
  | BrightnessVivid
  deriving (Eq, Show)

plainRune :: Char -> Rune
plainRune c =
  Rune [c] (RuneColor HueWhite BrightnessVivid) (RuneColor HueBlack BrightnessDull)

renderGlyph :: GlyphRenderSettings -> Glyph -> Rune
renderGlyph settings g = case toChar g of
  '\n' -> _newlineGlyph settings
  '\t' -> _tabGlyph settings
  _    -> plainRune (toChar g)


data Glyph
  = Glyph Char
  deriving Eq

instance Show Glyph where
  show (Glyph c) = concat
    [ "(mkGlyph ", show c, ")" ]

mkGlyph :: Char -> Glyph
mkGlyph = Glyph

instance
  ( IsWidth w, IsTab t
  ) => Valued (MeasureText w t) Glyph
  where
    value (Glyph c) = value c

instance Arb Glyph where
  arb = mkGlyph <$> arb

instance Prune Glyph where
  prune (Glyph c) = map Glyph $ prune c



instance IsChar Glyph where
  toChar (Glyph c) = c
  fromChar = mkGlyph



