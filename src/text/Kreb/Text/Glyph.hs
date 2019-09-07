{-# LANGUAGE
    MultiParamTypeClasses
#-}

module Kreb.Text.Glyph (
    Glyph()
  , mkGlyph
) where

import Kreb.Check
import Kreb.Struct

import Kreb.Text.ScreenOffset
import Kreb.Text.MeasureText



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



instance IsChar Glyph where
  toChar (Glyph c) = c
  fromChar = mkGlyph



