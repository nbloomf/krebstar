{-# LANGUAGE
    MultiParamTypeClasses
#-}

module Ned.Data.Glyph (
    Glyph()
  , mkGlyph
) where

import Ned.Data.FingerTree
import Ned.Data.ScreenOffset
import Ned.Data.MeasureText



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



instance IsChar Glyph where
  toChar (Glyph c) = c
  fromChar = mkGlyph



