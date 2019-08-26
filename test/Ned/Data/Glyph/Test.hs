{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ned.Data.Glyph.Test (
    IsGlyph(..)

  , Ascii(..)
  , charAscii
  , pAscii

  , NonNewline(..)
) where

import Data.Proxy

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

import Ned.Data.Glyph
import Ned.Data.FingerTree
import Ned.Data.MeasureText
import Ned.Data.ScreenOffset
import Ned.Data.Buffer

instance
  Arbitrary Glyph
  where
    arbitrary = mkGlyph
      <$> arbitrary

class IsGlyph a where
  toGlyph :: a -> Glyph
  fromGlyph :: Glyph -> a




-- Printable Ascii Characters

data Ascii
  = Ascii Glyph
  deriving Eq

charAscii :: Char -> Ascii
charAscii = Ascii . fromChar

instance IsGlyph Ascii where
  toGlyph (Ascii c) = c
  fromGlyph = Ascii

instance IsChar Ascii where
  toChar = toChar . toGlyph
  fromChar = fromGlyph . fromChar

pAscii :: Proxy Ascii
pAscii = Proxy

instance Show Ascii where
  show (Ascii c) = show c

instance
  ( IsWidth w, IsTab t
  ) => Valued (MeasureText w t) Ascii where
  value (Ascii c) = value c

instance Arbitrary Ascii where
  arbitrary = do
    c :: Char <- elements $ concat
      [ ['a'..'z'], ['A'..'Z'], ['0'..'9']
      , "`~!@#$%^&*()-_=+\\|]}[{'\";:/?.>,<"
      ]
    return $ Ascii (fromChar c)





newtype NonNewline a = NonNewline
  { unNonNewline :: a
  } deriving Eq

instance
  ( Show a
  ) => Show (NonNewline a)
  where
    show (NonNewline a) = show a

instance
  ( IsWidth w, IsTab t, Valued (MeasureText w t) a
  ) => Valued (MeasureText w t) (NonNewline a)
  where
    value (NonNewline a) = value a

instance
  ( IsGlyph a
  ) => IsGlyph (NonNewline a)
  where
    fromGlyph = NonNewline . fromGlyph
    toGlyph = toGlyph . unNonNewline

instance
  ( IsChar a
  ) => IsChar (NonNewline a)
  where
    fromChar = NonNewline . fromChar
    toChar = toChar . unNonNewline

instance
  ( Arbitrary a, IsChar a
  ) => Arbitrary (NonNewline a)
  where
    arbitrary = do
      c <- arbitrary `suchThat` (\x -> '\n' /= toChar x)
      return $ NonNewline c
