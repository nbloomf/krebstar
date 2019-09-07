{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Kreb.Text.Glyph.Test (
--    IsGlyph(..)

--  , Ascii(..)
--  , charAscii
--  , pAscii

--  , NonNewline(..)
) where

import Data.Proxy

import Test.Tasty

import Kreb.Check
import Kreb.Struct
import Kreb.Text

{-

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

instance Arb Ascii where
  arb = do
    c <- arbPrintableAsciiChar
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
  ( Arb a, IsChar a
  ) => Arb (NonNewline a)
  where
    arb = do
      c <- arb `satisfying` (\x -> '\n' /= toChar x)
      return $ NonNewline c

-}
