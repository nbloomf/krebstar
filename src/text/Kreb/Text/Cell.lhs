> {-# LANGUAGE
>     MultiParamTypeClasses
>   , FlexibleContexts
> #-}

> module Kreb.Text.Cell where

> import Data.Foldable

> import Kreb.Check
> 
> import Kreb.Struct.Valued

> import Kreb.Text.Rune
> import Kreb.Text.ScreenOffset
> import Kreb.Text.MeasureText

> data Cell a
>   = Cell a
>   | EOF
>   deriving Eq
> 
> listCell :: Cell a -> [a]
> listCell x = case x of
>   Cell a -> [a]
>   EOF -> []
> 
> instance Functor Cell where
>   fmap f x = case x of
>     Cell c -> Cell (f c)
>     EOF -> EOF
> 
> instance
>   ( Arb a, IsChar a
>   ) => Arb (Cell a)
>   where
>     arb = cell <$> arb
> 
> instance Prune (Cell a) where
>   prune _ = []
> 
> instance
>   ( IsChar a
>   ) => Show (Cell a)
>   where
>     show x = case x of
>       Cell c -> concat
>         [ "(fromChar ", show (toChar c), ")" ]
>       EOF -> "eof"
> 
> cell :: (IsChar a) => Char -> Cell a
> cell = Cell . fromChar
> 
> eof :: Cell a
> eof = EOF
> 
> unCell :: Cell a -> Maybe a
> unCell x = case x of
>   Cell c -> Just c
>   EOF -> Nothing
> 
> unCells :: [Cell a] -> [a]
> unCells xs = case xs of
>   [] -> []
>   c:cs -> case c of
>     Cell a -> a : unCells cs
>     EOF -> unCells cs
> 
> instance
>   ( IsChar a
>   ) => IsChar (Cell a)
>   where
>     toChar x = case x of
>       Cell c -> toChar c
>       EOF -> '@'
>     fromChar = Cell . fromChar
> 
> instance
>   ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a
>   ) => Valued (MeasureText w t d) (Cell a)
>   where
>     value x = case x of
>       Cell a -> value a
>       EOF -> MeasureText
>         { charCount          = 0
>         , byteCount          = 0
>         , logicalOffset      = LineCol 0 0
>         , logicalCoords      = LineCol 0 0
>         , screenOffset       = mkNoNewlines [(1,Fixed1)]
>         , screenCoords       = mkNoNewlines []
>         , hasEOF             = True
>         , runeId             = Supremum
>         }
