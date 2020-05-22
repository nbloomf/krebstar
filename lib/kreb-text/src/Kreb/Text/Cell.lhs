> {-# LANGUAGE
>     MultiParamTypeClasses
>   , FlexibleContexts
> #-}

> module Kreb.Text.Cell where

> import Data.Foldable

> import Kreb.Prop
> 
> import Kreb.Struct.Valued

> import Kreb.Text.Rune
> import Kreb.Text.ScreenOffset
> import Kreb.Text.MeasureText
> import Kreb.Text.CursorWord

> data Cell a
>   = Cell a
>   | Focus
>   | Point
>   | MarkLeft
>   | MarkRight
>   | EOF
>   deriving Eq
> 
> listCell :: Cell a -> [a]
> listCell x = case x of
>   Cell a -> [a]
>   _ -> []
> 
> instance Functor Cell where
>   fmap f x = case x of
>     Cell c -> Cell (f c)
>     Focus -> Focus
>     Point -> Point
>     MarkLeft -> MarkLeft
>     MarkRight -> MarkRight
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
>       Focus -> "focus"
>       Point -> "point"
>       MarkLeft -> "markL"
>       MarkRight -> "markR"
> 
> cell :: (IsChar a) => Char -> Cell a
> cell = Cell . fromChar
> 
> unCell :: Cell a -> Maybe a
> unCell x = case x of
>   Cell c -> Just c
>   _ -> Nothing
> 
> unCells :: [Cell a] -> [a]
> unCells xs = case xs of
>   [] -> []
>   c:cs -> case c of
>     Cell a -> a : unCells cs
>     _ -> unCells cs
> 
> instance
>   ( IsChar a
>   ) => IsChar (Cell a)
>   where
>     toChar x = case x of
>       Cell c -> toChar c
>       _ -> '@'
>     fromChar = Cell . fromChar
