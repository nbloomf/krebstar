> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE InstanceSigs #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE OverloadedStrings #-}

> module Kreb.Text.Grapheme (
>     Grapheme(..)
>   , Control(..)
>   , RegionBoundary(..)
>   , resize

>   , unCell

>   , fromControls
>   , getGraphemeChar
>   , getColorfulGraphemeChar
>   , getGraphemeString

>   , makePlainGraphemeWithLoc

>   , makePlainGraphemes
>   , makePlainGraphemesL
>   , makePlainGraphemesF
>   , makePlainGraphemesLF
>   , makePlainGraphemesTF
>   , makeColorfulGraphemes
>   , makeColorfulGraphemesL
> ) where

> import qualified Kreb.Format as Fmt
> import           Kreb.Format (display, (<+>))
> import           Kreb.Prop hiding (G)
> import           Kreb.Arith
> import           Kreb.Struct
> 
> import Kreb.Text.ScreenOffset
> import Kreb.Text.MeasureText
> import Kreb.Text.CursorWord
> import Kreb.Text.Pigment

> data Grapheme w t d
>   = G (Color (Loc d EventId Char))
>   deriving (Eq, Show)

> instance Fmt.Display (Grapheme w t d) where
>   display (G x) = "G" <+> display x

> makeColorfulGraphemeWithLoc
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => Pigment -> Pigment -> Loc d EventId Char
>   -> Grapheme w t d
> makeColorfulGraphemeWithLoc fg bg loc =
>   G (Color fg bg loc)
> 
> makePlainGraphemeWithLoc
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => Loc d EventId Char
>   -> Grapheme w t d
> makePlainGraphemeWithLoc =
>   makeColorfulGraphemeWithLoc vividWhite dullBlack

> getGraphemeChar :: Grapheme w t d -> Char
> getGraphemeChar (G (Color _ _ (Loc _ _ c))) = c

> getColorfulGraphemeChar
>   :: Grapheme w t d -> (Char, Pigment, Pigment)
> getColorfulGraphemeChar (G (Color fg bg (Loc _ _ c))) =
>   (c, fg, bg)

> getGraphemeString :: Grapheme w t d -> String
> getGraphemeString x = [getGraphemeChar x]

> data Control a
>   = Cell a
>   | Focus (Maybe RegionBoundary)
>   | Point (Maybe RegionBoundary)
>   | MarkLeft
>   | MarkRight
>   deriving (Eq, Show)

> instance (Fmt.Display a) => Fmt.Display (Control a) where
>   display x = case x of
>     Cell a    -> "Cell" <+> display a
>     Focus z   -> "Focus" <+> display z
>     Point z   -> "Point" <+> display z
>     MarkLeft  -> "MarkLeft"
>     MarkRight -> "MarkRight"

> data RegionBoundary
>   = OnLeft | OnRight
>   deriving (Eq, Show)

> instance Fmt.Display RegionBoundary where
>   display x = case x of
>     OnLeft  -> "OnLeft"
>     OnRight -> "OnRight"

> unCell
>   :: Control a -> a
> unCell (Cell a) = a
> unCell _ = error "unCell: panic"

> instance Functor Control where
>   fmap f x = case x of
>     Cell a -> Cell (f a)
>     Focus b -> Focus b
>     Point b -> Point b
>     MarkLeft -> MarkLeft
>     MarkRight -> MarkRight

> fromControls
>   :: [Control a] -> [a]
> fromControls xs = case xs of
>   [] -> []
>   z:zs -> case z of
>     Cell a -> a : fromControls zs
>     _      -> fromControls zs

> data Color a
>   = Color Pigment Pigment a
>   deriving (Eq, Show)

> instance (Fmt.Display a) => Fmt.Display (Color a) where
>   display (Color fg bg a) = "Color"
>     <+> display fg <+> display bg <+> display a

> instance
>   ( IsWidth w, IsTab t, IsBase d
>   ) => Valued (Grapheme w t d)
>   where
>     type Value (Grapheme w t d) = MeasureText w t d
> 
>     value :: Grapheme w t d -> MeasureText w t d
>     value (G (Color _ _ (Loc d eId u))) =
>       let v = measureChar u :: MeasureText w t d
>       in v { runeId = Augmented (Loc d eId u) }

> instance
>   ( IsWidth w, IsTab t, IsBase d
>   ) => Valued (Control (Grapheme w t d))
>   where
>     type Value (Control (Grapheme w t d)) = MeasureText w t d
> 
>     value :: Control (Grapheme w t d) -> MeasureText w t d
>     value x = case x of
>       Cell z -> value z
>       Focus _ -> MeasureText
>         { charCount          = 0
>         , byteCount          = 0
>         , logicalOffset      = LineCol 0 0
>         , logicalCoords      = LineCol 0 0
>         , screenOffset       = mkNoNewlines []
>         , screenCoords       = mkNoNewlines []
>         , hasEOF             = False
>         , runeId             = Infimum
>         , cursorData         = CursorData True 1 0 P
>         , cursorCount        = 1
>         , endOnControl       = True
>         }
>       Point _ -> MeasureText
>         { charCount          = 0
>         , byteCount          = 0
>         , logicalOffset      = LineCol 0 0
>         , logicalCoords      = LineCol 0 0
>         , screenOffset       = mkNoNewlines []
>         , screenCoords       = mkNoNewlines []
>         , hasEOF             = False
>         , runeId             = Infimum
>         , cursorData         = CursorData False 1 0 P
>         , cursorCount        = 1
>         , endOnControl       = True
>         }
>       MarkLeft -> MeasureText
>         { charCount          = 0
>         , byteCount          = 0
>         , logicalOffset      = LineCol 0 0
>         , logicalCoords      = LineCol 0 0
>         , screenOffset       = mkNoNewlines []
>         , screenCoords       = mkNoNewlines []
>         , hasEOF             = False
>         , runeId             = Infimum
>         , cursorData         = CursorData False 0 1 L
>         , cursorCount        = 0
>         , endOnControl       = True
>         }
>       MarkRight -> MeasureText
>         { charCount          = 0
>         , byteCount          = 0
>         , logicalOffset      = LineCol 0 0
>         , logicalCoords      = LineCol 0 0
>         , screenOffset       = mkNoNewlines []
>         , screenCoords       = mkNoNewlines []
>         , hasEOF             = False
>         , runeId             = Infimum
>         , cursorData         = CursorData False 0 1 R
>         , cursorCount        = 0
>         , endOnControl       = True
>         }

> makeColorfulGrapheme
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => EventId -> Int -> Pigment -> Pigment -> Char
>   -> Grapheme w t d
> makeColorfulGrapheme eId i fg bg c = G
>   $ Color fg bg $ Loc (fromIntegral i) eId c



> makePlainGrapheme
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => EventId -> Int -> Char
>   -> Grapheme w t d
> makePlainGrapheme eId i =
>   makeColorfulGrapheme eId i vividWhite dullBlack

> makePlainGraphemesF
>   :: forall w t d f
>    . ( IsWidth w, IsTab t, IsBase d, Functor f )
>   => EventId -> [f Char] -> [f (Grapheme w t d)]
> makePlainGraphemesF eId = zipWith f [0..]
>   where
>     f :: Int -> f Char -> f (Grapheme w t d)
>     f i x = fmap (makePlainGrapheme eId i) x

> makePlainGraphemesL
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => EventId -> [[Char]] -> [[Grapheme w t d]]
> makePlainGraphemesL eId = f 0
>   where
>     f :: Int -> [[Char]] -> [[Grapheme w t d]]
>     f u = \case
>       [] -> []
>       xs:xss ->
>         let (zs,v) = g u xs
>         in zs : f v xss
> 
>     g :: Int -> [Char] -> ([Grapheme w t d], Int)
>     g k = \case
>       [] -> ([], k)
>       x:xs ->
>         let
>           z = makePlainGrapheme eId k x
>           (zs, v) = g (k+1) xs
>         in (z:zs, v)

> makePlainGraphemesLF
>   :: forall w t d f
>    . ( IsWidth w, IsTab t, IsBase d, Functor f )
>   => EventId -> [[f Char]] -> [[f (Grapheme w t d)]]
> makePlainGraphemesLF eId = f 0
>   where
>     f :: Int -> [[f Char]] -> [[f (Grapheme w t d)]]
>     f u = \case
>       [] -> []
>       xs:xss ->
>         let (zs,v) = g u xs
>         in zs : f (v+1) xss
> 
>     g :: Int -> [f Char] -> ([f (Grapheme w t d)], Int)
>     g k = \case
>       [] -> ([], k)
>       x:xs ->
>         let
>           z = fmap (makePlainGrapheme eId k) x
>           (zs, v) = g (k+1) xs
>         in (z:zs, v)

> makePlainGraphemesTF
>   :: forall w t d f
>    . ( IsWidth w, IsTab t, IsBase d, Functor f )
>   => EventId -> [f Char] -> [Char] -> [f Char]
>   -> ([f (Grapheme w t d)], [Grapheme w t d], [f (Grapheme w t d)])
> makePlainGraphemesTF eId as rs bs =
>   let
>     (as', k1) = h 0 as
>     (rs', k2) = g k1 rs
>     (bs', _)  = h k2 bs
>   in (as', rs', bs')
>   where
>     g :: Int -> [Char] -> ([Grapheme w t d], Int)
>     g k = \case
>       [] -> ([], k)
>       x:xs ->
>         let
>           z = makePlainGrapheme eId k x
>           (zs, v) = g (k+1) xs
>         in (z:zs, v)
> 
>     h :: Int -> [f Char] -> ([f (Grapheme w t d)], Int)
>     h k = \case
>       [] -> ([], k)
>       x:xs ->
>         let
>           z = fmap (makePlainGrapheme eId k) x
>           (zs, v) = h (k+1) xs
>         in (z:zs, v)


> makeColorfulGraphemes
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => EventId -> [Color Char] -> [Grapheme w t d]
> makeColorfulGraphemes eId = zipWith f [0..]
>   where
>     f :: Int -> Color Char -> Grapheme w t d
>     f i (Color fg bg x) = makeColorfulGrapheme eId i fg bg x

> makeColorfulGraphemesL
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => EventId -> [[Color Char]] -> [[Grapheme w t d]]
> makeColorfulGraphemesL eId = f 0
>   where
>     f :: Int -> [[Color Char]] -> [[Grapheme w t d]]
>     f u = \case
>       [] -> []
>       xs:xss ->
>         let (zs,v) = g u xs
>         in zs : f (v+1) xss
> 
>     g :: Int -> [Color Char] -> ([Grapheme w t d], Int)
>     g k = \case
>       [] -> ([], k)
>       (Color fg bg x):xs ->
>         let
>           z = makeColorfulGrapheme eId k fg bg x
>           (zs, v) = g (k+1) xs
>         in (z:zs, v)

> makePlainGraphemes
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => EventId -> [Char] -> [Grapheme w t d]
> makePlainGraphemes eId = zipWith f [0..]
>   where
>     f :: Int -> Char -> Grapheme w t d
>     f i x = makePlainGrapheme eId i x

> makeColorfulGraphemesF
>   :: forall w t d f
>    . ( IsWidth w, IsTab t, IsBase d, Functor f )
>   => EventId -> [f (Color Char)] -> [f (Grapheme w t d)]
> makeColorfulGraphemesF eId = zipWith f [0..]
>   where
>     f :: Int -> f (Color Char) -> f (Grapheme w t d)
>     f i x = fmap
>       (\(Color fg bg c) -> makeColorfulGrapheme eId i fg bg c) x



> resize
>   :: ( IsWidth w1, IsWidth w2, IsTab t1, IsTab t2, IsBase d )
>   => Grapheme w1 t1 d -> Grapheme w2 t2 d
> resize (G x) = G x



> instance (Arb a) => Arb (Color a) where
>   arb = Color <$> arb <*> arb <*> arb

> instance (Prune a) => Prune (Color a) where
>   prune (Color fg bg x) =
>     [ Color fg' bg x | fg' <- prune fg ] ++
>     [ Color fg bg' x | bg' <- prune bg ] ++
>     [ Color fg bg x' | x' <- prune x ]

> arbPlainGraphemeList
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => EventId -> Sample [Grapheme w t d]
> arbPlainGraphemeList eId = makePlainGraphemes eId <$> arb

> instance Prune (Grapheme w t d) where
>   prune _ = []

> instance (Prune a) => Prune (Control a) where
>   prune x = case x of
>     Cell a -> map Cell $ prune a
>     _ -> []
