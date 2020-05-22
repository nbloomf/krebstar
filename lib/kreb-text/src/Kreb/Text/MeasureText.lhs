---
title: Kreb.Text.MeasureText
---



Contents
--------

* [Introduction](#introduction)
* [A text measurement monoid](#a-text-measurement-monoid)




Introduction
============

> {-# LANGUAGE
>     MultiParamTypeClasses
>   , ScopedTypeVariables
>   , FlexibleContexts
>   , TypeFamilies
>   , InstanceSigs, OverloadedStrings
> #-}
> 
> module Kreb.Text.MeasureText (
>     MeasureText(..)
>   , LineCol(..)
>   , Loc(..)
>   , setEventId
>   , witnessLoc
>   , newLocBetween
>   , mText

>   , measureChar
>   , EventId(..)
> 
>   , breakLineAtWidth
>   , breakLinesAtWidth
> ) where
> 
> import Data.Proxy
> 
> import qualified Kreb.Format as Fmt
> import           Kreb.Format (display, (<+>), braceList)
> import           Kreb.Prop
> import           Kreb.Arith
> import           Kreb.Struct hiding (reverse)
> 
> import Kreb.Text.ScreenOffset
> import Kreb.Text.CursorWord



A text measurement monoid
=========================

We are on the cusp of defining our core data structure for manipulating text. The last preliminary step is to give a monoid that captures the right concept of _location_ in text, to allow for efficient navigation via finger tree splitting.

We've already seen one such monoid -- `ScreenOffset`. This type models the geometric location of a character on a virtual screen. But there's another natural notion of position for characters in text, the line and column position. The `LineCol` type models offsets of this kind.

> data LineCol = LineCol
>   { lineNum :: Int
>   , colNum  :: Int
>   } deriving (Eq, Ord)
> 
> instance Fmt.Display LineCol where
>   display (LineCol l c) = "LineCol"
>     <+> display l <+> display c
> 
> instance Show LineCol where
>   show (LineCol l c) = concat
>     [ "l", show l, "c", show c ]
> 
> instance Arb LineCol where
>   arb = LineCol
>     <$> (fromNonNegative <$> arb)
>     <*> (fromNonNegative <$> arb)
> 
> instance Prune LineCol where
>   prune (LineCol l c) =
>     [ LineCol m c | m <- prune l, m >= 0 ] ++
>     [ LineCol l m | m <- prune c, m >= 0 ]

We can give a monoid instance for these offsets:

> instance Semigroup LineCol where
>   (LineCol l1 c1) <> (LineCol l2 c2) =
>     if l2 <= 0
>       then LineCol l1 (c1 + c2)
>       else LineCol (l1 + l2) c2
> 
> instance Monoid LineCol where
>   mempty = LineCol 0 0

Putting this all together we have the `MeasureText` monoid.



> data Loc d e a
>   = Loc (ZZFrac d) e a
>   deriving (Eq, Ord, Show)

> instance
>   ( Fmt.Display e, Fmt.Display a
>   ) => Fmt.Display (Loc d e a)
>   where
>     display (Loc f e a) = "Loc"
>       <+> display f
>       <+> display e
>       <+> display a

> setEventId
>   :: e -> Loc d e a -> Loc d e a
> setEventId e (Loc d _ a) = Loc d e a

> newLocBetween
>   :: ( IsBase d )
>   => e -> a -> Augmented (Loc d e a) -> Augmented (Loc d e a)
>   -> Loc d e a
> newLocBetween e a u v = case (u,v) of
>   (Infimum, Supremum) ->
>     Loc 0 e a
>   (Infimum, Augmented (Loc y _ _)) ->
>     Loc (intBelow y) e a
>   (Augmented (Loc x _ _), Supremum) ->
>     Loc (intAbove x) e a
>   (Augmented (Loc x _ _), Augmented (Loc y _ _)) ->
>     Loc (chooseBetween x y) e a

> witnessLoc
>   :: forall d1 d2 e a
>    . ( IsBase d1, IsBase d2 )
>   => Proxy d2 -> Loc d1 e a -> Loc d2 e a
> witnessLoc p (Loc (ZZFrac m k) e a) =
>   if (toBase p) == (toBase (Proxy :: Proxy d1))
>     then Loc (ZZFrac m k) e a
>     else error "panic: witnessLoc"

> data EventId
>   = EventId Integer String
>   deriving (Eq, Ord, Show)

> instance Fmt.Display EventId where
>   display (EventId k s) =
>     "EventId" <+> display k <+> display s

> instance Arb EventId where
>   arb = EventId <$> arb <*> arb
> 
> instance Prune EventId where
>   prune (EventId k m) =
>     [ EventId k' m | k' <- prune k ] ++
>     [ EventId k m' | m' <- prune m ]



> data MeasureText w t d = MeasureText
>   { charCount          :: Int
>   , byteCount          :: Int
>   , logicalOffset      :: LineCol
>   , logicalCoords      :: LineCol
>   , screenOffset       :: ScreenOffset w t
>   , screenCoords       :: ScreenOffset w t
>   , hasEOF             :: Bool
>   , runeId             :: Augmented (Loc d EventId Char)
>   , cursorData         :: CursorData
>   , cursorCount        :: Int
>   , endOnControl       :: Bool
>   } deriving (Eq, Show)
> 

> instance Fmt.Display (MeasureText w t d) where
>   display m = "MeasureText" <+> braceList
>     [ "charCount =" <+> display (charCount m)
>     , "byteCount =" <+> display (byteCount m)
>     , "logicalOffset =" <+> display (logicalOffset m)
>     , "logicalCoords =" <+> display (logicalCoords m)
>     ]

 > instance
 >   ( IsWidth w, IsTab t, IsBase d
 >   ) => Show (MeasureText w t d)
 >   where
 >     show x = concat
 >      [ "MeasureText {\n"
 >      , "  runeId: " ++ show (runeId x) ++ "\n"
 >      , "  cursorData: " ++ show (cursorData x) ++ "\n"
 >      , "}\n"
 >      ]

> instance
>   ( IsWidth w, IsTab t, IsBase d
>   ) => Arb (MeasureText w t d)
>   where
>     arb = do
>       cs <- arb :: Sample String
>       return $ mconcat $ map measureChar cs

> 
> instance
>   ( IsWidth w, IsTab t, IsBase d
>   ) => Prune (MeasureText w t d)
>   where
>     prune _ = []
> 
> mText
>   :: Int -> Int
>   -> (Int, Int) -> (Int, Int)
>   -> Bool
>   -> ScreenOffset w t
>   -> ScreenOffset w t
>   -> Loc d EventId Char
>   -> Bool
>   -> MeasureText w t d
> mText c b (lcl,lcc) (lol,loc) p sc so rid eoc = MeasureText
>   { charCount = c
>   , byteCount = b
>   , logicalOffset = LineCol lol loc
>   , logicalCoords = LineCol lcl lcc
>   , screenOffset = so
>   , screenCoords = sc
>   , hasEOF = p
>   , runeId = Augmented rid
>   , cursorData = mempty
>   , cursorCount = 0
>   , endOnControl = eoc
>   }

Note that for both logical and screen positions we maintain _offsets_ and _coordinates_ separately. Now for the monoid instance:

> instance
>   ( IsWidth w, IsTab t, IsBase d
>   ) => Semigroup (MeasureText w t d) where
>   m1 <> m2 = MeasureText
>     { charCount =
>         (charCount m1) + (charCount m2)
>     , byteCount =
>         (byteCount m1) + (byteCount m2)
>     , logicalOffset =
>         if (mempty == logicalOffset m2) && (mempty == logicalCoords m2)
>           then logicalOffset m1
>           else logicalOffset m2
>     , logicalCoords =
>         if (mempty == logicalOffset m2) && (mempty == logicalCoords m2)
>           then (logicalCoords m1)
>           else (logicalCoords m1) <> (logicalOffset m1) <> (logicalCoords m2)
>     , screenOffset =
>         if (mempty == screenOffset m2) && (mempty == screenCoords m2)
>           then screenOffset m1
>           else screenOffset m2
>     , screenCoords =
>         if (mempty == screenOffset m2) && (mempty == screenCoords m2)
>           then (screenCoords m1)
>           else (screenCoords m1) <> (screenOffset m1) <> (screenCoords m2)
>     , hasEOF = (hasEOF m1) || (hasEOF m2)
>     , runeId = max (runeId m1) (runeId m2)
>     , cursorData = (cursorData m1) <> (cursorData m2)
>     , cursorCount = (cursorCount m1) + (cursorCount m2)
>     , endOnControl = endOnControl m2
>     }
> 
> instance
>   ( IsWidth w, IsTab t, IsBase d
>   ) => Monoid (MeasureText w t d) where
>   mempty = MeasureText
>     { charCount          = 0
>     , byteCount          = 0
>     , logicalOffset      = mempty
>     , logicalCoords      = mempty
>     , screenOffset       = mempty
>     , screenCoords       = mempty
>     , hasEOF             = False
>     , runeId             = Infimum
>     , cursorData         = mempty
>     , cursorCount        = 0
>     , endOnControl       = False
>     }

And most importantly, we need a `Valued` instance for `Char` against `MeasureText`. Remember that the coordinates represent the position of the current character, while the offsets represent the relative position of the next character.

> measureChar :: ( IsWidth w, IsTab t, IsBase d ) => Char -> MeasureText w t d
> measureChar c = case c of
>   '\n' -> MeasureText
>     { charCount          = 1
>     , byteCount          = 1
>     , logicalOffset      = LineCol 1 0
>     , logicalCoords      = LineCol 0 0
>     , screenOffset       = mkWithNewlines [] 1 []
>     , screenCoords       = mkNoNewlines []
>     , hasEOF             = False
>     , runeId             = Infimum
>     , cursorData         = mempty
>     , cursorCount        = 0
>     , endOnControl       = False
>     }
>   '\t' -> MeasureText
>     { charCount          = 1
>     , byteCount          = 1
>     , logicalOffset      = LineCol 0 1
>     , logicalCoords      = LineCol 0 0
>     , screenOffset       = mkNoNewlines [(1, Stretchy)]
>     , screenCoords       = mkNoNewlines []
>     , hasEOF             = False
>     , runeId             = Infimum
>     , cursorData         = mempty
>     , cursorCount        = 0
>     , endOnControl       = False
>     }
>   _ -> MeasureText
>     { charCount          = 1
>     , byteCount          = charBytes c
>     , logicalOffset      = LineCol 0 1
>     , logicalCoords      = LineCol 0 0
>     , screenOffset       = mkNoNewlines [(1, charWidth c)]
>     , screenCoords       = mkNoNewlines []
>     , hasEOF             = False
>     , runeId             = Infimum
>     , cursorData         = mempty
>     , cursorCount        = 0
>     , endOnControl       = False
>     }


We need a utility for converting characters into spans and to byte counts:

> charWidth :: Char -> Span
> charWidth c = case c of
>   '\t' -> Stretchy
>   _ -> Fixed1
> 
> charBytes :: Char -> Int
> charBytes c = 1



Testing and debugging
---------------------

> class IsChar a where
>   toChar   :: a -> Char
>   fromChar :: Char -> a

> instance IsChar Char where
>   toChar   = id
>   fromChar = id

> breakLineAtWidth
>   :: ( IsChar a )
>   => Int -> Int -> [a]
>   -> Maybe ([a], Maybe [a])
> breakLineAtWidth tab w xs = f 0 [] xs
>   where
>     f k as bs = case bs of
>       [] -> case as of
>         [] -> Nothing
>         _  -> Just (reverse as, Nothing)
>       c:cs -> case toChar c of
>         '\n' -> if k+1 > w
>           then Just (reverse as, Just bs)
>           else case cs of
>             [] -> Just (reverse (c:as), Nothing)
>             _  -> Just (reverse (c:as), Just cs)
>         _ -> case charWidth $ toChar c of
>           Fixed0 -> f k (c:as) cs
> 
>           Fixed1 -> if k+1 > w
>             then Just (reverse as, Just bs)
>             else f (k+1) (c:as) cs
> 
>           Fixed2 -> if k+2 > w
>             then Just (reverse as, Just bs)
>             else f (k+2) (c:as) cs
> 
>           Stretchy -> if k+1 > w
>             then Just (reverse as, Just bs)
>             else let n = tab * (1 + quot k tab) in
>               if n > w
>                 then case cs of
>                   [] -> Just (reverse (c:as), Nothing)
>                   _  -> Just (reverse (c:as), Just cs)
>                 else f n (c:as) cs

> breakLinesAtWidth
>   :: ( IsChar a )
>   => Int -> Int -> [a]
>   -> [[a]]
> breakLinesAtWidth tab w xs =
>   case breakLineAtWidth tab w xs of
>     Nothing -> []
>     Just (as, Nothing) -> [as]
>     Just (as, Just bs) ->
>       as : breakLinesAtWidth tab w bs
