> {-# LANGUAGE OverloadedStrings #-}

> module Kreb.Text.Pigment (
>     Pigment(NoColor)
>   , pigmentToWord8
>   , pigmentToRGB24
> 
>   , dullBlack
>   , dullRed
>   , dullGreen
>   , dullYellow
>   , dullBlue
>   , dullMagenta
>   , dullCyan
>   , dullWhite
>   , vividBlack
>   , vividRed
>   , vividGreen
>   , vividYellow
>   , vividBlue
>   , vividMagenta
>   , vividCyan
>   , vividWhite
> 
>   , rgb6
>   , gray24
> ) where

> import Data.Word
> 
> import qualified Kreb.Format as Fmt
> import           Kreb.Format ((<+>), display)
> import           Kreb.Prop

> data Pigment
>   = NoColor
>   | Ansi16 Bool Bool Bool Bool
>   | RGB6 Nat6 Nat6 Nat6
>   | Gray24 Nat24
>   deriving (Eq, Ord, Show)
> 
> instance Fmt.Display Pigment where
>   display x = case x of
>     NoColor -> "NoColor"
>     Ansi16 r g b a -> "Ansi16"
>       <+> display r <+> display g <+> display b <+> display a
>     RGB6 r g b -> "RGB6"
>       <+> display r <+> display g <+> display b
>     Gray24 g -> "Gray24"
>       <+> display g
> 
> instance Arb Pigment where
>   arb = pickFrom4
>     ( pure NoColor
>     , Ansi16 <$> arb <*> arb <*> arb <*> arb
>     , RGB6 <$> arb <*> arb <*> arb
>     , Gray24 <$> arb
>     )
> 
> instance Prune Pigment where
>   prune x = case x of
>     NoColor -> []
>     _ -> [NoColor]

> pigmentToWord8
>   :: Pigment -> Word8
> pigmentToWord8 x = case x of
>   NoColor -> 0x00
>   Ansi16 lum hueR hueG hueB -> case lum of
>     False -> case (hueR, hueG, hueB) of
>       (False, False, False) -> 0x00 -- dull black
>       (True,  False, False) -> 0x01 -- dull red
>       (False, True,  False) -> 0x02 -- dull green
>       (True,  True,  False) -> 0x03 -- dull yellow
>       (False, False, True ) -> 0x04 -- dull blue
>       (True,  False, True ) -> 0x05 -- dull magenta
>       (False, True,  True ) -> 0x06 -- dull cyan
>       (True,  True,  True ) -> 0x07 -- dull white
>     True -> case (hueR, hueG, hueB) of
>       (False, False, False) -> 0x08 -- vivid black
>       (True,  False, False) -> 0x09 -- vivid red
>       (False, True,  False) -> 0x0a -- vivid green
>       (True,  True,  False) -> 0x0b -- vivid yellow
>       (False, False, True ) -> 0x0c -- vivid blue
>       (True,  False, True ) -> 0x0d -- vivid magenta
>       (False, True,  True ) -> 0x0e -- vivid cyan
>       (True,  True,  True ) -> 0x0f -- vivid white
>   RGB6 r g b -> fromIntegral $
>     36*(nat6ToInt r) + 6*(nat6ToInt g) + (nat6ToInt b) + 16
>   Gray24 m -> fromIntegral $
>     (nat24ToInt m) + 232

> pigmentToRGB24
>   :: Pigment -> (Word8, Word8, Word8)
> pigmentToRGB24 x = case x of
>   NoColor -> (0x00, 0x00, 0x00)
>   Ansi16 lum hueR hueG hueB -> case lum of
>     False -> case (hueR, hueG, hueB) of
>       (False, False, False) -> (0x00, 0x00, 0x00) -- dull black
>       (True,  False, False) -> (0x80, 0x00, 0x00) -- dull red
>       (False, True,  False) -> (0x00, 0x80, 0x00) -- dull green
>       (True,  True,  False) -> (0x80, 0x80, 0x00) -- dull yellow
>       (False, False, True ) -> (0x00, 0x00, 0x80) -- dull blue
>       (True,  False, True ) -> (0x80, 0x00, 0x80) -- dull magenta
>       (False, True,  True ) -> (0x00, 0x80, 0x80) -- dull cyan
>       (True,  True,  True ) -> (0xc0, 0xc0, 0xc0) -- dull white
>     True -> case (hueR, hueG, hueB) of
>       (False, False, False) -> (0x80, 0x80, 0x80) -- vivid black
>       (True,  False, False) -> (0xff, 0x00, 0x00) -- vivid red
>       (False, True,  False) -> (0x00, 0xff, 0x00) -- vivid green
>       (True,  True,  False) -> (0xff, 0xff, 0x00) -- vivid yellow
>       (False, False, True ) -> (0x00, 0x00, 0xff) -- vivid blue
>       (True,  False, True ) -> (0xff, 0x00, 0xff) -- vivid magenta
>       (False, True,  True ) -> (0x00, 0xff, 0xff) -- vivid cyan
>       (True,  True,  True ) -> (0xff, 0xff, 0xff) -- vivid white
>   RGB6 r g b ->
>     (nat6ToWord8 r, nat6ToWord8 g, nat6ToWord8 b)
>   Gray24 m ->
>     (nat24ToWord8 m, nat24ToWord8 m, nat24ToWord8 m)

> data Nat6
>   = N0 | N1 | N2 | N3 | N4 | N5
>   deriving (Eq, Ord, Show, Enum)
> 
> instance Fmt.Display Nat6 where
>   display x = case x of
>     N0 -> "N0"
>     N1 -> "N1"
>     N2 -> "N2"
>     N3 -> "N3"
>     N4 -> "N4"
>     N5 -> "N5"
> 
> nat6ToWord8
>   :: Nat6 -> Word8
> nat6ToWord8 x = case x of
>   N0 -> 0x00; N1 -> 0x5f; N2 -> 0x87
>   N3 -> 0xaf; N4 -> 0xd7; N5 -> 0xff
> 
> nat6ToInt
>   :: Nat6 -> Int
> nat6ToInt x = case x of
>   N0 -> 0; N1 -> 1; N2 -> 2
>   N3 -> 3; N4 -> 4; N5 -> 5
> 
> integralToNat6
>   :: ( Integral n ) => n -> Nat6
> integralToNat6 n = case n `mod` 6 of
>   0 -> N0; 1 -> N1; 2 -> N2
>   3 -> N3; 4 -> N4; _ -> N5
> 
> instance Arb Nat6 where
>   arb = oneFrom [N0, N1, N2, N3, N4, N5]
> 
> instance Prune Nat6 where
>   prune x = case x of
>     N0 -> [];           N1 -> [ N0 ];       N2 -> [ N0, N1 ]
>     N3 -> [ N0 .. N2 ]; N4 -> [ N0 .. N3 ]; N5 -> [ N0 .. N4 ]

> data Nat24
>   = M00 | M01 | M02 | M03 | M04 | M05
>   | M06 | M07 | M08 | M09 | M10 | M11
>   | M12 | M13 | M14 | M15 | M16 | M17
>   | M18 | M19 | M20 | M21 | M22 | M23
>   deriving (Eq, Ord, Show, Enum)

> instance Fmt.Display Nat24 where
>   display x = case x of
>     M00 -> "M00"; M01 -> "M01"; M02 -> "M02"; M03 -> "M03"
>     M04 -> "M04"; M05 -> "M05"; M06 -> "M06"; M07 -> "M07"
>     M08 -> "M08"; M09 -> "M09"; M10 -> "M10"; M11 -> "M11"
>     M12 -> "M12"; M13 -> "M13"; M14 -> "M14"; M15 -> "M15"
>     M16 -> "M16"; M17 -> "M17"; M18 -> "M18"; M19 -> "M19"
>     M20 -> "M20"; M21 -> "M21"; M22 -> "M22"; M23 -> "M23"
> 
> nat24ToWord8
>   :: Nat24 -> Word8
> nat24ToWord8 x = case x of
>   M00 -> 0x08; M01 -> 0x12; M02 -> 0x1c; M03 -> 0x26;
>   M04 -> 0x30; M05 -> 0x3a; M06 -> 0x44; M07 -> 0x4e;
>   M08 -> 0x58; M09 -> 0x60; M10 -> 0x66; M11 -> 0x76;
>   M12 -> 0x80; M13 -> 0x8a; M14 -> 0x94; M15 -> 0x9e;
>   M16 -> 0xa8; M17 -> 0xb2; M18 -> 0xbc; M19 -> 0xc6;
>   M20 -> 0xd0; M21 -> 0xda; M22 -> 0xe4; M23 -> 0xee;
> 
> nat24ToInt
>   :: Nat24 -> Int
> nat24ToInt x = case x of
>   M00 -> 0;  M01 -> 1;  M02 -> 2;  M03 -> 3;
>   M04 -> 4;  M05 -> 5;  M06 -> 6;  M07 -> 7;
>   M08 -> 8;  M09 -> 9;  M10 -> 10; M11 -> 11;
>   M12 -> 12; M13 -> 13; M14 -> 14; M15 -> 15;
>   M16 -> 16; M17 -> 17; M18 -> 18; M19 -> 19;
>   M20 -> 20; M21 -> 21; M22 -> 22; M23 -> 23;
> 
> integralToNat24
>   :: ( Integral n ) => n -> Nat24
> integralToNat24 n = case n `mod` 24 of
>   0  -> M00; 1  -> M01; 2  -> M02; 3  -> M03
>   4  -> M04; 5  -> M05; 6  -> M06; 7  -> M07
>   8  -> M08; 9  -> M09; 10 -> M10; 11 -> M11
>   12 -> M12; 13 -> M13; 14 -> M14; 15 -> M15
>   16 -> M16; 17 -> M17; 18 -> M18; 19 -> M19
>   20 -> M20; 21 -> M21; 22 -> M22; 23 -> M23
> 
> instance Arb Nat24 where
>   arb = oneFrom
>     [ M00, M01, M02, M03, M04, M05, M06, M07
>     , M08, M09, M10, M11, M12, M13, M14, M16
>     , M16, M17, M18, M19, M20, M21, M22, M23
>     ]
> 
> instance Prune Nat24 where
>   prune x = case x of
>     M00 -> [];             M01 -> [ M00 ]
>     M02 -> [ M00, M01 ];   M03 -> [ M00 .. M02 ]
>     M04 -> [ M00 .. M03 ]; M05 -> [ M00 .. M04 ]
>     M06 -> [ M00 .. M05 ]; M07 -> [ M00 .. M06 ]
>     M08 -> [ M00 .. M07 ]; M09 -> [ M00 .. M08 ]
>     M10 -> [ M00 .. M09 ]; M11 -> [ M00 .. M10 ]
>     M12 -> [ M00 .. M11 ]; M13 -> [ M00 .. M12 ]
>     M14 -> [ M00 .. M13 ]; M15 -> [ M00 .. M14 ]
>     M16 -> [ M00 .. M15 ]; M17 -> [ M00 .. M16 ]
>     M18 -> [ M00 .. M17 ]; M19 -> [ M00 .. M18 ]
>     M20 -> [ M00 .. M19 ]; M21 -> [ M00 .. M20 ]
>     M22 -> [ M00 .. M21 ]; M23 -> [ M00 .. M22 ]


> dullBlack :: Pigment
> dullBlack = Ansi16 False False False False
> 
> dullRed :: Pigment
> dullRed = Ansi16 False True False False
> 
> dullGreen :: Pigment
> dullGreen = Ansi16 False False True False
> 
> dullYellow :: Pigment
> dullYellow = Ansi16 False True True False
> 
> dullBlue :: Pigment
> dullBlue = Ansi16 False False False True
> 
> dullMagenta :: Pigment
> dullMagenta = Ansi16 False True False True
> 
> dullCyan :: Pigment
> dullCyan = Ansi16 False False True True
> 
> dullWhite :: Pigment
> dullWhite = Ansi16 False True True True

> vividBlack :: Pigment
> vividBlack = Ansi16 True False False False
> 
> vividRed :: Pigment
> vividRed = Ansi16 True True False False
> 
> vividGreen :: Pigment
> vividGreen = Ansi16 True False True False
> 
> vividYellow :: Pigment
> vividYellow = Ansi16 True True True False
> 
> vividBlue :: Pigment
> vividBlue = Ansi16 True False False True
> 
> vividMagenta :: Pigment
> vividMagenta = Ansi16 True True False True
> 
> vividCyan :: Pigment
> vividCyan = Ansi16 True False True True
> 
> vividWhite :: Pigment
> vividWhite = Ansi16 True True True True



> rgb6
>   :: ( Integral n )
>   => n -> n -> n -> Pigment
> rgb6 r g b = RGB6
>   (integralToNat6 r)
>   (integralToNat6 g)
>   (integralToNat6 b)

> gray24
>   :: ( Integral n )
>   => n -> Pigment
> gray24 m = Gray24
>   (integralToNat24 m)
