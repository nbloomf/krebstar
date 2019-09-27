module Kreb.Editor.CLI.Render (
    imageAppState
) where

import Graphics.Vty

import Kreb.Text
import Kreb.Editor



imageAppState
  :: ( Monad m ) => AppState m -> Image
imageAppState st =
  let
    (w,_) = windowDim st
  in case getActiveTab $ tabbedBuffers st of
    Nothing -> char defAttr ' '
    Just t -> imageTiles w t

imageTiles
  :: Int -> Panel -> Image
imageTiles w panel =
  case renderedPanel panel of
    Nothing -> char defAttr ' '
    Just rp -> imageRenderedPanel w rp

drawCell :: Rune -> Image
drawCell (Rune c fore back) =
  string (withForeColor (withBackColor defAttr (getColor back)) (getColor fore)) c

getColor :: RuneColor -> Color
getColor (RuneColor hue brightness) = case (hue, brightness) of
  (HueBlack,   BrightnessDull)  -> black
  (HueRed,     BrightnessDull)  -> red
  (HueGreen,   BrightnessDull)  -> green
  (HueYellow,  BrightnessDull)  -> yellow
  (HueBlue,    BrightnessDull)  -> blue
  (HueMagenta, BrightnessDull)  -> magenta
  (HueCyan,    BrightnessDull)  -> cyan
  (HueWhite,   BrightnessDull)  -> white
  (HueBlack,   BrightnessVivid) -> brightBlack
  (HueRed,     BrightnessVivid) -> brightRed
  (HueGreen,   BrightnessVivid) -> brightGreen
  (HueYellow,  BrightnessVivid) -> brightYellow
  (HueBlue,    BrightnessVivid) -> brightBlue
  (HueMagenta, BrightnessVivid) -> brightMagenta
  (HueCyan,    BrightnessVivid) -> brightCyan
  (HueWhite,   BrightnessVivid) -> brightWhite

imageRenderedPanel
  :: Int -> RenderedPanel -> Image
imageRenderedPanel w rp =
  let
    labels = case lineLabels rp of
      [] -> string defAttr "  "
      x:xs -> vertCat $ (l' x) : (map l xs)
        where
          l' x = case x of
            Just k -> string defAttr $ show k ++ " "
            Nothing -> string defAttr "  "
          l x = case x of
            Just k -> string defAttr $ show k
            Nothing -> char defAttr ' '
    vsep = vertCat $ map (horizCat . map drawCell) $ labelSep rp
    ssep = vertCat $ map (horizCat . map drawCell) $ histSep rp
    hsep = horizCat $ map drawCell $ cmdSep rp
    tsep = horizCat $ map drawCell $ statusSep rp

    text = vertCat $ map (horizCat . map drawCell) (textLines rp)
    cmd  = vertCat $ map (horizCat . map drawCell) (cmdLines rp)
    hist = vertCat $ map (horizCat . map drawCell) (histLines rp)
    stat = vertCat $ map (horizCat . map drawCell) (statusLine rp)
  in
    vertCat
      [ horizCat [ labels, vsep, text, ssep, vertCat [ hist, hsep, cmd ] ]
      , tsep, stat
      ]
