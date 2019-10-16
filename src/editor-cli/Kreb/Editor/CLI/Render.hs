module Kreb.Editor.CLI.Render (
    imageAppState
) where

import Graphics.Vty

import Kreb.Text
import Kreb.Editor



imageAppState
  :: ( Monad m ) => AppState m -> Picture
imageAppState st =
  case getActiveTab $ tabbedBuffers st of
    Nothing -> Picture (Cursor 0 0) [char defAttr ' '] (ClearBackground)
    Just t -> imageTiles (editorMode st) t

imageTiles
  :: EditorMode -> Panel -> Picture
imageTiles mode panel =
  case renderedPanel panel of
    Nothing -> Picture (Cursor 0 0) [char defAttr ' '] (ClearBackground)
    Just rp -> imageRenderedPanel mode rp

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
  :: EditorMode -> RenderedPanel -> Picture
imageRenderedPanel mode rp =
  let
    (labelPaneLines, (labW, _)) = lineLabels rp
    (textPaneLines, (textW, textH), (tcX, tcY)) = textLines rp
    (histPaneLines, (histW, histH)) = histLines rp
    (cmdPaneLines, (cmdW, cmdH), (ccX, ccY)) = cmdLines rp
    (statPaneLine, (statW, statH)) = statusLine rp

    labelSep = replicate textH [dimRune '│']
    histSep = concat
      [ replicate histH [dimRune '│']
      , [[dimRune '├']]
      , replicate cmdH [dimRune '│']
      ]
    statusSep  = concat
      [ replicate labW (dimRune '═')
      , [dimRune '╧']
      , replicate textW (dimRune '═')
      , [dimRune '╧']
      , replicate histW (dimRune '═')
      ]
    cmdSep = replicate histW (dimRune '─')

    vsep = vertCat $ map (horizCat . map drawCell) $ labelSep
    ssep = vertCat $ map (horizCat . map drawCell) $ histSep
    hsep = horizCat $ map drawCell $ cmdSep
    tsep = horizCat $ map drawCell $ statusSep

    labels = vertCat $ map (horizCat . map drawCell) labelPaneLines
    text = vertCat $ map (horizCat . map drawCell) textPaneLines
    cmd  = vertCat $ map (horizCat . map drawCell) cmdPaneLines
    hist = vertCat $ map (horizCat . map drawCell) histPaneLines
    stat = vertCat $ map (horizCat . map drawCell) statPaneLine

    mainPane = vertCat
      [ horizCat [ labels, vsep, text, ssep, vertCat [ hist, hsep, cmd ] ]
      , tsep, stat
      ]

    cursorPane = horizCat
      [ pad 5 5 5 5 (drawCell phantomCursorRune)
      ]

    cursor = case mode of
      CommandMode -> Cursor (labW + 1 + textW + 1 + ccX) (histH + 1)
      InsertMode -> Cursor (labW + 1 + tcX) (tcY)
      NormalMode -> NoCursor
  in Picture
    cursor
    [mainPane]
    (ClearBackground)
