module Ned.App.IO.Render (
    imageAppState
) where

import Graphics.Vty

import Ned.App
import Ned.Data
import Ned.App.State



imageAppState
  :: ( Monad m ) => AppState m -> Image
imageAppState st =
  let (w,_) = windowDim st in
  case getActiveTab $ tabbedBuffers st of
    Nothing -> char defAttr ' '
    Just t -> case renderedStatusBar $ statusBar st of
      Nothing -> imageTiles w (panels t)
      Just rs ->
        vertCat
          [ imageTiles w (panels t)
          , string defAttr $ replicate w '░'
          , imageRenderedStatusBar rs
          ]

imageTiles
  :: Int -> Tiled Panel -> Image
imageTiles w (Tiled panel) =
  case renderedPanel panel of
    Nothing -> char defAttr ' '
    Just rp -> imageRenderedPanel w rp

imageRenderedStatusBar
  :: RenderedStatusBar -> Image
imageRenderedStatusBar rs =
  horizCat
    [ string defAttr $ statusbarMode rs
    , string defAttr $ statusbarError rs
    ]

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
    vsep = vertCat $ map (string defAttr) $ labelSep rp
    hsep = horizCat $ map (char defAttr) $ cmdSep rp

    text = vertCat $ map (resize w 1 . string defAttr . map toChar) (textLines rp)
    cmd  = vertCat $ map (string defAttr . map toChar) (cmdLines rp)
  in
    vertCat
      [ horizCat [ labels, vsep, text ]
      , hsep, cmd
      ]
