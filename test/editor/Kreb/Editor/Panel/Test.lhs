> module Kreb.Editor.Panel.Test (
>     test_Panel
> ) where
> 
> import Data.Proxy
> 
> import Test.Tasty
> 
> import Kreb.Check
> import Kreb.Text
> import Kreb.Editor

> test_Panel :: TestTree
> test_Panel =
>   testGroup "Panel"
>     [ test_Panel_render_examples
>     ]

> grid :: [[Char]] -> [[Rune]]
> grid = map (map plainRune)

> prop_Panel_render_examples
>   :: ( (Int, Int), Int, [PanelAction] )
>   -> RenderedPanel
>   -> Check
> prop_Panel_render_examples (dim, tab, acts) render =
>   let
>     Just x = renderedPanel $ updateRenderedPanel
>       defaultBufferRenderSettings defaultGlyphRenderSettings dim $
>       alterPanel acts (initPanel dim tab)
>   in checkAll
>     [ claimEqualNamed "lineLabels: "
>         (lineLabels render) (lineLabels x)
>     , claimEqualNamed "labelSep: "
>         (labelSep render) (labelSep x)
>     , claimEqualNamed "textLines: "
>         (textLines render) (textLines x)
>     , claimEqualNamed "histSep: "
>         (histSep render) (histSep x)
>     , claimEqualNamed "histLines: "
>         (histLines render) (histLines x)
>     , claimEqualNamed "cmdSep: "
>         (cmdSep render) (cmdSep x)
>     , claimEqualNamed "cmdLines: "
>         (cmdLines render) (cmdLines x)
>     , claimEqualNamed "statusSep: "
>         (statusSep render) (statusSep x)
>     , claimEqualNamed "statusLine: "
>         (statusLine render) (statusLine x)
>     ]

> test_Panel_render_examples :: TestTree
> test_Panel_render_examples =
>   localOption (KrebCheckTests 1) $
>   testGroup "Panel Render Examples"
>     [ testKreb "#1" $
>         prop_Panel_render_examples
>         ( (6, 5), 2
>         , []
>         ) $
>         RenderedPanel
>           { lineLabels = [Just 0, Nothing, Nothing]
>           , labelSep = ["|", "|", "|"]
>           , textLines = grid [" "," "," "]
>           , histSep = ["|", "|", "|"]
>           , histLines = grid [ " " ]
>           , cmdSep = ['-']
>           , cmdLines = grid [ " " ]
>           , statusSep = "------"
>           , statusLine = grid [ "      " ]
>           }
> 
>     , testKreb "#2" $
>         prop_Panel_render_examples
>         ( (6, 5), 2
>         , [ PanelAlterText [TextBoxInsert (mkGlyph 'a')] ]
>         ) $
>         RenderedPanel
>           { lineLabels = [Just 0, Nothing, Nothing]
>           , labelSep = ["|", "|", "|"]
>           , textLines = grid ["a"," "," "]
>           , histSep = ["|", "|", "|"]
>           , histLines = grid [ " " ]
>           , cmdSep = ['-']
>           , cmdLines = grid [ " " ]
>           , statusSep = "------"
>           , statusLine = grid [ "      " ]
>           }
> 
>     , testKreb "#3" $
>         prop_Panel_render_examples
>         ( (6, 5), 2
>         , [ PanelAlterCmd [TextBoxInsert (mkGlyph 'a')] ]
>         ) $
>         RenderedPanel
>           { lineLabels = [Just 0, Nothing, Nothing]
>           , labelSep = ["|", "|", "|"]
>           , textLines = grid
>               [ " ", " ", " " ]
>           , histSep = ["|", "|", "|"]
>           , histLines = grid
>               [ " " ]
>           , cmdSep = ['-']
>           , cmdLines = grid [ " " ]
>           , statusSep = "------"
>           , statusLine = grid [ "      " ]
>           }
>     ]
