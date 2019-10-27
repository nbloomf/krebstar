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

> grid :: [[Char]] -> [[Glyph String]]
> grid = map (map plainRune)

> dimGrid :: [[Char]] -> [[Glyph String]]
> dimGrid = map (map dimRune)

> line :: [Char] -> [Glyph String]
> line = map dimRune

> initPanelDim :: PanelDim
> initPanelDim = PanelDim
>   { _textLabelDim = (1,1)
>   , _textDim      = (1,1)
>   , _historyDim   = (1,1)
>   , _commandDim   = (1,1)
>   , _statusDim    = (1,1)
>   }

> prop_Panel_render_examples
>   :: ( (Int, Int), Int, [PanelAction] )
>   -> RenderedPanel
>   -> Check
> prop_Panel_render_examples (dim, tab, acts) render =
>   let
>     Just x = renderedPanel $ updateRenderedPanel
>       defaultGlyphRenderSettings NormalMode tab $
>       alterPanel acts (initPanel "fakestdlib" dim initPanelDim tab)
>   in claimAll
>     [ claimEqualNamed "lineLabels: "
>         (lineLabels render) (lineLabels x)
>     , claimEqualNamed "textLines: "
>         (textLines render) (textLines x)
>     , claimEqualNamed "histLines: "
>         (histLines render) (histLines x)
>     , claimEqualNamed "cmdLines: "
>         (cmdLines render) (cmdLines x)
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
>           { lineLabels = (grid ["0 ", "  ", "  "], (2,3))
>           , textLines = (grid [" "," "," "], (1,3), (1,1))
>           , histLines = (grid [ " " ], (1,1))
>           , cmdLines = (grid [ " " ], (1,1), (1,1))
>           , statusLine = (grid [ "NOR   " ], (6,1))
>           }
> 
>     , testKreb "#2" $
>         prop_Panel_render_examples
>         ( (6, 5), 2
>         , [ PanelAlterText [TextBoxInsert (plainGlyph 'a')] ]
>         ) $
>         RenderedPanel
>           { lineLabels = (grid ["0 ", "  ", "  "], (2,3))
>           , textLines = (grid ["a"," "," "], (1,3), (1,1))
>           , histLines = (grid [ " " ], (1,1))
>           , cmdLines = (grid [ " " ], (1,1), (1,1))
>           , statusLine = (grid [ "NOR   " ], (6,1))
>           }
> 
>     , testKreb "#3" $
>         prop_Panel_render_examples
>         ( (6, 5), 2
>         , [ PanelAlterCmd [TextBoxInsert (plainGlyph 'a')] ]
>         ) $
>         RenderedPanel
>           { lineLabels = (grid ["0 ", "  ", "  "], (2,3))
>           , textLines = (grid
>               [ " ", " ", " " ], (1,3), (1,1))
>           , histLines = (grid
>               [ " " ], (1,1))
>           , cmdLines = (grid [ " " ], (1,1), (1,1))
>           , statusLine = (grid [ "NOR   " ], (6,1))
>           }
> 
>     , testKreb "#4" $
>         prop_Panel_render_examples
>         ( (16, 5), 4
>         , [ PanelAlterText
>             [ TextBoxInsert (plainGlyph '\t')
>             , TextBoxInsert (plainGlyph 'b')
>             , TextBoxCursorLeft
>             , TextBoxCursorLeft
>             , TextBoxInsert (plainGlyph 'a')]
>             ]
>         ) $
>         RenderedPanel
>           { lineLabels = (grid ["0 ", "  ", "  "], (2,3))
>           , textLines = (grid
>               [ "a   b"
>               , "     "
>               , "     "
>               ], (5,3), (1,1))
>           , histLines = (grid
>               [ "       " ], (7,1))
>           , cmdLines = (grid [ "       " ], (7,1), (1,1))
>           , statusLine = (grid [ "NOR             " ], (16,1))
>           }
>     ]
