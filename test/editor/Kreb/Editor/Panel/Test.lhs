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

> dimGrid :: [[Char]] -> [[Rune]]
> dimGrid = map (map dimRune)

> line :: [Char] -> [Rune]
> line = map dimRune

> prop_Panel_render_examples
>   :: ( (Int, Int), Int, [PanelAction] )
>   -> RenderedPanel
>   -> Check
> prop_Panel_render_examples (dim, tab, acts) render =
>   let
>     Just x = renderedPanel $ updateRenderedPanel
>       defaultBufferRenderSettings defaultGlyphRenderSettings NormalMode dim tab $
>       alterPanel acts (initPanel "fakestdlib" dim tab)
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
>           , labelSep = dimGrid ["│", "│", "│"]
>           , textLines = grid [" "," "," "]
>           , histSep = dimGrid ["│", "├", "│"]
>           , histLines = grid [ " " ]
>           , cmdSep = line ['─']
>           , cmdLines = grid [ " " ]
>           , statusSep = line "══╧═╧═"
>           , statusLine = grid [ "NOR   " ]
>           }
> 
>     , testKreb "#2" $
>         prop_Panel_render_examples
>         ( (6, 5), 2
>         , [ PanelAlterText [TextBoxInsert (mkGlyph 'a')] ]
>         ) $
>         RenderedPanel
>           { lineLabels = [Just 0, Nothing, Nothing]
>           , labelSep = dimGrid ["│", "│", "│"]
>           , textLines = grid ["a"," "," "]
>           , histSep = dimGrid ["│", "├", "│"]
>           , histLines = grid [ " " ]
>           , cmdSep = line ['─']
>           , cmdLines = grid [ " " ]
>           , statusSep = line "══╧═╧═"
>           , statusLine = grid [ "NOR   " ]
>           }
> 
>     , testKreb "#3" $
>         prop_Panel_render_examples
>         ( (6, 5), 2
>         , [ PanelAlterCmd [TextBoxInsert (mkGlyph 'a')] ]
>         ) $
>         RenderedPanel
>           { lineLabels = [Just 0, Nothing, Nothing]
>           , labelSep = dimGrid ["│", "│", "│"]
>           , textLines = grid
>               [ " ", " ", " " ]
>           , histSep = dimGrid ["│", "├", "│"]
>           , histLines = grid
>               [ " " ]
>           , cmdSep = line ['─']
>           , cmdLines = grid [ " " ]
>           , statusSep = line "══╧═╧═"
>           , statusLine = grid [ "NOR   " ]
>           }
> 
>     , testKreb "#4" $
>         prop_Panel_render_examples
>         ( (16, 5), 4
>         , [ PanelAlterText
>             [ TextBoxInsert (mkGlyph '\t')
>             , TextBoxInsert (mkGlyph 'b')
>             , TextBoxCursorLeft
>             , TextBoxCursorLeft
>             , TextBoxInsert (mkGlyph 'a')]
>             ]
>         ) $
>         RenderedPanel
>           { lineLabels = [Just 0, Nothing, Nothing]
>           , labelSep = dimGrid ["│", "│", "│"]
>           , textLines = grid
>               [ "a   b"
>               , "     "
>               , "     "
>               ]
>           , histSep = dimGrid ["│", "├", "│"]
>           , histLines = grid
>               [ "       " ]
>           , cmdSep = line "───────"
>           , cmdLines = grid [ "       " ]
>           , statusSep = line "══╧═════╧═══════"
>           , statusLine = grid [ "NOR             " ]
>           }
>     ]
