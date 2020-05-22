> module Kreb.Text.TextBox.Test.Unit where

> import Test.Tasty

> import Kreb.Unit

> import Kreb.Text.TextBox

> test_TextBox_unit :: TestTree
> test_TextBox_unit = testGroup "TextBox (unit tests)"
>   [ testGroup "Unit Tests"
>     [ test_TextBox_units_height
>     , test_TextBox_units_insert
>     , test_TextBox_units_insertMany
>     , test_TextBox_units_backspace
>     , test_TextBox_units_cursorLeft
>     , test_TextBox_units_cursorRight
>     , test_TextBox_units_cursorUp
>     , test_TextBox_units_cursorDown
>     , test_TextBox_units_resize
>     , test_TextBox_units_clear
>     ]
>   ]

> test_TextBox_units_height :: TestTree
> test_TextBox_units_height =
>   testGroup "getHeight/setHeight"
>     [ krebUnit "getHeight" $ do
>         let
>           inputs =
>             [ 2, 3, 4, 5 ]
>           test ht1 = do
>             (ht2,_,_) <- evalEditT (emptyTextBox (3,ht1) 2) $ do
>               getHeight
>             declareEqualTo ht1 ht2
>         withInputs inputs test
> 
>     , krebUnit "setHeight" $ do
>         let
>           inputs =
>             [ 2, 3, 4, 5 ]
>           test ht1 = do
>             (ht2,_,_) <- evalEditT (emptyTextBox (3,2) 2) $ do
>               setHeight ht1
>               getHeight
>             declareEqualTo ht1 ht2
>         withInputs inputs test
>     ]

> test_TextBox_units_insert :: TestTree
> test_TextBox_units_insert =
>   testGroup "textboxInsert"
>     [ krebUnit "insert one character" $ do
>         let
>           inputs =
>             [ 'a', 'b', 'c', 'd' ]
>           action c = evalEditT (emptyTextBox (3,2) 2) $ do
>             textboxInsert c
>           test c = do
>             (_,box,_) <- action c
>             let
>               actual = showTextBoxDebug box
>               expected = expectedTextBoxDebug
>                 (1,0)
>                 [ "0:" ++ [c] ++ "[F]" ]
>             declareEqualTo expected actual
>         withInputs inputs test
> 
>     , krebUnit "insert two characters" $ do
>         let
>           inputs =
>             [ ('a','a')
>             , ('a','b')
>             ]
>           action c1 c2 = evalEditT (emptyTextBox (3,2) 2) $ do
>             textboxInsert c1
>             textboxInsert c2
>           test c1 c2 = do
>             (_,box,_) <- action c1 c2
>             let
>               actual = showTextBoxDebug box
>               expected = expectedTextBoxDebug
>                 (2,0)
>                 [ "0:" ++ [c1,c2] ++ "[F]" ]
>             declareEqualTo expected actual
>         withInputs2 inputs test
> 
>     , krebUnit "insert three characters" $ do
>         let
>           inputs =
>             [ ('a','a','b')
>             , ('a','b','c')
>             ]
>           action c1 c2 c3 = evalEditT (emptyTextBox (3,2) 2) $ do
>             textboxInsert c1
>             textboxInsert c2
>             textboxInsert c3
>           test c1 c2 c3 = do
>             (_,box,_) <- action c1 c2 c3
>             let
>               actual = showTextBoxDebug box
>               expected = expectedTextBoxDebug
>                 (0,1)
>                 [ "0:" ++ [c1,c2,c3]
>                 , "-:[F]" ]
>             declareEqualTo expected actual
>         withInputs3 inputs test
> 
>     , krebUnit "insert a tab, then a non-tab" $ do
>         let
>           inputs =
>             [ ('\t','a')
>             , ('\t','b')
>             ]
>           action c1 c2 = evalEditT (emptyTextBox (3,2) 2) $ do
>             textboxInsert c1
>             textboxInsert c2
>           test c1 c2 = do
>             (_,box,_) <- action c1 c2
>             let
>               actual = showTextBoxDebug box
>               expected = expectedTextBoxDebug
>                 (0,1)
>                 [ "0:\\t" ++ [c2]
>                 , "-:[F]" ]
>             declareEqualTo expected actual
>         withInputs2 inputs test
>     ]

> test_TextBox_units_insertMany :: TestTree
> test_TextBox_units_insertMany =
>   testGroup "textboxInsertMany"
>     [ krebUnit "insert multiple lines (last line not empty)" $ do
>         let
>           inputs =
>             [ "abcdefgh"
>             , "mnopqrst" ]
>           action cs = evalEditT (emptyTextBox (3,2) 2) $ do
>             textboxInsertMany cs
>           test cs = do
>             (_,box,_) <- action cs
>             let
>               actual = showTextBoxDebug box
>               expected = expectedTextBoxDebug
>                 (2,1)
>                 [ "-:" ++ (take 3 $ drop 3 cs)
>                 , "-:" ++ (drop 6 cs) ++ "[F]" ]
>             declareEqualTo expected actual
>         withInputs inputs test
> 
>     , krebUnit "insert multiple lines (last line empty)" $ do
>         let
>           inputs =
>             [ "abcdefghi"
>             , "mnopqrstu" ]
>           action cs = evalEditT (emptyTextBox (3,2) 2) $ do
>             textboxInsertMany cs
>           test cs = do
>             (_,box,_) <- action cs
>             let
>               actual = showTextBoxDebug box
>               expected = expectedTextBoxDebug
>                 (0,1)
>                 [ "-:" ++ (drop 6 cs)
>                 , "-:[F]" ]
>             declareEqualTo expected actual
>         withInputs inputs test
> 
>     , krebUnit "insert multiple lines (different dimensions)" $ do
>         let
>           inputs =
>             [ "abcdefghi"
>             , "mnopqrstu" ]
>           action cs = evalEditT (emptyTextBox (4,1) 2) $ do
>             textboxInsertMany cs
>           test cs = do
>             (_,box,_) <- action cs
>             let
>               actual = showTextBoxDebug box
>               expected = expectedTextBoxDebug
>                 (1,0)
>                 [ "-:" ++ (drop 8 cs) ++ "[F]" ]
>             declareEqualTo expected actual
>         withInputs inputs test
>     ]

> test_TextBox_units_backspace :: TestTree
> test_TextBox_units_backspace =
>   testGroup "textboxBackspace"
>     [ krebUnit "insert no characters, then backspace" $ do
>         let
>           action = evalEditT (emptyTextBox (3,2) 2) $ do
>             textboxBackspace
>           test = do
>             (_,box,_) <- action
>             let
>               actual = showTextBoxDebug box
>               expected = expectedTextBoxDebug
>                 (0,0)
>                 [ "0:[F]" ]
>             declareEqualTo expected actual
>         test
> 
>     , krebUnit "insert two characters, then backspace" $ do
>         let
>           inputs =
>             [ ('a','b')
>             , ('x','y')
>             ]
>           action c1 c2 = evalEditT (emptyTextBox (3,2) 2) $ do
>             textboxInsertMany [c1,c2]
>             textboxBackspace
>           test c1 c2 = do
>             (_,box,_) <- action c1 c2
>             let
>               actual = showTextBoxDebug box
>               expected = expectedTextBoxDebug
>                 (1,0)
>                 [ "0:" ++ [c1] ++ "[F]" ]
>             declareEqualTo expected actual
>         withInputs2 inputs test
> 
>     , krebUnit "insert five characters, then backspace twice" $ do
>         let
>           inputs =
>             [ "abcde"
>             , "uvxyz"
>             ]
>           action cs = evalEditT (emptyTextBox (3,2) 2) $ do
>             textboxInsertMany cs
>             textboxBackspace
>             textboxBackspace
>           test cs = do
>             (_,box,_) <- action cs
>             let
>               actual = showTextBoxDebug box
>               expected = expectedTextBoxDebug
>                 (0,1)
>                 [ "0:" ++ (take 3 cs)
>                 , "-:[F]" ]
>             declareEqualTo expected actual
>         withInputs inputs test
>     ]

> test_TextBox_units_cursorLeft :: TestTree
> test_TextBox_units_cursorLeft =
>   testGroup "textboxCursorLeft"
>     [ krebUnit "insert no characters, then cursor left" $ do
>         let
>           action = evalEditT (emptyTextBox (3,2) 2) $ do
>             textboxCursorLeft
>           test = do
>             (_,box,_) <- action
>             let
>               actual = showTextBoxDebug box
>               expected = expectedTextBoxDebug
>                 (0,0)
>                 [ "0:[F]" ]
>             declareEqualTo expected actual
>         test
> 
>     , krebUnit "insert one character, then cursor left" $ do
>         let
>           inputs =
>             [ 'a', 'b', 'c', 'd', 'e' ]
>           action c = evalEditT (emptyTextBox (3,2) 2) $ do
>             textboxInsert c
>             textboxCursorLeft
>           test c = do
>             (_,box,_) <- action c
>             let
>               actual = showTextBoxDebug box
>               expected = expectedTextBoxDebug
>                 (0,0)
>                 [ "0:[F]" ++ [c] ]
>             declareEqualTo expected actual
>         withInputs inputs test
> 
>     , krebUnit "insert two characters, then cursor left" $ do
>         let
>           inputs =
>             [ ('a','b'), ('c','d'), ('1','2') ]
>           action c1 c2 = evalEditT (emptyTextBox (3,2) 2) $ do
>             textboxInsert c1
>             textboxInsert c2
>             textboxCursorLeft
>           test c1 c2 = do
>             (_,box,_) <- action c1 c2
>             let
>               actual = showTextBoxDebug box
>               expected = expectedTextBoxDebug
>                 (1,0)
>                 [ "0:" ++ [c1] ++ "[F]" ++ [c2] ]
>             declareEqualTo expected actual
>         withInputs2 inputs test
> 
>     , krebUnit "insert five characters, then cursor left twice" $ do
>         let
>           inputs =
>             [ "abcde", "12345" ]
>           action cs = evalEditT (emptyTextBox (3,1) 2) $ do
>             textboxInsertMany cs
>             textboxCursorLeft
>             textboxCursorLeft
>           test cs = do
>             (_,box,_) <- action cs
>             let
>               actual = showTextBoxDebug box
>               expected = expectedTextBoxDebug
>                 (0,0)
>                 [ "-:[F]" ++ (drop 3 cs) ]
>             declareEqualTo expected actual
>         withInputs inputs test
>     ]

> test_TextBox_units_cursorRight :: TestTree
> test_TextBox_units_cursorRight =
>   testGroup "textboxCursorRight"
>     [ krebUnit "insert no characters, then cursor right" $ do
>         let
>           action = evalEditT (emptyTextBox (3,2) 2) $ do
>             textboxCursorRight
>           test = do
>             (_,box,_) <- action
>             let
>               actual = showTextBoxDebug box
>               expected = expectedTextBoxDebug
>                 (0,0)
>                 [ "0:[F]" ]
>             declareEqualTo expected actual
>         test
> 
>     , krebUnit "insert three characters, cursor left twice, then cursor right" $ do
>         let
>           inputs =
>             [ ('a', 'b', 'c'), ('x', 'y', 'z') ]
>           action c1 c2 c3 = evalEditT (emptyTextBox (3,2) 2) $ do
>             textboxInsert c1
>             textboxInsert c2
>             textboxInsert c3
>             textboxCursorLeft
>             textboxCursorLeft
>             textboxCursorRight
>           test c1 c2 c3 = do
>             (_,box,_) <- action c1 c2 c3
>             let
>               actual = showTextBoxDebug box
>               expected = expectedTextBoxDebug
>                 (2,0)
>                 [ "0:" ++ [c1,c2] ++ "[F]" ++ [c3] ]
>             declareEqualTo expected actual
>         withInputs3 inputs test
>     ]

> test_TextBox_units_cursorUp :: TestTree
> test_TextBox_units_cursorUp =
>   testGroup "textboxCursorUp"
>     [ krebUnit "insert no characters, then cursor up" $ do
>         let
>           action = evalEditT (emptyTextBox (3,2) 2) $ do
>             textboxCursorUp
>           test = do
>             (_,box,_) <- action
>             let
>               actual = showTextBoxDebug box
>               expected = expectedTextBoxDebug
>                 (0,0)
>                 [ "0:[F]" ]
>             declareEqualTo expected actual
>         test
> 
>     , krebUnit "insert four characters, then cursor up" $ do
>         let
>           inputs =
>             [ ('a', 'b', 'c', 'd'), ('w', 'x', 'y', 'z') ]
>           action c1 c2 c3 c4 = evalEditT (emptyTextBox (3,2) 2) $ do
>             textboxInsertMany [c1,c2,c3,c4]
>             textboxCursorUp
>           test c1 c2 c3 c4 = do
>             (_,box,_) <- action c1 c2 c3 c4
>             let
>               actual = showTextBoxDebug box
>               expected = expectedTextBoxDebug
>                 (1,0)
>                 [ "0:" ++ [c1] ++ "[F]" ++ [c2,c3]
>                 , "-:" ++ [c4] ]
>             declareEqualTo expected actual
>         withInputs4 inputs test
> 
>     , krebUnit "insert three characters, then cursor up" $ do
>         let
>           inputs =
>             [ ('a', 'b'), ('w', 'x') ]
>           action c1 c2 = evalEditT (emptyTextBox (3,2) 2) $ do
>             textboxInsertMany ['\t',c1,c2]
>             textboxCursorUp
>           test c1 c2 = do
>             (_,box,_) <- action c1 c2
>             let
>               actual = showTextBoxDebug box
>               expected = expectedTextBoxDebug
>                 (0,0)
>                 [ "0:[F]\\t" ++ [c1]
>                 , "-:" ++ [c2] ]
>             declareEqualTo expected actual
>         withInputs2 inputs test
> 
>     , krebUnit "insert four characters, cursor up, then cursor left" $ do
>         let
>           inputs =
>             [ ('a', 'b', 'c', 'd'), ('w', 'x', 'y', 'z') ]
>           action c1 c2 c3 c4 = evalEditT (emptyTextBox (3,2) 2) $ do
>             textboxInsertMany [c1,c2,c3,c4]
>             textboxCursorUp
>             textboxCursorLeft
>           test c1 c2 c3 c4 = do
>             (_,box,_) <- action c1 c2 c3 c4
>             let
>               actual = showTextBoxDebug box
>               expected = expectedTextBoxDebug
>                 (0,0)
>                 [ "0:[F]" ++ [c1,c2,c3]
>                 , "-:" ++ [c4] ]
>             declareEqualTo expected actual
>         withInputs4 inputs test
> 
>     , krebUnit "insert four characters, cursor up, then cursor right" $ do
>         let
>           inputs =
>             [ ('a', 'b', 'c', 'd'), ('w', 'x', 'y', 'z') ]
>           action c1 c2 c3 c4 = evalEditT (emptyTextBox (3,2) 2) $ do
>             textboxInsertMany [c1,c2,c3,c4]
>             textboxCursorUp
>             textboxCursorRight
>           test c1 c2 c3 c4 = do
>             (_,box,_) <- action c1 c2 c3 c4
>             let
>               actual = showTextBoxDebug box
>               expected = expectedTextBoxDebug
>                 (2,0)
>                 [ "0:" ++ [c1,c2] ++ "[F]" ++ [c3]
>                 , "-:" ++ [c4] ]
>             declareEqualTo expected actual
>         withInputs4 inputs test
>     ]

> test_TextBox_units_cursorDown :: TestTree
> test_TextBox_units_cursorDown =
>   testGroup "textboxCursorDown"
>     [ krebUnit "insert no characters, then cursor down" $ do
>         let
>           action = evalEditT (emptyTextBox (3,2) 2) $ do
>             textboxCursorDown
>           test = do
>             (_,box,_) <- action
>             let
>               actual = showTextBoxDebug box
>               expected = expectedTextBoxDebug
>                 (0,0)
>                 [ "0:[F]" ]
>             declareEqualTo expected actual
>         test
> 
>     , krebUnit "insert five characters, cursor up, then cursor down" $ do
>         let
>           action = evalEditT (emptyTextBox (3,2) 2) $ do
>             textboxInsertMany "abcde"
>             textboxCursorUp
>             textboxCursorDown
>           test = do
>             (_,box,_) <- action
>             let
>               actual = showTextBoxDebug box
>               expected = expectedTextBoxDebug
>                 (2,1)
>                 [ "0:abc", "-:de[F]" ]
>             declareEqualTo expected actual
>         test
> 
>     , krebUnit "insert five characters, cursor up, left, down" $ do
>         let
>           action = evalEditT (emptyTextBox (3,2) 2) $ do
>             textboxInsertMany "abcde"
>             textboxCursorUp
>             textboxCursorLeft
>             textboxCursorDown
>           test = do
>             (_,box,_) <- action
>             let
>               actual = showTextBoxDebug box
>               expected = expectedTextBoxDebug
>                 (1,1)
>                 [ "0:abc", "-:d[F]e" ]
>             declareEqualTo expected actual
>         test
>     ]

> test_TextBox_units_resize :: TestTree
> test_TextBox_units_resize =
>   testGroup "textboxResize"
>     [ krebUnit "just resize" $ do
>         let
>           action = evalEditT (emptyTextBox (3,2) 2) $ do
>             textboxResize (4,2)
>           test = do
>             (_,box,_) <- action
>             let
>               actual = showTextBoxDebug box
>               expected = expectedTextBoxDebug
>                 (0,0)
>                 [ "0:[F]" ]
>             declareEqualTo expected actual
>         test
> 
>     , krebUnit "insert five characters, then resize" $ do
>         let
>           action = evalEditT (emptyTextBox (3,2) 2) $ do
>             textboxInsertMany "abcde"
>             textboxResize (4,2)
>           test = do
>             (_,box,_) <- action
>             let
>               actual = showTextBoxDebug box
>               expected = expectedTextBoxDebug
>                 (1,1)
>                 [ "0:abcd", "-:e[F]" ]
>             declareEqualTo expected actual
>         test
>     ]

> test_TextBox_units_clear :: TestTree
> test_TextBox_units_clear =
>   testGroup "textboxClear"
>     [ krebUnit "just clear" $ do
>         let
>           action = evalEditT (emptyTextBox (3,2) 2) $ do
>             textboxClear
>           test = do
>             (_,box,_) <- action
>             let
>               actual = showTextBoxDebug box
>               expected = expectedTextBoxDebug
>                 (0,0)
>                 [ "0:[F]" ]
>             declareEqualTo expected actual
>         test
> 
>     , krebUnit "insert characters, then clear" $ do
>         let
>           action = evalEditT (emptyTextBox (3,2) 2) $ do
>             textboxInsertMany "abcde"
>             textboxClear
>           test = do
>             (_,box,_) <- action
>             let
>               actual = showTextBoxDebug box
>               expected = expectedTextBoxDebug
>                 (0,0)
>                 [ "0:[F]" ]
>             declareEqualTo expected actual
>         test
>     ]
