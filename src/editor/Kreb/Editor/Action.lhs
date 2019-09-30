> module Kreb.Editor.Action (
>     Action(..)
>   , Then(..)
>   , performAction
>   , runtimeStateIO
>   , editorTypes
> ) where

> import Kreb.Text

> import Kreb.Editor.State
> import Kreb.Editor.Error
> import Kreb.Editor.Panel
> import Kreb.Lang



> data Then
>   = GoOn
>   | Stop
>   | Bail AppError
>   deriving (Eq, Show)



> data Action
>   -- Nothing
>   = NoOp
>   | Quit

>   -- Meta
>   | SetMode EditorMode

>   -- Cursor Movement
>   | CursorUp
>   | CursorDown
>   | CursorRight
>   | CursorLeft

>   -- Editing
>   | CharInsert Char
>   | CharBackspace

>   -- Load and Save
>   | FileSaveAs
>   | FileSave
>   | FileLoad FilePath




>   | CursorLineStart
>   | CursorLineEnd

>   | CursorDocStart
>   | CursorDocEnd

>   -- Selection Management
>   | SelectionMark
>   | SelectionUnmark

>   | SelectionCut
>   | SelectionCopy
>   | SelectionPaste

>   -- Edit Operations
>   | CharOverwrite Char
>   | CharDelete


>   | LineDelete

>   | CharInsertCmd Char
>   | CharBackspaceCmd

>   | RunCmd



>   | ShowDebug String

>   | WindowResize (Int, Int)
>   deriving (Eq, Show)



> performAction
>   :: ( Monad m )
>   => Action -> AppState m
>   -> m (Then, AppState m)
> performAction act st = case act of
>   NoOp ->
>     return (GoOn, st)

>   ShowDebug msg -> do
>     let st' = alterActivePanel (showDebugMessage msg) st
>     return (GoOn, st')

>   Quit ->
>     return (Stop, st)

>   SetMode mode -> do
>     let st' = setEditorMode mode st
>     return (GoOn, st')

>   CharInsert c -> do
>     let
>       st' = alterActivePanel (alterPanel
>         [PanelAlterText [TextBoxInsert (fromChar c)]]) st
>     return (GoOn, st')

>   CharInsertCmd c -> do
>     let
>       st' = alterActivePanel (alterPanel
>         [PanelAlterCmd [TextBoxInsert (fromChar c)]]) st
>     return (GoOn, st')

>   CharBackspace -> do
>     let
>       st' = alterActivePanel (alterPanel
>         [PanelAlterText [TextBoxBackspace]]) st
>     return (GoOn, st')

>   CharBackspaceCmd -> do
>     let
>       st' = alterActivePanel (alterPanel
>         [PanelAlterCmd [TextBoxBackspace]]) st
>     return (GoOn, st')

>   CursorUp -> do
>     let
>       st' = alterActivePanel (alterPanel
>         [PanelAlterText [TextBoxCursorUp]]) st
>     return (GoOn, st')

>   CursorDown -> do
>     let
>       st' = alterActivePanel (alterPanel
>         [PanelAlterText [TextBoxCursorDown]]) st
>     return (GoOn, st')

>   CursorRight -> do
>     let
>       st' = alterActivePanel (alterPanel
>         [PanelAlterText [TextBoxCursorRight]]) st
>     return (GoOn, st')

>   CursorLeft -> do
>     let
>       st' = alterActivePanel (alterPanel
>         [PanelAlterText [TextBoxCursorLeft]]) st
>     return (GoOn, st')

>   WindowResize (w,h) -> do
>     let
>       st' = setWindowDim (w,h) st
>     return (GoOn, st')

>   RunCmd -> do
>     let
>       cmd = queryActivePanel getPanelCmdString st
>       st2 = alterActivePanel (alterPanel
>         [PanelClearCmd]) st
>     case cmd of
>       Nothing -> do
>         let st3 = alterActivePanel (showDebugMessage "no command") st2
>         return (GoOn, st2)
>       Just str -> do
>         r <- evalHook str st2
>         case r of
>           Left err -> do
>             let st3 = alterActivePanel (showDebugMessage $ show err) st2
>             return (GoOn, st3)
>           Right st' -> return (GoOn, st')

>   act -> do
>     let
>       st' = alterActivePanel
>         (showDebugMessage $ " Not implemented: " ++ show act) st
>     return (GoOn, st')





> runHook
>   :: ( Monad m )
>   => AppState m -> Hook m a -> m (a, AppState m)
> runHook st (Hook x) = x st

> evalHook
>   :: ( Monad m )
>   => String -> AppState m
>   -> m (Either (Either Error ReplError) (AppState m))
> evalHook str st = case str of
>   ':':'t':' ':rest -> case runParser pPhrase rest of
>     Left err -> return $ Left (Left err)
>     Right ph -> do
>       (r, x) <- runHook st $ evalRuntime (inferType ph) (runtimeSt st)
>       case r of
>         Left err -> return $ Left (Right err)
>         Right tp -> return $ Right $
>           alterActivePanel (updateHistory (TypeQuery str tp)) x
>   _ -> case runParser pPhrase str of
>     Left err -> return $ Left (Left err)
>     Right ph -> do
>       (r, x) <- runHook st $ evalRuntime (doActionFor ph) (runtimeSt st)
>       case r of
>         Left err -> return $ Left (Right err)
>         Right () -> return $ Right $
>           alterActivePanel (updateHistory (RunCommand ph)) x


> runtimeStateIO :: RuntimeState (Hook IO)
> runtimeStateIO =
>   initRuntimeState editorActionsIO editorTypes

> editorTypes
>   :: String -> Maybe Scheme
> editorTypes str = case str of
>   "#cursor_left" -> Just $
>     ForAll (Vars [V "S"] []) $ Arrow
>       (Stack (V "S") [])
>       (Stack (V "S") [])
>   "#cursor_right" -> Just $
>     ForAll (Vars [V "S"] []) $ Arrow
>       (Stack (V "S") [])
>       (Stack (V "S") [])
>   "#cursor_up" -> Just $
>     ForAll (Vars [V "S"] []) $ Arrow
>       (Stack (V "S") [])
>       (Stack (V "S") [])
>   "#cursor_down" -> Just $
>     ForAll (Vars [V "S"] []) $ Arrow
>       (Stack (V "S") [])
>       (Stack (V "S") [])
> 
>   "#set_path" -> Just $
>     ForAll (Vars [V "S"] []) $ Arrow
>       (Stack (V "S") [])
>       (Stack (V "S") [])
>   "#load" -> Just $
>     ForAll (Vars [V "S"] []) $ Arrow
>       (Stack (V "S") [TyCon $ C "@Eff"])
>       (Stack (V "S") [TyCon $ C "@Eff"])

>   _ -> Nothing

> editorActionsIO
>   :: String -> Maybe (Runtime (Hook IO) ())
> editorActionsIO str = case str of
>   "#cursor_left" -> Just $ Runtime $ \rts -> Hook $ \st -> do
>     (_, st') <- performAction CursorLeft st
>     return (Right ((), rts), st')
>   "#cursor_right" -> Just $ Runtime $ \rts -> Hook $ \st -> do
>     (_, st') <- performAction CursorRight st
>     return (Right ((), rts), st')
>   "#cursor_up" -> Just $ Runtime $ \rts -> Hook $ \st -> do
>     (_, st') <- performAction CursorUp st
>     return (Right ((), rts), st')
>   "#cursor_down" -> Just $ Runtime $ \rts -> Hook $ \st -> do
>     (_, st') <- performAction CursorDown st
>     return (Right ((), rts), st')
>   _ -> Nothing
