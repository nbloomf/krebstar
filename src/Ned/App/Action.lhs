> module Ned.App.Action (
>     Action(..)
>   , Then(..)
>   , performAction
>   , runtimeStateIO
> ) where

> import Kreb.Text

> import Ned.App.State
> import Ned.App.Error
> import Ned.Data
> import Lang



> data Then
>   = GoOn
>   | Stop
>   | Bail AppError
>   deriving (Eq, Show)



> data Action
>   -- Nothing
>   = NoOp
>   | Quit

>   -- Cursor Movement
>   | CursorUp
>   | CursorDown
>   | CursorRight
>   | CursorLeft






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
>   | CharInsertAfter Char
>   | CharInsertBefore Char
>   | CharOverwrite Char
>   | CharDelete
>   | CharBackspace

>   | LineDelete

>   | CharInsertCmdAfter Char
>   | CharBackspaceCmd

>   | RunCmd

>   -- Modes
>   | SetMode EditorMode

>   | SetError String
>   | ClearError

>   | WindowResize (Int, Int)
>   deriving (Eq, Show)



> performAction
>   :: ( Monad m )
>   => Action -> AppState m
>   -> m (Then, AppState m)
> performAction act st = case act of
>   NoOp ->
>     return (GoOn, st)

>   SetError msg -> do
>     let st' = setLastError msg st
>     return (GoOn, st')

>   ClearError -> do
>     let st' = clearLastError st
>     return (GoOn, st')

>   Quit ->
>     return (Stop, st)

>   SetMode mode -> do
>     let st' = setEditorMode mode st
>     return (GoOn, st')

>   CharInsertAfter c -> do
>     let
>       st' = alterActivePanel (alterPanel
>         [PanelAlterText [TextBoxInsert (fromChar c)]]) st
>     return (GoOn, st')

>   CharInsertCmdAfter c -> do
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
>         let st3 = setLastError "no command" st2
>         return (GoOn, st2)
>       Just str -> do
>         r <- evalHook str st2
>         case r of
>           Left err -> do
>             let st3 = setLastError (show err) st2
>             return (GoOn, st3)
>           Right st' -> return (GoOn, st')





> runHook
>   :: ( Monad m )
>   => AppState m -> Hook m a -> m (a, (AppState m))
> runHook st (Hook x) = x st

> evalHook
>   :: ( Monad m )
>   => String -> AppState m -> m (Either (Either Error ReplError) (AppState m))
> evalHook str st =
>   case runParser pPhrase str of
>     Left err -> return $ Left (Left err)
>     Right ph -> do
>       (r, x) <- runHook st $ evalRuntime (doActionFor ph) (runtimeSt st)
>       case r of
>         Left err -> return $ Left (Right err)
>         Right () -> return $ Right x


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


