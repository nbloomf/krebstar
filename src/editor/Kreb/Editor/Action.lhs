> module Kreb.Editor.Action (
>     Action(..)
>   , Then(..)
>   , performAction
>   , runtimeState
>   , editorTypes
>   , hookActions

>   , loadStdLib
> ) where

> import Kreb.Text

> import Kreb.Format
> import Kreb.Editor.State
> import Kreb.Editor.Env
> import Kreb.Editor.Signal
> import Kreb.Editor.Panel
> import Kreb.Lang



> data Then
>   = GoOn
>   | Stop
>   | Bail AppSignal
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

>   | StringInsert String

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
>   => AppEnv m -> AppState m -> Action
>   -> m (Either AppSignal (AppState m))
> performAction env st act = case act of
>   NoOp -> return $
>     Right st
> 
>   ShowDebug msg -> return $
>     Right $ alterActivePanel (showDebugMessage msg) st
> 
>   Quit -> return $
>     Left ExitNormally
> 
>   SetMode mode -> return $
>     Right $ setEditorMode mode st
> 
>   CharInsert c -> return $
>     Right $ alterActivePanel
>       (alterPanel
>         [PanelAlterText [TextBoxInsert (fromChar c)]]) st
> 
>   CharInsertCmd c -> return $
>     Right $ alterActivePanel
>       (alterPanel
>         [PanelAlterCmd [TextBoxInsert (fromChar c)]]) st
> 
>   StringInsert cs -> return $
>     Right $ alterActivePanel
>       (alterPanel
>         [PanelAlterText [TextBoxInsertMany (map fromChar cs)]]) st
> 
>   CharBackspace -> return $
>     Right $ alterActivePanel
>       (alterPanel
>         [PanelAlterText [TextBoxBackspace]]) st
> 
>   CharBackspaceCmd -> return $
>     Right $ alterActivePanel
>       (alterPanel
>         [PanelAlterCmd [TextBoxBackspace]]) st
> 
>   CursorUp -> return $
>     Right $ alterActivePanel
>       (alterPanel
>         [PanelAlterText [TextBoxCursorUp]]) st
> 
>   CursorDown -> return $
>     Right $ alterActivePanel
>       (alterPanel
>         [PanelAlterText [TextBoxCursorDown]]) st
> 
>   CursorRight -> return $
>     Right $ alterActivePanel
>       (alterPanel
>         [PanelAlterText [TextBoxCursorRight]]) st
> 
>   CursorLeft -> return $
>     Right $ alterActivePanel
>       (alterPanel
>         [PanelAlterText [TextBoxCursorLeft]]) st
> 
>   WindowResize (w,h) -> return $
>     Right $ setWindowDim (w,h) st
> 
>   RunCmd -> do
>     let
>       cmd = queryActivePanel getPanelCmdString st
>       st2 = alterActivePanel (alterPanel
>         [PanelClearCmd]) st
>     case cmd of
>       Nothing -> return $
>         Right $ alterActivePanel (showDebugMessage "no command") st2
>       Just str -> do
>         r <- evalHook str st2
>         case r of
>           Left err -> do
>             let
>               msg = case err of
>                 Left a -> displayNeat a
>                 Right b -> displayNeat b
>               st3 = alterActivePanel (showDebugMessage (msg ++ "\n")) st2
>             return $ Right st3
>           Right st' -> return $ Right st'
> 
>   act -> return $
>     Right $ alterActivePanel
>       (showDebugMessage $ " Not implemented: " ++ show act) st






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
>           alterActivePanel (updateHistory (TypeQuery rest tp)) x
>   _ -> case runParser pPhrase str of
>     Left err -> return $ Left (Left err)
>     Right ph -> do
>       (r, x) <- runHook st $ evalRuntime (inferType ph >> doActionFor ph) (runtimeSt st)
>       case r of
>         Left err -> return $ Left (Right err)
>         Right () -> return $ Right $
>           alterActivePanel (updateHistory (RunCommand ph)) x


> loadStdLib
>   :: ( Monad m )
>   => FilePath -> AppEnv m -> AppState m
>   -> m (Either AppSignal (RuntimeState (Hook m)))
> loadStdLib path env st1 = do
>   readResult <- loadFile env path
>   case readResult of
>     Left ioErr -> return $ Left $ StdLibReadError ioErr
>     Right str -> do
>       case runParser pModule str of
>         Left err -> return $ Left $ StdLibParseError err
>         Right ast -> do
>           let Module ds = ast
>           (result, st2) <- runHook st1 $ runRuntime (applyDecls ds) (initRuntimeState (hookActions env) editorTypes)
>           return $ case result of
>             Left err -> Left $ StdLibInterpretError err
>             Right (_, rts) -> Right rts


> runtimeState
>   :: ( Monad m )
>   => AppEnv m -> RuntimeState (Hook m)
> runtimeState env =
>   initRuntimeState (hookActions env) editorTypes

> editorTypes
>   :: String -> Maybe Scheme
> editorTypes str = case str of
>   "#cursor_left" -> Just $
>     ForAll (Vars [V "S"] []) $ Arrow
>       (Stack (V "S") [TyCon $ C "@Eff"])
>       (Stack (V "S") [TyCon $ C "@Eff"])
>   "#cursor_right" -> Just $
>     ForAll (Vars [V "S"] []) $ Arrow
>       (Stack (V "S") [TyCon $ C "@Eff"])
>       (Stack (V "S") [TyCon $ C "@Eff"])
>   "#cursor_up" -> Just $
>     ForAll (Vars [V "S"] []) $ Arrow
>       (Stack (V "S") [TyCon $ C "@Eff"])
>       (Stack (V "S") [TyCon $ C "@Eff"])
>   "#cursor_down" -> Just $
>     ForAll (Vars [V "S"] []) $ Arrow
>       (Stack (V "S") [TyCon $ C "@Eff"])
>       (Stack (V "S") [TyCon $ C "@Eff"])

>   "#insert" -> Just $
>     ForAll (Vars [V "S"] []) $ Arrow
>       (Stack (V "S") [TyCon $ C "String", TyCon $ C "@Eff"])
>       (Stack (V "S") [TyCon $ C "@Eff"])
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

> hookActions
>   :: ( Monad m )
>   => AppEnv m -> String -> Maybe (Runtime (Hook m) ())
> hookActions env str = case str of
>   "#cursor_left" -> Just $ do
>     doHookActionM env (CursorLeft)
>   "#cursor_right" -> Just $ do
>     doHookActionM env (CursorRight)
>   "#cursor_up" -> Just $ do
>     doHookActionM env (CursorUp)
>   "#cursor_down" -> Just $ do
>     doHookActionM env (CursorDown)

>   "#insert" -> Just $ do
>     str <- popString
>     doHookActionM env (StringInsert str)
>   _ -> Nothing

> doHookActionM
>   :: ( Monad m )
>   => AppEnv m -> Action -> Runtime (Hook m) ()
> doHookActionM env act = Runtime $ \rts ->
>   Hook $ \st -> do
>     result <- performAction env st act
>     case result of
>       Left sig -> return (Right ((), rts), st)
>       Right st' -> return (Right ((), rts), st')
