> module Kreb.Editor.Action (
>     Action(..)
>   , Then(..)
>   , performAction
>   , runtimeState
>   , editorTypes
>   , hookActions
>   , performActions
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

>   | LeaveMark
>   | ClearMark






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


> performActions
>   :: ( Monad m )
>   => AppEnv m -> AppState m -> EventId -> [Action]
>   -> m (Either AppSignal (AppState m))
> performActions env st eId acts = case acts of
>   [] -> return (Right st)
>   a:as -> do
>     result <- performAction env st eId a
>     case result of
>       Left sig -> return $ Left sig
>       Right st2 -> performActions env st2 eId as


> performAction
>   :: ( Monad m )
>   => AppEnv m -> AppState m -> EventId -> Action
>   -> m (Either AppSignal (AppState m))
> performAction env st eId act = case act of
>   NoOp -> return $
>     Right st
> 
>   ShowDebug msg -> return $
>     Right $ alterActivePanel (showDebugMessage eId msg) st
> 
>   Quit -> return $
>     Left ExitNormally
> 
>   SetMode mode -> return $
>     Right $ setEditorMode mode st
> 
>   CharInsert c -> return $
>     Right $ alterActivePanel
>       (alterPanel eId
>         [PanelAlterText [TextBoxInsert (fromChar c)]]) st
> 
>   CharInsertCmd c -> return $
>     Right $ alterActivePanel
>       (alterPanel eId
>         [PanelAlterCmd [TextBoxInsert (fromChar c)]]) st
> 
>   StringInsert cs -> return $
>     Right $ alterActivePanel
>       (alterPanel eId
>         [PanelAlterText [TextBoxInsertMany (map fromChar cs)]]) st
> 
>   CharBackspace -> return $
>     Right $ alterActivePanel
>       (alterPanel eId
>         [PanelAlterText [TextBoxBackspace]]) st
> 
>   CharBackspaceCmd -> return $
>     Right $ alterActivePanel
>       (alterPanel eId
>         [PanelAlterCmd [TextBoxBackspace]]) st
> 
>   CursorUp -> return $
>     Right $ alterActivePanel
>       (alterPanel eId
>         [PanelAlterText [TextBoxCursorUp]]) st
> 
>   CursorDown -> return $
>     Right $ alterActivePanel
>       (alterPanel eId
>         [PanelAlterText [TextBoxCursorDown]]) st
> 
>   CursorRight -> return $
>     Right $ alterActivePanel
>       (alterPanel eId
>         [PanelAlterText [TextBoxCursorRight]]) st
> 
>   CursorLeft -> return $
>     Right $ alterActivePanel
>       (alterPanel eId
>         [PanelAlterText [TextBoxCursorLeft]]) st
> 
>   LeaveMark -> return $
>     Right $ alterActivePanel
>       (alterPanel eId
>         [PanelAlterText [TextBoxLeaveMark]]) st
> 
>   ClearMark -> return $
>     Right $ alterActivePanel
>       (alterPanel eId
>         [PanelAlterText [TextBoxClearMark]]) st
> 
>   WindowResize (w,h) -> return $
>     Right $ setWindowDim eId (w,h) st
> 
>   FileLoad path -> do
>     let x = queryActivePanel (textboxHasChanged . getTextBox) st
>     case x of
>       Nothing -> return $ Right st
>       Just True -> return $
>         Right $ alterActivePanel (showDebugMessage eId "Unsaved changes") st
>       Just False -> do
>         read <- loadFile env path
>         case read of
>           Left err -> return $
>             Right $ alterActivePanel (showDebugMessage eId $ show err) st
>           Right contents -> return $
>             Right $ alterActivePanel (alterPanel eId [PanelAlterText [TextBoxLoad path contents]]) st
> 
>   RunCmd -> do
>     let
>       cmd = queryActivePanel getPanelCmdString st
>       st2 = alterActivePanel (alterPanel eId
>         [PanelClearCmd]) st
>     case cmd of
>       Nothing -> return $
>         Right $ alterActivePanel (showDebugMessage eId "no command") st2
>       Just str -> do
>         r <- evalHook eId str st2
>         case r of
>           Left err -> do
>             let
>               msg = case err of
>                 Left a -> displayNeat a
>                 Right b -> displayNeat b
>               st3 = alterActivePanel (showDebugMessage eId (msg ++ "\n")) st2
>             return $ Right st3
>           Right st' -> return $ Right st'
> 
>   act -> return $
>     Right $ alterActivePanel
>       (showDebugMessage eId $ "Not implemented: " ++ show act) st






> runHook
>   :: ( Monad m )
>   => AppState m -> Hook m a -> m (a, AppState m)
> runHook st (Hook x) = x st

> evalHook
>   :: ( Monad m )
>   => EventId -> String -> AppState m
>   -> m (Either (Either Error ReplError) (AppState m))
> evalHook eId str st = case str of
>   ':':'t':' ':rest -> case runParser pPhrase rest of
>     Left err -> return $ Left (Left err)
>     Right ph -> do
>       (r, x) <- runHook st $ evalRuntime (inferType ph) (runtimeSt st)
>       case r of
>         Left err -> return $ Left (Right err)
>         Right tp -> return $ Right $
>           alterActivePanel (updateHistory eId (TypeQuery rest tp)) x
>   _ -> case runParser pPhrase str of
>     Left err -> return $ Left (Left err)
>     Right ph -> do
>       (r, x) <- runHook st $ runRuntime (inferType ph >> doActionFor eId ph) (runtimeSt st)
>       case r of
>         Left err -> return $ Left (Right err)
>         Right ((), rts) -> do
>           let dst = _rtStack rts
>           return $ Right $
>             alterActivePanel (updateHistory eId (RunCommand ph dst)) x


> loadStdLib
>   :: ( Monad m )
>   => FilePath -> AppEnv m -> AppState m -> EventId
>   -> m (Either AppSignal (RuntimeState (Hook m)))
> loadStdLib path env st1 eId = do
>   readResult <- loadFile env path
>   case readResult of
>     Left ioErr -> return $ Left $ StdLibReadError ioErr
>     Right str -> do
>       case runParser pModule str of
>         Left err -> return $ Left $ StdLibParseError err
>         Right ast -> do
>           let Module ds = ast
>           (result, st2) <- runHook st1 $ runRuntime (applyDecls eId ds) (initRuntimeState (hookActions env) editorTypes)
>           return $ case result of
>             Left err -> Left $ StdLibInterpretError err
>             Right (_, rts) -> Right rts


> runtimeState
>   :: ( Monad m )
>   => AppEnv m -> EventId -> RuntimeState (Hook m)
> runtimeState env eId =
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
> 
>   "#insert" -> Just $
>     ForAll (Vars [V "S"] []) $ Arrow
>       (Stack (V "S") [TyCon $ C "String", TyCon $ C "@Eff"])
>       (Stack (V "S") [TyCon $ C "@Eff"])
> 
>   "#load_file" -> Just $
>     ForAll (Vars [V "S"] []) $ Arrow
>       (Stack (V "S") [TyCon $ C "String", TyCon $ C "@Eff"])
>       (Stack (V "S") [TyCon $ C "@Eff"])
> 
>   "#set_path" -> Just $
>     ForAll (Vars [V "S"] []) $ Arrow
>       (Stack (V "S") [])
>       (Stack (V "S") [])
> 
>   _ -> Nothing

> hookActions
>   :: ( Monad m )
>   => AppEnv m -> EventId -> String -> Maybe (Runtime (Hook m) ())
> hookActions env eId str = case str of
>   "#cursor_left" -> Just $ do
>     doHookActionM env eId (CursorLeft)
>   "#cursor_right" -> Just $ do
>     doHookActionM env eId (CursorRight)
>   "#cursor_up" -> Just $ do
>     doHookActionM env eId (CursorUp)
>   "#cursor_down" -> Just $ do
>     doHookActionM env eId (CursorDown)
> 
>   "#insert" -> Just $ do
>     str <- popString
>     doHookActionM env eId (StringInsert str)
>   "#load_file" -> Just $ do
>     path <- popString
>     doHookActionM env eId (FileLoad path)
>   _ -> Nothing

> doHookActionM
>   :: ( Monad m )
>   => AppEnv m -> EventId -> Action -> Runtime (Hook m) ()
> doHookActionM env eId act = Runtime $ \rts ->
>   Hook $ \st -> do
>     result <- performAction env st eId act
>     case result of
>       Left sig -> return (Right ((), rts), st)
>       Right st' -> return (Right ((), rts), st')
