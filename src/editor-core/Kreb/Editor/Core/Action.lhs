> module Kreb.Editor.Core.Action (
>     performAction
>   , runtimeState
>   , editorTypes
>   , hookActions
>   , performActions
>   , loadStdLib
> ) where

> import Kreb.Text
> import Kreb.Effect
> import Kreb.Format
> import Kreb.Editor.Core.Data
> import Kreb.Lang



> performActions
>   :: ( Monad m )
>   => AppEnv m -> AppState m -> EventId -> [(EditorMode, Action)]
>   -> m (Either AppSignal (AppState m))
> performActions env st eId acts = case acts of
>   [] -> return (Right st)
>   (mode,a):as -> do
>     result <- performAction env st mode eId a
>     case result of
>       Left sig -> return $ Left sig
>       Right st2 -> performActions env st2 eId as

> doPanelActions
>   :: ( Monad m )
>   => EventId -> AppState m -> [PanelAction m]
>   -> m (Either AppSignal (AppState m))
> doPanelActions eId st acts = fmap Right $
>   alterActivePanelM (alterPanelM eId acts) st

> performAction
>   :: ( Monad m )
>   => AppEnv m -> AppState m -> EditorMode -> EventId -> Action
>   -> m (Either AppSignal (AppState m))
> performAction env st mode eId act = do
>   logMessageWith (logWriter env) Debug_ $ show act
>   case act of
>     NoOp -> return (Right st)
> 
>     Quit -> return (Left ExitNormally)
> 
>     ShowDebug msg -> fmap Right $
>       alterActivePanelM (showDebugMessage eId msg) st
> 
>     CharInsert c -> case mode of
>       NormalMode -> return (Right st)
>       InsertMode -> doPanelActions eId st
>         [PanelAlterText
>           [ TextBoxDeleteRegion
>           , TextBoxInsert (fromChar c) ]]
>       CommandMode -> doPanelActions eId st
>         [PanelAlterCmd
>           [ TextBoxDeleteRegion
>           , TextBoxInsert (fromChar c) ]]
> 
>     StringInsert cs -> doPanelActions eId st
>       [PanelAlterText
>         [ TextBoxDeleteRegion
>         , TextBoxInsertMany (map fromChar cs) ]]
> 
>     CharBackspace -> case mode of
>       NormalMode -> return (Right st)
>       InsertMode -> doPanelActions eId st
>         [PanelAlterText
>           [ TextBoxDeleteRegion
>           , TextBoxBackspace ]]
>       CommandMode -> doPanelActions eId st
>         [PanelAlterCmd
>           [ TextBoxDeleteRegion
>           , TextBoxBackspace ]]
> 
>     CursorLeft -> case mode of
>       NormalMode -> return (Right st)
>       InsertMode -> doPanelActions eId st
>         [PanelAlterText [TextBoxCursorLeft]]
>       CommandMode -> doPanelActions eId st
>         [PanelAlterCmd [TextBoxCursorLeft]]
> 
>     CursorRight -> case mode of
>       NormalMode -> return (Right st)
>       InsertMode -> doPanelActions eId st
>         [PanelAlterText [TextBoxCursorRight]]
>       CommandMode -> doPanelActions eId st
>         [PanelAlterCmd [TextBoxCursorRight]]
> 
>     CursorUp -> doPanelActions eId st
>       [PanelAlterText [TextBoxCursorUp]]
> 
>     CursorDown -> doPanelActions eId st
>       [PanelAlterText [TextBoxCursorDown]]
> 
>     CursorTo pos -> case mode of
>       NormalMode -> return (Right st)
>       InsertMode -> doPanelActions eId st
>         [PanelAlterText [TextBoxCursorTo pos]]
>       CommandMode -> doPanelActions eId st
>         [PanelAlterCmd [TextBoxCursorTo pos]]
> 
>     CursorDrag pos -> case mode of
>       NormalMode -> return (Right st)
>       InsertMode -> doPanelActions eId st
>         [PanelAlterText [TextBoxCursorDrag pos]]
>       CommandMode -> doPanelActions eId st
>         [PanelAlterCmd [TextBoxCursorDrag pos]]
> 
>     CancelDrag -> case mode of
>       NormalMode -> return (Right st)
>       InsertMode -> doPanelActions eId st
>         [PanelAlterText [TextBoxCancelDrag]]
>       CommandMode -> doPanelActions eId st
>         [PanelAlterCmd [TextBoxCancelDrag]]
> 
>     RegionDelete -> case mode of
>       NormalMode -> return (Right st)
>       InsertMode -> doPanelActions eId st
>         [PanelAlterText [TextBoxDeleteRegion]]
>       CommandMode -> doPanelActions eId st
>         [PanelAlterCmd [TextBoxDeleteRegion]]
> 
>     RegionClip -> case mode of
>       NormalMode -> return (Right st)
>       InsertMode -> doPanelActions eId st
>         [PanelAlterText [TextBoxClipRegion (clipboardWriter env)]]
>       CommandMode -> doPanelActions eId st
>         [PanelAlterCmd [TextBoxClipRegion (clipboardWriter env)]]
> 
>     RegionPaste -> case mode of
>       NormalMode -> return (Right st)
>       InsertMode -> doPanelActions eId st
>         [PanelAlterText [TextBoxPasteRegion (clipboardReader env)]]
>       CommandMode -> doPanelActions eId st
>         [PanelAlterCmd [TextBoxPasteRegion (clipboardReader env)]]
> 
>     LeaveMark -> doPanelActions eId st
>       [PanelAlterText [TextBoxLeaveMark]]
> 
>     ClearMark -> doPanelActions eId st
>       [PanelAlterText [TextBoxClearMark]]
> 
>     WindowResize (w,h) -> fmap Right $
>       setWindowDim eId (w,h) st
> 
>     FileLoad path -> doPanelActions eId st
>       [PanelAlterText
>         [ TextBoxSetSource path
>         , TextBoxLoad False (fileReader env) ]]
> 
>     RunCmd -> do
>       let cmd = queryActivePanel getPanelCmdString st
>       st2 <- alterActivePanelM (alterPanelM eId [PanelClearCmd]) st
>       case cmd of
>         Nothing -> fmap Right $
>           alterActivePanelM (showDebugMessage eId "no command") st2
>         Just str -> do
>           r <- evalHook eId str st2
>           case r of
>             Left err -> do
>               let
>                 msg = case err of
>                   Left a -> displayNeat a
>                   Right b -> displayNeat b
>               fmap Right $ alterActivePanelM (showDebugMessage eId (msg ++ "\n")) st2
>             Right st' -> return $ Right st'
> 
>     act -> fmap Right $ alterActivePanelM
>       (showDebugMessage eId $ "Not implemented: " ++ show act) st





> evalHook
>   :: ( Monad m )
>   => EventId -> String -> AppState m
>   -> m (Either (Either Error ReplError) (AppState m))
> evalHook eId str st = case str of
>   -- ":t expr"
>   ':':'t':' ':rest -> case runParser pPhrase rest of
>     Left err -> return $ Left (Left err)
>     Right ph -> do
>       (r, x) <- runHook st $ evalRuntime (inferType ph) (runtimeSt st)
>       case r of
>         Left err -> return $ Left (Right err)
>         Right tp -> fmap Right $
>           alterActivePanelM (updateHistory eId (TypeQuery rest tp)) x
>   _ -> case runParser pPhrase str of
>     Left err -> return $ Left (Left err)
>     Right ph -> do
>       (r, x) <- runHook st $ runRuntime (inferType ph >> doActionFor eId ph) (runtimeSt st)
>       case r of
>         Left err -> return $ Left (Right err)
>         Right ((), rts) -> do
>           let dst = _rtStack rts
>           fmap Right $
>             alterActivePanelM (updateHistory eId (RunCommand ph dst)) x


> loadStdLib
>   :: ( Monad m )
>   => FilePath -> AppEnv m -> AppState m -> EventId
>   -> m (Either AppSignal (RuntimeState (Hook m)))
> loadStdLib path env st1 eId = do
>   readResult <- readFileWith (fileReader env) path
>   case readResult of
>     Left ioErr -> return $ Left $ StdLibReadError ioErr
>     Right str -> 
>       if str == ""
>         then do
>           (result, st2) <- runHook st1 $ runRuntime (applyDecls eId []) (initRuntimeState (hookActions env) editorTypes)
>           return $ case result of
>             Left err -> Left $ StdLibInterpretError err
>             Right (_, rts) -> Right rts
>         else do
>           case runParser pModule str of
>             Left err -> return $ Left $ StdLibParseError err
>             Right ast -> do
>               let Module ds = ast
>               (result, st2) <- runHook st1 $ runRuntime (applyDecls eId ds) (initRuntimeState (hookActions env) editorTypes)
>               return $ case result of
>                 Left err -> Left $ StdLibInterpretError err
>                 Right (_, rts) -> Right rts


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
>       (Stack (V "S") [TyCon $ C "String", TyCon $ C "@Eff"])
>       (Stack (V "S") [TyCon $ C "@Eff"])
> 
>   "#delete_region" -> Just $
>     ForAll (Vars [V "S"] []) $ Arrow
>       (Stack (V "S") [TyCon $ C "@Eff"])
>       (Stack (V "S") [TyCon $ C "@Eff"])
> 
>   _ -> Nothing

> hookActions
>   :: ( Monad m )
>   => AppEnv m -> EventId -> String
>   -> Maybe (Runtime (Hook m) ())
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
> 
>   "#delete_region" -> Just $ do
>     doHookActionM env eId (RegionDelete)
> 
>   _ -> Nothing

> doHookActionM
>   :: ( Monad m )
>   => AppEnv m -> EventId -> Action -> Runtime (Hook m) ()
> doHookActionM env eId act = Runtime $ \rts ->
>   Hook $ \st -> do
>     result <- performAction env st InsertMode eId act
>     case result of
>       Left sig -> return (Right ((), rts), st)
>       Right st' -> return (Right ((), rts), st')
