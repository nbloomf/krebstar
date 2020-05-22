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
> import Kreb.Editor.Core.Data
> import Kreb.Lang



> performActions
>   :: ( Monad m )
>   => AppEnv m -> AppState EventId m -> EventId -> [(EditorMode, Action)]
>   -> m (Either AppSignal (AppState EventId m))
> performActions env st eId acts = case acts of
>   [] -> return (Right st)
>   (mode,a):as -> do
>     result <- performAction env st mode eId a
>     case result of
>       Left sig -> return $ Left sig
>       Right st2 -> performActions env st2 eId as

> doPanelActions
>   :: ( Monad m )
>   => AppEnv m -> EventId -> AppState EventId m -> [PanelAction]
>   -> m (Either AppSignal (AppState EventId m))
> doPanelActions env eId st acts = fmap Right $
>   alterActivePanelM (alterPanelM (textboxFX env) eId acts) st

> performAction
>   :: ( Monad m )
>   => AppEnv m -> AppState EventId m -> EditorMode -> EventId -> Action
>   -> m (Either AppSignal (AppState EventId m))
> performAction env st mode eId act = do
>   logMessageWith (logWriter $ textboxFX env) Debug_ $ show act
>   case act of
>     NoOp -> return (Right st)
> 
>     Quit -> return (Left ExitNormally)
> 
>     ShowDebug msg -> fmap Right $
>       alterActivePanelM (showDebugMessage (textboxFX env) eId msg) st
> 
>     CharInsert c -> case mode of
>       NormalMode -> return (Right st)
>       InsertMode -> doPanelActions env eId st
>         [PanelAlterText
>           [ TextBoxDeleteRegion
>           , TextBoxInsert c ]]
>       CommandMode -> doPanelActions env eId st
>         [PanelAlterCmd
>           [ TextBoxDeleteRegion
>           , TextBoxInsert c ]]
> 
>     StringInsert cs -> doPanelActions env eId st
>       [PanelAlterText
>         [ TextBoxDeleteRegion
>         , TextBoxInsertMany cs ]]
> 
>     CharBackspace -> case mode of
>       NormalMode -> return (Right st)
>       InsertMode -> doPanelActions env eId st
>         [PanelAlterText
>           [ TextBoxDeleteRegion
>           , TextBoxBackspace ]]
>       CommandMode -> doPanelActions env eId st
>         [PanelAlterCmd
>           [ TextBoxDeleteRegion
>           , TextBoxBackspace ]]
> 
>     CursorLeft -> case mode of
>       NormalMode -> return (Right st)
>       InsertMode -> doPanelActions env eId st
>         [PanelAlterText [TextBoxCursorLeft]]
>       CommandMode -> doPanelActions env eId st
>         [PanelAlterCmd [TextBoxCursorLeft]]
> 
>     CursorRight -> case mode of
>       NormalMode -> return (Right st)
>       InsertMode -> doPanelActions env eId st
>         [PanelAlterText [TextBoxCursorRight]]
>       CommandMode -> doPanelActions env eId st
>         [PanelAlterCmd [TextBoxCursorRight]]
> 
>     CursorUp -> doPanelActions env eId st
>       [PanelAlterText [TextBoxCursorUp]]
> 
>     CursorDown -> doPanelActions env eId st
>       [PanelAlterText [TextBoxCursorDown]]
> 
>     CursorTo pos -> case mode of
>       NormalMode -> return (Right st)
>       InsertMode -> doPanelActions env eId st
>         [PanelAlterText [TextBoxCursorTo pos]]
>       CommandMode -> doPanelActions env eId st
>         [PanelAlterCmd [TextBoxCursorTo pos]]
> 
>     CursorDrag pos -> case mode of
>       NormalMode -> return (Right st)
>       InsertMode -> doPanelActions env eId st
>         [PanelAlterText [TextBoxCursorDrag pos]]
>       CommandMode -> doPanelActions env eId st
>         [PanelAlterCmd [TextBoxCursorDrag pos]]
> 
>     CancelDrag -> case mode of
>       NormalMode -> return (Right st)
>       InsertMode -> doPanelActions env eId st
>         [PanelAlterText [TextBoxCancelDrag]]
>       CommandMode -> doPanelActions env eId st
>         [PanelAlterCmd [TextBoxCancelDrag]]
> 
>     RegionDelete -> case mode of
>       NormalMode -> return (Right st)
>       InsertMode -> doPanelActions env eId st
>         [PanelAlterText [TextBoxDeleteRegion]]
>       CommandMode -> doPanelActions env eId st
>         [PanelAlterCmd [TextBoxDeleteRegion]]
> 
>     RegionClip -> case mode of
>       NormalMode -> return (Right st)
>       InsertMode -> doPanelActions env eId st
>         [PanelAlterText [TextBoxClipRegion]]
>       CommandMode -> doPanelActions env eId st
>         [PanelAlterCmd [TextBoxClipRegion]]
> 
>     RegionPaste -> case mode of
>       NormalMode -> return (Right st)
>       InsertMode -> doPanelActions env eId st
>         [PanelAlterText [TextBoxPasteRegion]]
>       CommandMode -> doPanelActions env eId st
>         [PanelAlterCmd [TextBoxPasteRegion]]
> 
>     LeaveMark -> doPanelActions env eId st
>       [PanelAlterText [TextBoxLeaveMark]]
> 
>     ClearMark -> doPanelActions env eId st
>       [PanelAlterText [TextBoxClearMark]]
> 
>     WindowResize (w,h) -> fmap Right $
>       setWindowDim (textboxFX env) eId (w,h) st
> 
>     FileLoad path -> doPanelActions env eId st
>       [PanelAlterText
>         [ TextBoxSetSource path
>         , TextBoxLoad False ]]
> 
>     RunCmd -> do
>       let cmd = queryActivePanel getPanelCmdString st
>       st2 <- alterActivePanelM (alterPanelM (textboxFX env) eId [PanelClearCmd]) st
>       case cmd of
>         Nothing -> fmap Right $
>           alterActivePanelM (showDebugMessage (textboxFX env) eId "no command") st2
>         Just str -> do
>           r <- evalHook env eId str st2
>           case r of
>             Left err -> do
>               let
>                 msg = case err of
>                   Left a -> show a
>                   Right b -> show b
>               fmap Right $ alterActivePanelM (showDebugMessage (textboxFX env) eId (msg ++ "\n")) st2
>             Right st' -> return $ Right st'
> 
>     act -> fmap Right $ alterActivePanelM
>       (showDebugMessage (textboxFX env) eId $ "Not implemented: " ++ show act) st





> evalHook
>   :: ( Monad m )
>   => AppEnv m -> EventId -> String -> AppState EventId m
>   -> m (Either (Either Error ReplError) (AppState EventId m))
> evalHook env eId str st = case str of
>   -- ":t expr"
>   ':':'t':' ':rest -> case runParser pPhrase rest of
>     Left err -> return $ Left (Left err)
>     Right ph -> do
>       (r, x) <- runHook st $ evalRuntime (inferType ph) (runtimeSt st)
>       case r of
>         Left err -> return $ Left (Right err)
>         Right tp -> fmap Right $
>           alterActivePanelM (updateHistory (textboxFX env) eId (TypeQuery rest tp)) x
>   _ -> case runParser pPhrase str of
>     Left err -> return $ Left (Left err)
>     Right ph -> do
>       (r, x) <- runHook st $ runRuntime (inferType ph >> doActionFor eId ph) (runtimeSt st)
>       case r of
>         Left err -> return $ Left (Right err)
>         Right ((), rts) -> do
>           let dst = _rtStack rts
>           fmap Right $
>             alterActivePanelM (updateHistory (textboxFX env) eId (RunCommand ph dst)) x


> loadStdLib
>   :: ( Monad m )
>   => FilePath -> AppEnv m -> AppState EventId m -> EventId
>   -> m (Either AppSignal (RuntimeState EventId (Hook EventId m)))
> loadStdLib path env st1 eId = do
>   readResult <- readFileWith (fileReader $ textboxFX env) path
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
>   => AppEnv m -> EventId -> RuntimeState EventId (Hook EventId m)
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
>   -> Maybe (Runtime EventId (Hook EventId m) ())
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
>   => AppEnv m -> EventId -> Action -> Runtime EventId (Hook EventId m) ()
> doHookActionM env eId act = Runtime $ \rts ->
>   Hook $ \st -> do
>     result <- performAction env st InsertMode eId act
>     case result of
>       Left sig -> return (Right ((), rts), st)
>       Right st' -> return (Right ((), rts), st')
