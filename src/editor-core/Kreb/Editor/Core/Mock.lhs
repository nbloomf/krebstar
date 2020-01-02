> {-# LANGUAGE ScopedTypeVariables #-}

> module Kreb.Editor.Core.Mock where

> import Kreb.Control
> import Kreb.Effect
> import Kreb.Text
> import Kreb.Editor.Core.Data
> import Kreb.Editor.Core.Action
> import Kreb.Editor.Core.Monad
> import Kreb.Lang



> runMockEditor
>   :: forall m
>    . ( Monad m )
>   => Chaos -> MockWorld [(EditorMode, Action)]
>   -> EventId -> FilePath -> (Int, Int)
>   -> m (MockWorld [(EditorMode, Action)])
> runMockEditor chi w eId path dim = do
>   let
>     params :: KrebEdReplParams (Mock [(EditorMode, Action)] m)
>     params = ReplParams
>       { _Init = \_ st -> return (Right st)
>       , _Read = \_ _ -> do
>           e <- getNextEvent
>           return $ case e of
>             Nothing -> [(NormalMode, Quit)]
>             Just acts -> acts
>       , _Eval = \env st acts -> do
>           let eId = EventId (1 + getActionCounter st) "foo"
>           performActions env (tickActionCounter st) eId acts
>       , _Print = \_ _ -> return ()
>       , _Exit = \_ -> return ()
>       }
> 
>     env :: AppEnv (Mock [(EditorMode, Action)] m)
>     env = AppEnv
>       { logWriter       = logWriterMock :: LogWriter (Mock [(EditorMode, Action)] m)
>       , fileReader      = fileReaderMock :: FileReader (Mock [(EditorMode, Action)] m)
>       , fileWriter      = fileWriterMock :: FileWriter (Mock [(EditorMode, Action)] m)
>       , clipboardReader = clipboardReaderMock :: ClipboardReader (Mock [(EditorMode, Action)] m)
>       , clipboardWriter = clipboardWriterMock :: ClipboardWriter (Mock [(EditorMode, Action)] m)
>       }
> 
>   fmap fst $ runMock chi w $ do
>     st' <- buildInitialAppState env eId path dim
>     case st' of
>       Left err -> error $ "runMockEditor: " ++ show err
>       Right st -> runEditorCore params env st
