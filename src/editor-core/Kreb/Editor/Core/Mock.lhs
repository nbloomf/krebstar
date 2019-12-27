> {-# LANGUAGE ScopedTypeVariables #-}

> module Kreb.Editor.Core.Mock where

> import Kreb.Control
> import Kreb.Effect
> import Kreb.Text
> import Kreb.Editor.Core.Panel
> import Kreb.Editor.Core.State
> import Kreb.Editor.Core.Action
> import Kreb.Editor.Core.Monad
> import Kreb.Editor.Core.Signal
> import Kreb.Editor.Core.Env
> import Kreb.Lang



> runMockEditor
>   :: forall m
>    . ( Monad m )
>   => Chaos -> MockWorld [Action]
>   -> EventId -> FilePath -> (Int, Int)
>   -> m (MockWorld [Action])
> runMockEditor chi w eId path dim =
>   let
>     params :: KrebEdReplParams (Mock [Action] m)
>     params = ReplParams
>       { _Init = \_ st -> return (Right st)
>       , _Read = \_ _ -> do
>           e <- getNextEvent
>           return $ case e of
>             Nothing -> [Quit]
>             Just acts -> acts
>       , _Eval = \env st acts -> do
>           let eId = EventId (1 + getActionCounter st) "foo"
>           performActions env (tickActionCounter st) eId acts
>       , _Print = \_ _ -> return ()
>       , _Exit = \_ -> return ()
>       }
> 
>     env :: AppEnv (Mock [Action] m)
>     env = AppEnv
>       { logWriter = logWriterMock :: LogWriter (Mock [Action] m)
>       , fileReader = fileReaderMock :: FileReader (Mock [Action] m)
>       , fileWriter = fileWriterMock :: FileWriter (Mock [Action] m)
>       }
> 
>   in fmap fst $ runMock chi w $
>     runKrebEd params env eId path dim
