> module Kreb.Editor.Mock where

> import Kreb.Control
> import Kreb.Editor.State
> import Kreb.Editor.Action
> import Kreb.Editor.Monad
> import Kreb.Editor.Signal
> import Kreb.Editor.Env
> import Kreb.Lang

> data MockInterrupt
>   = NoNextEvent
>   deriving (Eq, Show)

> data MockState = MockState
>   { mockCurrentScreen :: [String]
>   , mockLogs :: [String]
>   , mockFutureEvents :: [Action]
>   } deriving (Eq, Show)

> initMockState :: [Action] -> MockState
> initMockState events = MockState
>   { mockCurrentScreen = []
>   , mockLogs = []
>   , mockFutureEvents = events
>   }

> newtype Mock a = Mock
>   { unMock :: MockState -> Either MockInterrupt (a, MockState)
>   }

> instance Functor Mock where
>   fmap f (Mock x) = Mock $ \st1 ->
>     case x st1 of
>       Left i -> Left i
>       Right (a, st2) -> Right (f a, st2)
> 
> instance Applicative Mock where
>   pure a = Mock $ \st -> Right (a, st)
> 
>   (Mock f) <*> (Mock x) = Mock $ \st1 ->
>     case f st1 of
>       Left i -> Left i
>       Right (f', st2) -> case x st2 of
>         Left i -> Left i
>         Right (x', st3) -> Right (f' x', st3)
> 
> instance Monad Mock where
>   return = pure
> 
>   (Mock x) >>= f = Mock $ \st1 ->
>     case x st1 of
>       Left i -> Left i
>       Right (x', st2) -> (unMock $ f x') st2

> mockGets :: (MockState -> a) -> Mock a
> mockGets f = Mock $ \st ->
>   Right (f st, st)
> 
> mockMuts :: (MockState -> MockState) -> Mock ()
> mockMuts f = Mock $ \st ->
>   Right ((), f st)

> mockRenderState :: AppState Mock -> Mock ()
> mockRenderState st = return ()

> mockGetNextEvent :: EditorMode -> Mock Action
> mockGetNextEvent _ = Mock $ \st ->
>   case mockFutureEvents st of
>     [] -> Left NoNextEvent
>     e:es -> Right (e, st { mockFutureEvents = es })

> mockCleanup :: Mock ()
> mockCleanup = return ()

> mockLogMessage :: String -> Mock ()
> mockLogMessage msg = mockMuts $ \st ->
>   st { mockLogs = msg : mockLogs st }

> mockEnv :: AppEnv Mock
> mockEnv = AppEnv
>   { logMessage = mockLogMessage
>   }

> mockReplParams :: KrebEdReplParams Mock
> mockReplParams = ReplParams
>   { _Init = \_ st -> return (Right st)
>   , _Read = \_ st -> do
>       let mode = editorMode st
>       mockGetNextEvent mode
>   , _Eval = \_ st1 act -> do
>       result <- performAction mockEnv st1 act
>       case result of
>         Right st2 -> return (Right st2)
>         Left sig -> return (Left sig)
>   , _Print = \_ st -> mockRenderState st
>   , _Exit = \_ -> return ()
>   }

> runtimeStateMock :: RuntimeState (Hook Mock)
> runtimeStateMock = initRuntimeState (hookActions mockEnv) editorTypes

> runMock :: [Action] -> Mock a -> Either MockInterrupt (a, MockState)
> runMock es (Mock x) = x $ initMockState es


> runMockApp :: (Int, Int) -> [Action] -> Either MockInterrupt ((), MockState)
> runMockApp dim es = runMock es $ runKrebEd mockReplParams mockEnv (initAppState "stdlib.txt" runtimeStateMock dim) loopReplT

