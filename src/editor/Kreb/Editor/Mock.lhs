> module Kreb.Editor.Mock where

> import Kreb.Editor.State
> import Kreb.Editor.Action
> import Kreb.Editor.Monad
> import Kreb.Editor.Error
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
>   { renderState = mockRenderState
>   , getNextEvent = mockGetNextEvent
>   , cleanup = mockCleanup
>   , logMessage = mockLogMessage
>   }

> runtimeStateMock :: RuntimeState (Hook Mock)
> runtimeStateMock = initRuntimeState editorActionsMock editorTypes

> editorActionsMock
>   :: String -> Maybe (Runtime (Hook Mock) ())
> editorActionsMock str = case str of
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

> runMock :: [Action] -> Mock a -> Either MockInterrupt (a, MockState)
> runMock es (Mock x) = x $ initMockState es


> runMockApp :: (Int, Int) -> [Action] -> Either MockInterrupt (Maybe AppError, MockState)
> runMockApp dim es = runMock es $ runApp mockEnv (initAppState runtimeStateMock dim) primaryEventLoop

