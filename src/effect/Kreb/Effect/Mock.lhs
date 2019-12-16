> module Kreb.Effect.Mock where

> import System.IO.Error
> import Data.List (lines)

> import qualified Kreb.Struct.FiniteMap as FM
> import Kreb.Effect.Data
> import Kreb.Check



> newtype Chaos
>   = Chaos { unChaos :: Integer }
>   deriving (Eq, Show)

> _MODULUS :: Integer
> _MODULUS = (2 ^ 31) - 1 -- 8th Mersenne prime

> initChaos
>   :: Integer -> Chaos
> initChaos k =
>   Chaos ((1 + abs k) `mod` _MODULUS)

> nextChaos
>   :: Chaos -> Chaos
> nextChaos (Chaos k) =
>   Chaos ((17 * k) `mod` _MODULUS)

> rareEvent
>   :: Chaos -> Bool
> rareEvent (Chaos k) =
>   0 == (k `mod` 199)

> residueMod
>   :: Integer -> Chaos -> Integer
> residueMod m (Chaos k) = mod k m

> instance Arb Chaos where
>   arb = initChaos <$> arb
> 
> instance Prune Chaos where
>   prune _ = []



> newtype Mock ev m a = Mock
>   { unMock
>       :: Chaos -> MockWorld ev -> m (Chaos, MockWorld ev, a)
>   }

> runMock
>   :: ( Monad m )
>   => Chaos -> MockWorld ev
>   -> Mock ev m a -> m (MockWorld ev, a)
> runMock chi w x = do
>   (_,u,a) <- unMock x chi w
>   return (u,a)

> instance
>   ( Monad m
>   ) => Functor (Mock ev m)
>   where
>     fmap f (Mock x) = Mock $ \chi1 st1 -> do
>       (chi2, st2, a) <- x chi1 st1
>       return (nextChaos chi2, st2, f a)

> instance
>   ( Monad m
>   ) => Applicative (Mock ev m)
>   where
>     pure a = Mock $ \chi st ->
>       return (nextChaos chi, st, a)
> 
>     (Mock f) <*> (Mock x) = Mock $ \chi1 st1 -> do
>       (chi2, st2, g) <- f chi1 st1
>       (chi3, st3, a) <- x chi2 st2
>       return (nextChaos chi3, st3, g a)

> instance
>   ( Monad m
>   ) => Monad (Mock ev m)
>   where
>     return = pure
> 
>     (Mock x) >>= f = Mock $ \chi1 st1 -> do
>       (chi2, st2, a) <- x chi1 st1
>       unMock (f a) chi2 st2

> instance
>   ( LiftIO m
>   ) => LiftIO (Mock ev m)
>   where
>     liftIO x = Mock $ \chi st -> do
>       a <- liftIO x
>       return (nextChaos chi, st, a)

> getChaos
>   :: ( Monad m )
>   => Mock ev m Chaos
> getChaos = Mock $ \chi st ->
>   return (nextChaos chi, st, chi)

> getMockWorld
>   :: ( Monad m )
>   => Mock ev m (MockWorld ev)
> getMockWorld =
>   Mock $ \chi st ->
>     return (nextChaos chi, st, st)
> 
> setMockWorld
>   :: ( Monad m )
>   => MockWorld ev -> Mock ev m ()
> setMockWorld w =
>   Mock $ \chi _ ->
>     return (nextChaos chi, w, ())
> 
> alterMockWorld
>   :: ( Monad m )
>   => (MockWorld ev -> MockWorld ev) -> Mock ev m ()
> alterMockWorld f =
>   Mock $ \chi st ->
>     return (nextChaos chi, f st, ())



> data MockWorld ev = MockWorld
>   { _filesystem :: MockFilesystem
>   , _stdio :: MockStdio
>   , _events :: [ev]
>   } deriving (Eq, Show)
> 
> initMockWorld :: [ev] -> MockWorld ev
> initMockWorld es = MockWorld
>   { _filesystem = emptyMockFilesystem
>   , _stdio = emptyMockStdio
>   , _events = es
>   }
> 
> withMockFilesystem
>   :: MockFilesystem -> MockWorld ev -> MockWorld ev
> withMockFilesystem fs w =
>   w { _filesystem = fs }
> 
> withMockStdio
>   :: MockStdio -> MockWorld ev -> MockWorld ev
> withMockStdio std w =
>   w { _stdio = std }

> data MockFilesystem = MockFilesystem
>   { unMockFilesystem :: FM.FiniteMap FilePath String
>   } deriving (Eq, Show)
> 
> emptyMockFilesystem :: MockFilesystem
> emptyMockFilesystem = MockFilesystem FM.empty

> _filesystem_is_empty
>   :: MockWorld ev -> Bool
> _filesystem_is_empty =
>   FM.isEmpty . unMockFilesystem . _filesystem

> _stdout_is_empty
>   :: MockWorld ev -> Bool
> _stdout_is_empty =
>   (== []) . _stdout . _stdio

> data MockStdio = MockStdio
>   { _stdin :: [String]
>   , _stdout :: [String]
>   , _stderr :: [String]
>   } deriving (Eq, Show)
> 
> emptyMockStdio :: MockStdio
> emptyMockStdio = MockStdio
>   { _stdin = []
>   , _stdout = []
>   , _stderr = []
>   }

> _read_file
>   :: Chaos -> MockFilesystem -> FilePath
>   -> (Either IOError String, MockFilesystem)
> _read_file chi fs@(MockFilesystem m) path =
>   case FM.lookup path m of
>     Nothing ->
>       (Left (mkIOError doesNotExistErrorType path Nothing Nothing), fs)
>     Just contents ->
>       (Right contents, fs)
> 
> _write_file
>   :: Chaos -> MockFilesystem -> FilePath -> String
>   -> (Maybe IOError, MockFilesystem)
> _write_file chi fs@(MockFilesystem m) path contents =
>   if rareEvent chi
>     then
>       let f x = (Just (mkIOError x path Nothing Nothing), fs)
>       in case residueMod 3 chi of
>         0 -> f fullErrorType
>         1 -> f illegalOperationErrorType
>         _ -> f permissionErrorType
>     else (Nothing, MockFilesystem (FM.upsertAt path contents m))

> _read_stdin
>   :: Chaos -> MockStdio
>   -> (Either IOError String, MockStdio)
> _read_stdin chi std@(MockStdio in' out err) =
>   case in' of
>     [] -> (Left (mkIOError eofErrorType "stdin" Nothing Nothing), std)
>     l:ls -> (Right l, MockStdio ls out err)
> 
> _write_stdout
>   :: Chaos -> MockStdio -> String
>   -> (Maybe IOError, MockStdio)
> _write_stdout chi std@(MockStdio in' out err) l =
>   if rareEvent chi
>     then (Just (mkIOError fullErrorType "stdout" Nothing Nothing), std)
>     else (Nothing, MockStdio in' (out ++ [l]) err)
> 
> _write_stderr
>   :: Chaos -> MockStdio -> String
>   -> (Maybe IOError, MockStdio)
> _write_stderr chi std@(MockStdio in' out err) l =
>   if rareEvent chi
>     then (Just (mkIOError fullErrorType "stderr" Nothing Nothing), std)
>     else (Nothing, MockStdio in' out (err ++ [l]))

> getNextEvent
>   :: ( Monad m )
>   => Mock ev m (Maybe ev)
> getNextEvent = do
>   stream <- _events <$> getMockWorld
>   case stream of
>     [] -> return Nothing
>     e:es -> do
>       alterMockWorld (\w -> w { _events = es })
>       return (Just e)

> getMockFilesystem
>   :: ( Monad m )
>   => Mock ev m MockFilesystem
> getMockFilesystem =
>   _filesystem <$> getMockWorld
> 
> setMockFilesystem
>   :: ( Monad m )
>   => MockFilesystem -> Mock ev m ()
> setMockFilesystem fs =
>   alterMockWorld (\w -> w { _filesystem = fs })

> getMockStdio
>   :: ( Monad m )
>   => Mock ev m MockStdio
> getMockStdio =
>   _stdio <$> getMockWorld
> 
> setMockStdio
>   :: ( Monad m )
>   => MockStdio -> Mock ev m ()
> setMockStdio std =
>   alterMockWorld (\w -> w { _stdio = std })

> fileReaderMock
>   :: ( Monad m )
>   => FileReader (Mock ev m)
> fileReaderMock = FileReader $ \path -> do
>   fs <- getMockFilesystem
>   chi <- getChaos
>   let (result, fs') = _read_file chi fs path
>   setMockFilesystem fs'
>   return result

> fileWriterMock
>   :: ( Monad m )
>   => FileWriter (Mock ev m)
> fileWriterMock = FileWriter $ \path contents -> do
>   fs <- getMockFilesystem
>   chi <- getChaos
>   let (result, fs') = _write_file chi fs path contents
>   setMockFilesystem fs'
>   return result

> logWriterMock
>   :: ( Monad m )
>   => LogWriter (Mock ev m)
> logWriterMock = LogWriter $ \sev msg -> do
>   let msgs = map ((show sev) ++) $ lines msg
>   std <- getMockStdio
>   chi <- getChaos
>   let (result, std') = each (_write_stderr chi) std msgs
>   setMockStdio std'
>   return result
>   where
>     each :: (c -> a -> (Maybe b, c)) -> c -> [a] -> (Maybe b, c)
>     each f c x = case x of
>       [] -> (Nothing, c)
>       a:as -> case f c a of
>         (Nothing, c') -> each f c' as
>         (Just b, c') -> (Just b, c')

