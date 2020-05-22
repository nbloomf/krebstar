> module Kreb.Lang.Runtime where

> import Prelude hiding (Word)

> import Control.Monad (ap)
> import qualified Data.Map as M

> import Kreb.Lang.PrettyPrint
> import Kreb.Lang.Loc
> import Kreb.Lang.Error
> import Kreb.Lang.Expr
> import Kreb.Lang.Type hiding (getTypeEnv)
> import Kreb.Lang.Value

> data Actions r m = Actions
>   { _atomActions    :: M.Map Atom (Runtime r m ())
>   , _builtinActions :: r -> BuiltIn -> Runtime r m ()
>   }

> data RuntimeState r m = RuntimeState
>   { _rtActions :: Actions r m
>   , _rtTypes   :: TypeEnv
>   , _rtStack   :: DataStack
>   }

> data Runtime r m a = Runtime
>   { unRuntime
>       :: RuntimeState r m
>       -> m (Either ReplError (a, RuntimeState r m))
>   }

> initRuntimeState
>   :: ( Monad m )
>   => (r -> String -> Maybe (Runtime r m ()))
>   -> (String -> Maybe Scheme)
>   -> RuntimeState r m
> initRuntimeState act arrow = RuntimeState
>   { _rtActions = Actions
>     { _atomActions = mempty
>     , _builtinActions = builtinActions act
>     }
>   , _rtTypes = TypeEnv mempty (builtinTypes arrow)
>   , _rtStack = Empty
>   }

> instance
>   ( Monad m
>   ) => Functor (Runtime r m)
>   where
>     fmap f x = Runtime $ \st0 -> do
>       r <- unRuntime x st0
>       return $ do
>         (a, st1) <- r
>         return (f a, st1)
> 
> instance
>   ( Monad m
>   ) => Applicative (Runtime r m)
>   where
>     pure x = Runtime $ \st -> do
>       return $ Right (x, st)
> 
>     (<*>) = ap
> 
> instance
>   ( Monad m
>   ) => Monad (Runtime r m)
>   where
>     return = pure
> 
>     x >>= f = Runtime $ \st0 -> do
>       r <- unRuntime x st0
>       case r of
>         Left err -> return $ Left err
>         Right (a, st1) -> unRuntime (f a) st1

> runRuntime
>   :: ( Monad m )
>   => Runtime r m a -> RuntimeState r m
>   -> m (Either ReplError (a, RuntimeState r m))
> runRuntime (Runtime x) st = x st

> evalRuntime
>   :: ( Monad m )
>   => Runtime r m a -> RuntimeState r m
>   -> m (Either ReplError a)
> evalRuntime (Runtime x) st =
>   fmap (fmap fst) $ x st


> getActions
>   :: ( Monad m )
>   => Runtime r m (Actions r m)
> getActions = Runtime $ \st ->
>   return $ Right (_rtActions st, st)
> 
> getTypes
>   :: ( Monad m )
>   => Runtime r m TypeEnv
> getTypes = Runtime $ \st ->
>   return $ Right (_rtTypes st, st)
> 
> popStack
>   :: ( Monad m )
>   => Runtime r m Val
> popStack = Runtime $ \st ->
>   let stk = _rtStack st in
>   return $ case stk of
>     Empty -> Left $ RTE EmptyStack
>     Cons stk2 val -> Right (val, st { _rtStack = stk2 })

> pushStack
>   :: ( Monad m )
>   => Val -> Runtime r m ()
> pushStack val = Runtime $ \st ->
>   let stk = _rtStack st in
>   return $ Right ((), st { _rtStack = Cons stk val })

> popString
>   :: ( Monad m )
>   => Runtime r m String
> popString = do
>   val <- popStack
>   case val of
>     V_Prim (Prim_String str) -> return str
>     _ -> throwErr $ RTE $ TypePanic "String"

> pushString
>   :: ( Monad m )
>   => String -> Runtime r m ()
> pushString str =
>   pushStack (V_Prim (Prim_String str))

> popNat
>   :: ( Monad m )
>   => Runtime r m Int
> popNat = do
>   val <- popStack
>   case val of
>     V_Prim (Prim_Int nat) -> return nat
>     _ -> throwErr $ RTE $ TypePanic "Int"

> popSuspension
>   :: ( Monad m )
>   => Runtime r m [Sus]
> popSuspension = do
>   val <- popStack
>   case val of
>     V_Quote sus -> return sus
>     _ -> throwErr $ RTE ExpectedQuote

> mutateActions
>   :: ( Monad m )
>   => (Actions r m -> Actions r m) -> Runtime r m ()
> mutateActions f = Runtime $ \st ->
>   let
>     acts = _rtActions st
>     st1 = st { _rtActions = f acts }
>   in return $ Right ((), st1)
> 
> mutateTypes
>   :: ( Monad m )
>   => (TypeEnv -> TypeEnv) -> Runtime r m ()
> mutateTypes f = Runtime $ \st ->
>   let
>     env = _rtTypes st
>     st1 = st { _rtTypes = f env }
>   in return $ Right ((), st1)
> 
> mutateStack
>   :: ( Monad m )
>   => (DataStack -> Either RuntimeError DataStack) -> Runtime r m ()
> mutateStack f = Runtime $ \st ->
>   case f (_rtStack st) of
>     Left err -> return $ Left $ RTE err
>     Right stk2 -> return $ Right ((), st { _rtStack = stk2 })

> throwErr
>   :: ( Monad m )
>   => ReplError -> Runtime r m a
> throwErr err = Runtime $ \_ ->
>   return (Left err)



> compileAction
>   :: ( Monad m )
>   => r -> Phrase -> Runtime r m (Runtime r m ())
> compileAction r x = case x of
>   Silence -> return (return ())
>   Then word rest -> do
>     act1 <- lookupActionFor r word
>     act2 <- compileAction r rest
>     return (act1 >> act2)

> lookupActionFor
>   :: ( Monad m )
>   => r -> Word -> Runtime r m (Runtime r m ())
> lookupActionFor r word = case word of
>   Noop -> return (return ())
>   Quote phrase -> return $ mutateStack $ \stk ->
>     Right $ Cons stk (V_Quote [Sus_Say phrase])
>   Only atom -> do
>     dict <- _atomActions <$> getActions
>     case M.lookup atom dict of
>       Nothing -> throwErr $ RTE $
>         WordNotDefined atom
>       Just act -> return act
>   BuiltIn builtin -> do
>     dict <- _builtinActions <$> getActions <*> pure r
>     return $ dict builtin


> doActionFor
>   :: ( Monad m )
>   => r -> Phrase -> Runtime r m ()
> doActionFor r phrase = do
>   action <- compileAction r phrase
>   action

> inferType
>   :: ( Monad m )
>   => Phrase -> Runtime r m Scheme
> inferType phrase = Runtime $ \st -> do
>   let env = _rtTypes st
>   case runInfer env $ infer phrase of
>     Left err -> return $ Left $ TE err
>     Right arr -> return $ Right (arr, st)

> checkIfTyped
>   :: ( Monad m )
>   => Atom -> Runtime r m ()
> checkIfTyped atom =
>   Runtime $ \st -> do
>     let env = atomEnv $ _rtTypes st
>     if M.member atom env
>     then return $ Left $ SE $
>       WordAlreadyTyped atom
>     else return $ Right ((), st)
> 
> checkIfDefined
>   :: ( Monad m )
>   => Atom -> Runtime r m ()
> checkIfDefined atom = Runtime $ \st -> do
>   let dict = _atomActions $ _rtActions st
>   if M.member atom dict
>     then return $ Left $ SE $
>       WordAlreadyDefined atom
>     else return $ Right ((), st)

> defineAtom
>   :: ( Monad m )
>   => Atom -> Scheme -> Runtime r m () -> Runtime r m ()
> defineAtom atom arr act = do
>   mutateActions $ \dict -> dict
>     { _atomActions = M.insert atom act $ _atomActions dict }
>   mutateTypes $ \env -> env
>     { atomEnv = M.insert atom arr $ atomEnv env}




> checkType
>   :: ( Monad m )
>   => Phrase -> Scheme -> Runtime r m ()
> checkType phrase arr1 = Runtime $ \st -> do
>   let env = _rtTypes st
>   case runInfer env $ infer phrase of
>     Left err -> return $ Left $ TE err
>     Right arr2 ->
>       if arr1 /= arr2
>         then return $ Left $ SE $
>           SignatureMismatch arr1 arr2
>         else return $ Right ((), st)






> builtinActions
>   :: ( Monad m )
>   => (r -> String -> Maybe (Runtime r m ())) -> r -> BuiltIn -> Runtime r m ()
> builtinActions custom eId z = case z of
>   BuiltIn_Int k -> mutateStack $ \stk ->
>     Right $ Cons stk (V_Prim $ Prim_Int k)
>   BuiltIn_Char c -> mutateStack $ \stk ->
>     Right $ Cons stk (V_Prim $ Prim_Char c)
>   BuiltIn_String s -> mutateStack $ \stk ->
>     Right $ Cons stk (V_Prim $ Prim_String s)
> 
>   BuiltIn_Int_Plus -> mutateStack $ \stk ->
>     case stk of
>       Cons (Cons stk2 (V_Prim (Prim_Int a))) (V_Prim (Prim_Int b)) ->
>         Right $ Cons stk2 (V_Prim $ Prim_Int (a+b))
>       _ -> Left $ TypePanic "Int"
> 
>   BuiltIn_Int_Times -> mutateStack $ \stk ->
>     case stk of
>       Cons (Cons stk2 (V_Prim (Prim_Int a))) (V_Prim (Prim_Int b)) ->
>         Right $ Cons stk2 (V_Prim $ Prim_Int (a*b))
> 
>   BuiltIn_String_Concat -> do
>     str1 <- popString
>     str2 <- popString
>     pushString (str1 ++ str2)
> 
>   BuiltIn_Id -> return ()
> 
>   BuiltIn_Swap -> mutateStack $ \stk ->
>     case stk of
>       Cons (Cons st b) a -> Right $ Cons (Cons st a) b
>       _ -> Left StackHeadMismatch
> 
>   BuiltIn_Apply -> do
>     val <- popStack
>     case val of
>       V_Quote s -> mapM_ (applySuspension eId) s
>       _ -> throwErr $ RTE $ ExpectedQuote
> 
>   BuiltIn_Repeat -> do
>     num <- popNat
>     sus <- popSuspension
>     sequence_ $ replicate num (mapM_ (applySuspension eId) sus)
> 
>   BuiltIn_Quote -> mutateStack $ \stk ->
>     case stk of
>       Cons st a -> Right $ Cons st (V_Quote [Sus_Put a])
>       _ -> Left StackHeadMismatch
> 
>   BuiltIn_Compose -> mutateStack $ \stk ->
>     case stk of
>       Cons (Cons st (V_Quote s1)) (V_Quote s2) -> Right $ Cons st (V_Quote (s1 ++ s2))
>       _ -> Left StackHeadMismatch
> 
>   BuiltIn_Ext str ->
>     case custom eId str of
>       Nothing -> throwErr $ RTE $ BuiltInUnrecognized str
>       Just act -> act

> applySuspension
>   :: ( Monad m ) => r -> Sus -> Runtime r m ()
> applySuspension r x = case x of
>   Sus_Put v -> mutateStack $ \stk -> Right $ Cons stk v
>   Sus_Say p -> doActionFor r p

> builtinTypes
>   :: (String -> Maybe Scheme) -> BuiltIn -> Maybe Scheme
> builtinTypes custom z = case z of
>   BuiltIn_Int _ -> Just $
>     ForAll
>       (Vars [V "S"] []) $ Arrow
>       (Stack (V "S") [])
>       (Stack (V "S") [TyCon (C "Int")])
> 
>   BuiltIn_Char _ -> Just $
>     ForAll
>       (Vars [V "S"] []) $ Arrow
>       (Stack (V "S") [])
>       (Stack (V "S") [TyCon (C "Char")])
> 
>   BuiltIn_String _ -> Just $
>     ForAll
>       (Vars [V "S"] []) $ Arrow
>       (Stack (V "S") [])
>       (Stack (V "S") [TyCon (C "String")])
> 
>   BuiltIn_Int_Plus -> Just $
>     ForAll
>       (Vars [V "S"] []) $ Arrow
>       (Stack (V "S") [TyCon (C "Int"), TyCon (C "Int")])
>       (Stack (V "S") [TyCon (C "Int")])
> 
>   BuiltIn_Int_Times -> Just $
>     ForAll
>       (Vars [V "S"] []) $ Arrow
>       (Stack (V "S") [TyCon (C "Int"), TyCon (C "Int")])
>       (Stack (V "S") [TyCon (C "Int")])
> 
>   BuiltIn_String_Concat -> Just $
>     ForAll
>       (Vars [V "S"] []) $ Arrow
>       (Stack (V "S") [TyCon (C "String"), TyCon (C "String")])
>       (Stack (V "S") [TyCon (C "String")])
> 
>   BuiltIn_Id -> Just $
>     ForAll
>       (Vars [V "S"] []) $ Arrow
>       (Stack (V "S") [])
>       (Stack (V "S") [])
> 
>   BuiltIn_Swap -> Just $
>     ForAll
>       (Vars [V "S"] [V "a", V "b"]) $ Arrow
>       (Stack (V "S") [TyVar (V "a"), TyVar (V "b")])
>       (Stack (V "S") [TyVar (V "b"), TyVar (V "a")])
> 
>   BuiltIn_Apply -> Just $
>     ForAll
>       (Vars [V "S", V "R"] []) $ Arrow
>       (Stack (V "S") [TyArr $ Arrow
>         (Stack (V "S") [])
>         (Stack (V "R") [])])
>       (Stack (V "R") [])
> 
>   BuiltIn_Quote -> Just $
>     ForAll
>       (Vars [V "S", V "R"] [V "a"]) $ Arrow
>       (Stack (V "S") [TyVar (V "a")])
>       (Stack (V "S") [TyArr $ Arrow
>         (Stack (V "R") [])
>         (Stack (V "R") [TyVar (V "a")])])
> 
>   BuiltIn_Compose -> Just $
>     ForAll
>       (Vars [V "S", V "T", V "U", V "V"] []) $ Arrow
>       (Stack (V "S")
>         [ TyArr $ Arrow
>           (Stack (V "U") [])
>           (Stack (V "V") [])
>         , TyArr $ Arrow
>           (Stack (V "T") [])
>           (Stack (V "U") [])
>         ])
>       (Stack (V "S")
>         [ TyArr $ Arrow
>           (Stack (V "T") [])
>           (Stack (V "V") [])
>         ])
> 
>   BuiltIn_Repeat -> Just $
>     ForAll
>       (Vars [V "S"] []) $ Arrow
>       (Stack (V "S")
>         [ TyCon $ C "Int"
>         , TyArr $ Arrow
>           (Stack (V "S") [])
>           (Stack (V "S") [])])
>       (Stack (V "S") [])
> 
>   BuiltIn_Ext str -> custom str
