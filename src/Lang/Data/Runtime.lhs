> module Lang.Data.Runtime where

> import Prelude hiding (Word)

> import Control.Monad (ap)
> import qualified Data.Map as M

> import Lang.Data.PrettyPrint
> import Lang.Data.Loc
> import Lang.Data.Error
> import Lang.Data.Expr
> import Lang.Data.Type hiding (getTypeEnv)
> import Lang.Data.Value

> data Actions m = Actions
>   { _atomActions    :: M.Map Atom (Runtime m ())
>   , _builtinActions :: BuiltIn -> Runtime m ()
>   }

> data RuntimeState m = RuntimeState
>   { _rtActions :: Actions m
>   , _rtTypes   :: TypeEnv
>   , _rtStack   :: DataStack
>   }

> data Runtime m a = Runtime
>   { unRuntime
>       :: RuntimeState m
>       -> m (Either ReplError (a, RuntimeState m))
>   }

> initRuntimeState
>   :: ( Monad m )
>   => (String -> Maybe (Runtime m ()))
>   -> (String -> Maybe Arrow)
>   -> RuntimeState m
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
>   ) => Functor (Runtime m)
>   where
>     fmap f x = Runtime $ \st0 -> do
>       r <- unRuntime x st0
>       return $ do
>         (a, st1) <- r
>         return (f a, st1)
> 
> instance
>   ( Monad m
>   ) => Applicative (Runtime m)
>   where
>     pure x = Runtime $ \st -> do
>       return $ Right (x, st)
> 
>     (<*>) = ap
> 
> instance
>   ( Monad m
>   ) => Monad (Runtime m)
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
>   => Runtime m a -> RuntimeState m
>   -> m (Either ReplError (a, RuntimeState m))
> runRuntime (Runtime x) st = x st


> getActions
>   :: ( Monad m )
>   => Runtime m (Actions m)
> getActions = Runtime $ \st ->
>   return $ Right (_rtActions st, st)
> 
> getTypes
>   :: ( Monad m )
>   => Runtime m TypeEnv
> getTypes = Runtime $ \st ->
>   return $ Right (_rtTypes st, st)
> 
> popStack
>   :: ( Monad m )
>   => Runtime m Val
> popStack = Runtime $ \st ->
>   let stk = _rtStack st in
>   return $ case stk of
>     Empty -> Left $ RTE EmptyStack
>     Cons stk2 val -> Right (val, st { _rtStack = stk2 })

> mutateActions
>   :: ( Monad m )
>   => (Actions m -> Actions m) -> Runtime m ()
> mutateActions f = Runtime $ \st ->
>   let
>     acts = _rtActions st
>     st1 = st { _rtActions = f acts }
>   in return $ Right ((), st1)
> 
> mutateTypes
>   :: ( Monad m )
>   => (TypeEnv -> TypeEnv) -> Runtime m ()
> mutateTypes f = Runtime $ \st ->
>   let
>     env = _rtTypes st
>     st1 = st { _rtTypes = f env }
>   in return $ Right ((), st1)
> 
> mutateStack
>   :: ( Monad m )
>   => (DataStack -> Either RuntimeError DataStack) -> Runtime m ()
> mutateStack f = Runtime $ \st ->
>   case f (_rtStack st) of
>     Left err -> return $ Left $ RTE err
>     Right stk2 -> return $ Right ((), st { _rtStack = stk2 })

> throwErr
>   :: ( Monad m )
>   => ReplError -> Runtime m a
> throwErr err = Runtime $ \_ ->
>   return (Left err)



> compileAction
>   :: ( Monad m )
>   => Phrase -> Runtime m (Runtime m ())
> compileAction x = case x of
>   Silence -> return (return ())
>   Then word rest -> do
>     act1 <- lookupActionFor word
>     act2 <- compileAction rest
>     return (act1 >> act2)

> lookupActionFor
>   :: ( Monad m )
>   => Word -> Runtime m (Runtime m ())
> lookupActionFor word = case word of
>   Quote phrase -> return $ mutateStack $ \stk ->
>     Right $ Cons stk (V_Quote phrase)
>   Only atom -> do
>     dict <- _atomActions <$> getActions
>     case M.lookup atom dict of
>       Nothing -> throwErr $ RTE $
>         WordNotDefined atom
>       Just act -> return act
>   BuiltIn builtin -> do
>     dict <- _builtinActions <$> getActions
>     return $ dict builtin


> doActionFor
>   :: ( Monad m )
>   => Phrase -> Runtime m ()
> doActionFor phrase = do
>   action <- compileAction phrase
>   action

> inferType
>   :: ( Monad m )
>   => Phrase -> Runtime m Arrow
> inferType phrase = Runtime $ \st -> do
>   let env = _rtTypes st
>   case runInfer env $ infer phrase of
>     Left err -> return $ Left $ TE err
>     Right arr -> return $ Right (arr, st)

> checkIfTyped
>   :: ( Monad m )
>   => Atom -> Runtime m ()
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
>   => Atom -> Runtime m ()
> checkIfDefined atom = Runtime $ \st -> do
>   let dict = _atomActions $ _rtActions st
>   if M.member atom dict
>     then return $ Left $ SE $
>       WordAlreadyDefined atom
>     else return $ Right ((), st)

> defineAtom
>   :: ( Monad m )
>   => Atom -> Arrow -> Runtime m () -> Runtime m ()
> defineAtom atom arr act = do
>   mutateActions $ \dict -> dict
>     { _atomActions = M.insert atom act $ _atomActions dict }
>   mutateTypes $ \env -> env
>     { atomEnv = M.insert atom arr $ atomEnv env}


> checkType
>   :: ( Monad m )
>   => Phrase -> Arrow -> Runtime m ()
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
>   => (String -> Maybe (Runtime m ())) -> BuiltIn -> Runtime m ()
> builtinActions custom z = case z of
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
>       V_Quote phrase -> doActionFor phrase
>       _ -> throwErr $ RTE $ ExpectedQuote
> 
>   BuiltIn_Ext str ->
>     case custom str of
>       Nothing -> throwErr $ RTE $ BuiltInUnrecognized str
>       Just act -> act

> builtinTypes
>   :: (String -> Maybe Arrow) -> BuiltIn -> Maybe Arrow
> builtinTypes custom z = case z of
>   BuiltIn_Int _ -> Just $
>     ForAll
>       (Vars [V "S"] [])
>       (Stack (V "S") [])
>       (Stack (V "S") [TyCon (C "Int")])
> 
>   BuiltIn_Char _ -> Just $
>     ForAll
>       (Vars [V "S"] [])
>       (Stack (V "S") [])
>       (Stack (V "S") [TyCon (C "Char")])
> 
>   BuiltIn_String _ -> Just $
>     ForAll
>       (Vars [V "S"] [])
>       (Stack (V "S") [])
>       (Stack (V "S") [TyCon (C "String")])
> 
>   BuiltIn_Int_Plus -> Just $
>     ForAll
>       (Vars [V "S"] [])
>       (Stack (V "S") [TyCon (C "Int"), TyCon (C "Int")])
>       (Stack (V "S") [TyCon (C "Int")])
> 
>   BuiltIn_Int_Times -> Just $
>     ForAll
>       (Vars [V "S"] [])
>       (Stack (V "S") [TyCon (C "Int"), TyCon (C "Int")])
>       (Stack (V "S") [TyCon (C "Int")])
> 
>   BuiltIn_Id -> Just $
>     ForAll
>       (Vars [V "S"] [])
>       (Stack (V "S") [])
>       (Stack (V "S") [])
> 
>   BuiltIn_Swap -> Just $
>     ForAll
>       (Vars [V "S"] [V "a", V "b"])
>       (Stack (V "S") [TyVar (V "a"), TyVar (V "b")])
>       (Stack (V "S") [TyVar (V "b"), TyVar (V "a")])
> 
>   BuiltIn_Apply -> Just $
>     ForAll
>       (Vars [V "S", V "R"] [])
>       (Stack (V "S") [TyArr $
>         ForAll
>           mempty
>           (Stack (V "S") [])
>           (Stack (V "R") [])])
>       (Stack (V "R") [])
> 
>   BuiltIn_Ext str -> custom str











