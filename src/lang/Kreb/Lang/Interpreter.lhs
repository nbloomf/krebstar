> module Kreb.Lang.Interpreter where

> import Prelude hiding (Word)

> import qualified Data.Map as M
> import qualified Data.Set as S

> import Kreb.Lang.Expr
> import Kreb.Lang.Type
> import Kreb.Lang.Value
> import Kreb.Lang.Module
> import Kreb.Lang.Error
> import Kreb.Lang.Runtime
> import Kreb.Text.Rune



> data Command
>   = Declaration Decl
>   | Query Phrase
>   deriving (Eq, Show)

> data InterpreterError
>   = S RuntimeError
>   | P ProcessError
>   deriving (Eq, Show)

> interpret
>   :: ( Monad m )
>   => EventId -> Command -> Runtime m ()
> interpret eId cmd = case cmd of
>   Declaration decl -> applyDecl eId decl
>   Query expr -> doActionFor eId expr

> interpretAll
>   :: ( Monad m )
>   => EventId -> [Command] -> Runtime m ()
> interpretAll eId = mapM_ (interpret eId)

> {-







> {- interpret
>   :: ( Monad m )
>   => DataStack -> ReplState m -> Command
>   -> m (Either InterpreterError (Maybe DataStack, ReplState m))
> interpret st env cmd = case cmd of
>   Declaration decl ->
>     case processDecl decl env of
>       Left err -> return $ Left $ P err
>       Right env2 -> return $ Right (Nothing, env2)
> 
>   Query expr -> do
>     r <- eval st (_dictionary env) expr
>     case r of
>       Left err -> return $ Left $ S err
>       Right st -> return $ Right (Just st, env)

> interpretAll
>   :: ( Monad m )
>   => DataStack -> ReplState m -> [Command]
>   -> m (Either InterpreterError (Maybe DataStack, ReplState m))
> interpretAll st env x = case x of
>   [] -> return $ Right (Nothing, env)
>   [c] -> interpret st env c
>   c:cs -> do
>     r <- interpret st env c
>     case r of
>       Left err -> return $ Left err
>       Right (z, env2) ->
>         case z of
>           Nothing -> interpretAll st env2 cs
>           Just st2 -> interpretAll st2 env2 cs -}

> -}
