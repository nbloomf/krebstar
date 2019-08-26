> module Lang.Data.Interpreter where

> import Prelude hiding (Word)

> import qualified Data.Map as M
> import qualified Data.Set as S

> import Lang.Data.Expr
> import Lang.Data.Type
> import Lang.Data.Value
> import Lang.Data.Module
> import Lang.Data.Error
> import Lang.Data.Runtime



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
>   => Command -> Runtime m ()
> interpret cmd = case cmd of
>   Declaration decl -> applyDecl decl
>   Query expr -> doActionFor expr

> interpretAll
>   :: ( Monad m )
>   => [Command] -> Runtime m ()
> interpretAll = mapM_ interpret

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
