> module Kreb.Lang.Module where

> import Prelude hiding (Word)
> import Control.Monad ((>=>))

> import qualified Data.Map as M
> import qualified Data.Set as S

> import Kreb.Lang.Loc
> import Kreb.Lang.Error
> import Kreb.Lang.Expr
> import Kreb.Lang.Type
> import Kreb.Lang.Value
> import Kreb.Lang.Runtime


> data Decl
>   = Definition Atom Phrase Scheme
>   | DeclareData (String, [Type]) [(Atom, [Type])]
>   deriving (Eq, Show)

> data Module
>   = Module [Decl]
>   deriving (Eq, Show)

> data ProcessError
>   = ProcessError
>   | St RuntimeError
>   | InferenceError Atom Err
>   | TypeSigMismatch Scheme Scheme
>   deriving (Eq, Show)




> applyDecl
>   :: ( Monad m )
>   => Decl -> Runtime m ()
> applyDecl decl = case decl of
>   Definition atom phrase arr1 -> do
>     checkIfDefined atom
>     checkType phrase arr1
>     act <- compileAction phrase
>     defineAtom atom arr1 act

> {-  DeclareData f as cs -> do
>     case validateDataDeclVars (S.fromList as) (concatMap snd cs) of
>       Just bad -> throwSourceError $ ExtraneousVars bad
>       Nothing -> case validateDataDeclNames (map fst cs) of
>         Just bad -> throwSourceError $ DuplicateDataConst bad
>         Nothing -> do
>           sequence_ $ map (checkIfDefined . fst) cs
>           sequence_ $ map (checkIfTyped . fst) cs
>           let
>             g (k,zs) = do
>               t <- funcType f as (k, zs)
>               let e = funcEval k (length zs)
>               defineWord k mempty t e
>           mapM_ g cs -}


> applyDecls
>   :: ( Monad m )
>   => [Decl] -> Runtime m ()
> applyDecls =
>   sequence_ . map applyDecl



> {-


> validateDataDeclNames :: [Var Word] -> Maybe [Var Word]
> validateDataDeclNames ns = case dupe ns of
>   [] -> Nothing
>   ms -> Just ms
>   where
>     dupe xs = case xs of
>       [] -> []
>       a:as -> if elem a as then a : dupe as else dupe as

> validateDataDeclVars :: S.Set (Var MonoType) -> [MonoType] -> Maybe [Var MonoType]
> validateDataDeclVars ok ts =
>   case fmap concat $ sequence $ map f ts of
>     Just [] -> Nothing
>     z -> z
>     where
>       f t =
>         let bad = S.toList $ S.difference (freeTypeVars t) ok in
>         case bad of
>           [] -> Nothing
>           _  -> Just bad



1. Check that the word has not already been defined.
2. Verify that the signature matches the inferred type.
3. Add the definition to the dictionary and the type environment.



> -}


