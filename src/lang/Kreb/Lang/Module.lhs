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
> import Kreb.Text.Rune


> data Decl
>   = Definition Atom Phrase Scheme
>   | DeclareData (String, [V Type]) [(Atom, [Type])]
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
>   => EventId -> Decl -> Runtime m ()
> applyDecl eId decl = case decl of
>   Definition atom phrase arr1 -> do
>     checkIfDefined atom
>     checkType phrase arr1
>     act <- compileAction eId phrase
>     defineAtom atom arr1 act
> 
>   DeclareData (f, as) cs -> do
>     -- make sure all variables in the body
>     -- appear in the signature
>     case extraneousDataDeclVars as (concatMap snd cs) of
>       Just bad -> throwErr $ SE $ ExtraneousVars bad
>       -- Make sure the constructor names are
>       -- all distinct
>       Nothing -> case duplicateDataDeclNames (map fst cs) of
>         Just dupes -> throwErr $ SE $ DuplicateDataConst dupes
>         Nothing -> do
>           -- make sure the constructor names do
>           -- not already have definitions or types
>           sequence_ $ map (checkIfDefined . fst) cs
>           sequence_ $ map (checkIfTyped . fst) cs
>           let
>             buildType
>               :: String -> [V Type] -> [Type] -> Scheme
>             buildType str vars args =
>               quantify $ Arrow
>                 (Stack (V "S") (reverse args))
>                 (Stack (V "S") [foldl TyApp (TyCon $ C str) (map TyVar vars)])
> 
>             buildEval
>               :: ( Monad m )
>               => Atom -> Int -> Runtime m ()
>             buildEval (Atom name) k =
>               mutateStack (peelAndStick name k)
> 
>             defineConstructor
>               :: ( Monad m )
>               => (Atom, [Type]) -> Runtime m ()
>             defineConstructor (k,zs) = do
>               let t = buildType f as zs
>               let e = buildEval k (length zs)
>               defineAtom k t e
> 
>           mapM_ defineConstructor cs

> extraneousDataDeclVars
>   :: [V Type] -> [Type] -> Maybe [V Type]
> extraneousDataDeclVars ok ts =
>   let
>     bodyVars = concatMap (_tyVar . getFreeVars) ts
>     bad = filter (\v -> not $ elem v ok) bodyVars
>   in if null bad then Nothing else Just bad
> 
> duplicateDataDeclNames
>   :: [Atom] -> Maybe [Atom]
> duplicateDataDeclNames ns = case dupe ns of
>   [] -> Nothing
>   ms -> Just ms
>   where
>     dupe xs = case xs of
>       [] -> []
>       a:as -> if elem a as
>         then a : dupe as else dupe as

> applyDecls
>   :: ( Monad m )
>   => EventId -> [Decl] -> Runtime m ()
> applyDecls eId =
>   sequence_ . map (applyDecl eId)



> {-

1. Check that the word has not already been defined.
2. Verify that the signature matches the inferred type.
3. Add the definition to the dictionary and the type environment.

> -}


