> module Kreb.Pandoc.Anchor where

> import Data.Data
> import Control.Monad.State
> import Control.Monad.Error.Class
> import Control.Monad.Trans.Class

> import           Control.Monad.Loops (concatM)
> import qualified Data.Map.Strict as M
> import qualified Text.Pandoc as P
> import qualified Text.Pandoc.Generic as P

> import Kreb.Pandoc.Util

> data SlugMap =
>   SlugMap (M.Map String (String, String))
>   deriving (Eq, Show)

> showSlugMap :: SlugMap -> IO ()
> showSlugMap (SlugMap m) = do
>   putStrLn $ ">>> Debug: SlugMap (" ++ show (M.size m) ++ " entries)"
>   let
>     f :: (String, (String, String)) -> IO ()
>     f (slug, (url, fragment)) = do
>       putStrLn $ concat
>         [ ">>>   " ++ slug ++ " -> " ++ url ++ fragment
>         ]
>   mapM_ f (M.toList m)
>   putStrLn ""

> expandSlugs
>   :: ( P.PandocMonad m )
>   => FilePath -> SlugMap -> P.Pandoc -> m P.Pandoc
> expandSlugs path m = P.bottomUpM (expandSlug path m)

> expandSlug
>   :: ( P.PandocMonad m )
>   => FilePath -> SlugMap -> P.Inline -> m P.Inline
> expandSlug path (SlugMap m) x = case x of
>   P.Link attr inlines (slug, title) -> if isSlug slug
>     then do
>       case M.lookup slug m of
>         Nothing -> throwError $ P.PandocFilterError "Anchor" $ concat
>           [ "Anchor slug '" ++ slug ++ "' is not defined."
>           ]
>         Just (url, fragment) ->
>           let relativeUrl = relativize url path
>           in return $ P.Link attr inlines (relativeUrl ++ fragment, title)
>     else case inlines of
>       [P.Str w] -> if isSlug w
>         then case M.lookup w m of
>           Just (_,fragment) ->
>             return $ P.RawInline (P.Format "html") ("<a name='" ++ (tail fragment) ++ "'></a>")
>           Nothing -> throwError $ P.PandocFilterError "Anchor" $ concat
>             [ "Cross reference slug '" ++ w ++ "' in '" ++ path ++ "' is"
>             , "not defined."
>             ]
>         else return x
>       _ -> return x
>   _ -> return x



> getSlugMap
>   :: ( P.PandocMonad m )
>   => [(FilePath, P.Pandoc)] -> m SlugMap
> getSlugMap docs = concatM (map updateSlugMap docs) (SlugMap M.empty)

> updateSlugMap
>   :: ( P.PandocMonad m )
>   => (FilePath, P.Pandoc) -> SlugMap -> m SlugMap
> updateSlugMap (path, doc) m =
>   fst <$> execStateT (P.bottomUpM (getSlugs path) doc) (m,0)
>   

> getSlugs
>   :: ( P.PandocMonad m )
>   => FilePath -> P.Inline -> StateT (SlugMap, Int) m P.Inline
> getSlugs path x = case x of
>   P.Link attr inlines target -> case inlines of
>     [P.Str w] -> if isSlug w
>       then do
>         if (not $ null target) && target /= ("","")
>           then lift $ throwError $ P.PandocFilterError "Anchor" $ concat
>              [ "Cross reference slug '" ++ w ++ "' in '" ++ path ++ "' has "
>              , "a nonempty target " ++ show target ++ "; this is probably "
>              , "unintended since it will be overwritten."
>              ]
>           else do
>             SlugMap m <- getMap
>             case M.lookup w m of
>               Just (z,_) -> lift $ throwError $ P.PandocFilterError "Anchor" $ concat
>                 [ "Cross reference slug '" ++ w ++ "' in '" ++ path ++ "' has"
>                 , "already been defined in '" ++ z ++ "'. Slugs must be unique."
>                 ]
>               Nothing -> do
>                 anchor <- insertKey w path
>                 return x
>       else return x
>     _ -> return x
>   _ -> return x

> insertKey :: (Monad m) => String -> String -> StateT (SlugMap,Int) m String
> insertKey slug path = do
>   n <- getCrossrefNumber
>   let anchor = "#crossref-" ++ show n
>   modify' (\(SlugMap m,i) -> (SlugMap $ M.insert slug (path, anchor) m,i))
>   return anchor

> getMap :: (Monad m) => StateT (SlugMap,a) m SlugMap
> getMap = gets fst

> getCrossrefNumber :: (Monad m) => StateT (a, Int) m Int
> getCrossrefNumber = do
>   k <- gets snd
>   modify' (\(a,_) -> (a,k+1))
>   return k

> isSlug :: String -> Bool
> isSlug x = case x of
>   '@':xs -> all (\x -> elem x $ concat [['a'..'z'],['A'..'Z'],['0'..'9'],['-','_']]) xs
>   _ -> False

bottomUpM :: (Monad m, Data a, Data b) => (a -> m a) -> b -> m b
