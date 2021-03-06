> module Kreb.Pandoc.Util where

> import Control.Monad
> import System.FilePath ((</>), addTrailingPathSeparator)
> import System.Directory
> import Data.List ((\\), unfoldr, intercalate)
> import System.Environment

Utilities for working with file paths

> getPWD :: IO FilePath
> getPWD = getEnv "PWD"

> data FileExt = LHS | PDF | SVG | MD | HTML
> 
> instance Show FileExt where
>   show x = case x of
>     LHS  -> "lhs"
>     PDF  -> "pdf"
>     SVG  -> "svg"
>     MD   -> "md"
>     HTML -> "html"

> changeExtensionTo
>   :: FileExt -> FilePath -> FilePath
> changeExtensionTo ext path =
>   case reverse $ splitPath path of
>     [] -> ""
>     d:ds ->
>       let d' = reverse $ dropWhile (/= '.') $ reverse d
>       in unsplitPath $ reverse $ (d' ++ show ext):ds

> splitPath :: FilePath -> [String]
> splitPath = unfoldr f
>   where
>     f :: FilePath -> Maybe (String, FilePath)
>     f x = if null x
>       then Nothing
>       else
>         let (h,t) = span (/= '/') x
>         in case t of
>           '/':t' -> Just (h,t')
>           _      -> Just (h,"")

> collapseDoubleDots :: FilePath -> FilePath
> collapseDoubleDots path =
>   unsplitPath $ f [] (splitPath path)
>   where
>     f :: [String] -> [String] -> [String]
>     f x y = case y of
>       [] -> reverse x
>       b:bs -> if b == ".."
>         then case x of
>           [] -> f [b] bs
>           a:as -> f as bs
>         else f (b:x) bs

> unsplitPath :: [String] -> FilePath
> unsplitPath cs = intercalate "/" cs

> catPath :: FilePath -> FilePath -> FilePath
> catPath x y = x ++ "/" ++ y

> removeLastPathComponent
>   :: FilePath -> FilePath
> removeLastPathComponent =
>   unsplitPath . init . splitPath

> stripCommonPrefix
>   :: (Eq a) => [a] -> [a] -> ([a],[a])
> stripCommonPrefix x y = case (x,y) of
>   ([],_) -> ([],y)
>   (_,[]) -> (x,[])
>   (a:as, b:bs) -> if a == b
>     then stripCommonPrefix as bs
>     else (x,y)

> relativize :: FilePath -> FilePath -> FilePath
> relativize to from =
>   let
>     tos = splitPath to
>     froms = splitPath from
>     (st, sf) = stripCommonPrefix tos froms
>     tail' [] = []
>     tail' (_:as) = as
>   in unsplitPath $ (map (const "..") (tail' sf)) ++ st

taken from fsutils

> -- | We can use this data type to represent the pieces of a directory.
> data Directory = Directory
>   { -- | The path of the directory itself.
>     dirPath :: FilePath
>     -- | All subdirectories of this directory.
>   , subDirs :: [FilePath]
>     -- | All files contained in this directory.
>   , files   :: [FilePath]
>   } deriving (Show)

> -- | Creates a Directory instance from a FilePath.
> createDir :: FilePath -> IO Directory
> createDir path = do
>   contents <- topFileList path
>   subdirs  <- filterM doesDirectoryExist contents
>   files    <- filterM doesFileExist contents
>   return (Directory path subdirs files)

> walkDir :: FilePath -> IO [Directory]
> walkDir root = createDir root >>= mtreeList children
>   where children path = do
>           let dirs = subDirs path
>           mapM createDir dirs

> -- | Given a root path, a new root path, and a path to be changed,
> -- removes the old root from the path and replaces it with to.
> replaceRoot :: FilePath -> FilePath -> FilePath -> FilePath
> replaceRoot root to path = to </> removeRoot root path

> -- | Given a root (prefix), remove it from a path. This is useful
> -- for getting the filename and subdirs of a path inside of a root.
> removeRoot :: FilePath -> FilePath -> FilePath
> removeRoot prefix = drop . length $ addTrailingPathSeparator prefix

> -- | Remove useless paths from a list of paths.
> filterUseless :: [FilePath] -> [FilePath]
> filterUseless = (\\ [".", ".."])

> -- | Returns a list of nodes in a tree via a depth-first walk.
> mtreeList :: Monad m => (a -> m [a]) -> a -> m [a]
> mtreeList children root = do
>   xs <- children root
>   subChildren <- mapM (mtreeList children) xs
>   return $ root : concat subChildren

> -- | Get a list of files in path, but not recursively. Removes '.' and '..'.
> topFileList :: FilePath -> IO [FilePath]
> topFileList path =
>   fmap (map (path </>) . filterUseless) $ getDirectoryContents path

> copyDir :: FilePath -> FilePath -> IO ()
> copyDir from to = do
>   createDirectoryIfMissing True to
>   walked <- walkDir from
>   forM_ walked $ \(Directory _ dirs files) -> do
>     mapM_ (createDirectoryIfMissing True . replaceRoot from to) dirs
>     forM_ files $ \path -> copyFile path (replaceRoot from to path)
