> {-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

> module Kreb.Pandoc.TikZ where

> import           Control.Monad ((>=>))
> import           Control.Exception (try)
> import qualified Data.Text as T
> import qualified Data.Text.Encoding as T
> import qualified Data.Text.IO as T
> import qualified Data.ByteString.Lazy as LB
> import qualified Data.Digest.Pure.MD5 as D
> import           Data.List
> import           System.IO
> import           System.Exit
> import           System.Directory
> import           System.Environment
> import           System.FilePath.Posix (replaceExtension)

> import qualified System.Process.Typed as C
> import qualified Control.Monad.Trans.Except as E
> import qualified Control.Monad.Trans.State.Strict as S
> import qualified Text.Pandoc as P

> import Kreb.Pandoc.Base
> import Kreb.Pandoc.Util


Adapted from Stephen Diehl's cats https://github.com/sdiehl/cats

> convertTikZImages :: FilePath -> P.Pandoc -> P.PandocIO P.Pandoc
> convertTikZImages path = liftIO . P.bottomUpM (doTikZ path)

> processTikZ :: FilePath -> P.Pandoc -> P.PandocIO P.Pandoc
> processTikZ path = liftIO . P.bottomUpM (doTikZ path)

> doTikZ :: FilePath -> P.Block -> IO P.Block
> doTikZ path x = case x of
>   (P.CodeBlock (id, ["tikz"], kvs) blk) -> tikzPipeline path (T.pack blk)
>   _ -> return x

> tikzPipeline :: FilePath -> T.Text -> IO P.Block
> tikzPipeline path =
>   wrapTikZ >=> texToPdf >=> pdfToSvg >=> filenameToImageBlock ("docs/html/" ++ path)

> getOutputDir :: IO FilePath
> getOutputDir = return "images/"

> wrapTikZ :: T.Text -> IO T.Text
> wrapTikZ txt = do
>   pre <- tikzPreamble
>   post <- tikzPostamble
>   return $ T.concat [ pre, "\n", txt, "\n", post ]
> 
> tikzPreamble :: IO T.Text
> tikzPreamble = return $ T.pack $ concat
>   [ "\\documentclass{standalone}\n"
>   , "\\usepackage{amsmath}\n"
>   , "\\usepackage{tikz}\n"
>   , "\\usetikzlibrary{matrix}\n"
>   , "\\begin{document}\n"
>   , "\\begin{tikzpicture}[node distance=3.5cm, auto]\n"
>   , "\\tikzstyle{every node}=[font=\\Large]"
>   ]
> 
> tikzPostamble :: IO T.Text
> tikzPostamble = return $ T.pack $ concat
>   [ "\\end{tikzpicture}\n"
>   , "\\end{document}\n"
>   ]

> getTexCompiler :: IO String
> getTexCompiler = return "pdflatex"

> texToPdf :: T.Text -> IO FilePath
> texToPdf txt = do
>   let
>     input = LB.fromStrict $ T.encodeUtf8 txt
>     jobname = show $ D.md5 input
>     tikzDir = "docs/img/tikz/" ++ jobname ++ "/"
>     texFile = tikzDir ++ jobname ++ ".tex"
>     pdfFile = tikzDir ++ jobname ++ ".pdf"
>   createDirectoryIfMissing True tikzDir
>   T.writeFile texFile txt
>   texCompiler <- getTexCompiler
>   outputDir <- getOutputDir
>   let
>     procConf
>       = C.setStdout C.createPipe
>       $ C.setStderr C.createPipe
>       $ C.proc texCompiler
>           [ "-output-directory", tikzDir
>           , "-jobname", jobname
>           , "-interaction=nonstopmode"
>           , jobname ++ ".tex"
>           ]
>   C.withProcess procConf $ \p -> do
>     result <- try (C.checkExitCode p)
>     case result of
>       Right _ -> do
>         removeFile (tikzDir ++ jobname ++ ".log")
>         removeFile (tikzDir ++ jobname ++ ".aux")
>         return ()
>       Left (err :: C.ExitCodeException) -> do
>         putStrLn $ "!!!ERROR!!!\n while processing tikz block " ++ jobname
>         putStrLn $ show err
>         putStrLn "STDOUT:"
>         hGetContents (C.getStdout p) >>= putStrLn
>         putStrLn "STDERR:"
>         hGetContents (C.getStderr p) >>= putStrLn
>         putStrLn "!!!ERROR!!! There was an error running pdflatex."
>         putStrLn "This almost certainly generated an enormous log,"
>         putStrLn "shown above, which I hope you will find useful."
>         exitFailure
>     return pdfFile

> pdfToSvg :: FilePath -> IO FilePath
> pdfToSvg pdfFile = do
>   pwd <- getEnv "PWD"
>   let
>     svgFile = replaceExtension pdfFile "svg"
> 
>     procConf
>       = C.setStdout C.createPipe
>       $ C.setStderr C.createPipe
>       $ C.proc "inkscape"
>           [ "--without-gui"
>           , "--export-plain-svg"
>           , pwd ++ "/" ++ svgFile
>           , pwd ++ "/" ++ pdfFile
>           ]
>   C.withProcess procConf $ \p -> do
>     result <- try (C.checkExitCode p)
>     case result of
>       Right _ -> return ()
>       Left (err :: C.ExitCodeException) -> do
>         putStrLn ("!!!ERROR!!! inkscape barfed")
>         putStrLn $ show err
>         putStrLn "STDOUT:"
>         hGetContents (C.getStdout p) >>= putStrLn
>         putStrLn "STDERR:"
>         hGetContents (C.getStderr p) >>= putStrLn
>         putStrLn "!!!ERROR!!! There was an error running inkscape."
>         putStrLn "This almost certainly generated an enormous log,"
>         putStrLn "shown above, which I hope you will find useful."
>         exitFailure
>     return svgFile

> filenameToImageBlock :: FilePath -> FilePath -> IO P.Block
> filenameToImageBlock hostpath imgpath = do
>   let relativePath = relativize imgpath hostpath
>   return $ P.Para [P.Image ("", ["tikz"], []) [] (relativePath, "")]
