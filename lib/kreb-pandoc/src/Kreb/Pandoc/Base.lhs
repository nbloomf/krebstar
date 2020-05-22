> module Kreb.Pandoc.Base where

> import qualified Data.Text as T
> import qualified Data.Text.Encoding as T
> import qualified Data.Text.IO as T
> import qualified Data.ByteString.Lazy as LB
> import           System.Exit

> import qualified Control.Monad.Trans.Except as E
> import qualified Control.Monad.Trans.State.Strict as S
> import qualified Text.Pandoc as P

> mainMD
>   :: (P.Pandoc -> P.PandocIO P.Pandoc)
>   -> IO ()
> mainMD f = do
>   T.getContents
>     >>= mainMarkdownFilter f
>     >>= T.putStr

> mainMarkdownFilter
>   :: (P.Pandoc -> P.PandocIO P.Pandoc)
>   -> T.Text -> IO T.Text
> mainMarkdownFilter f txt =
>   runPandocIO (runMarkdownFilter f txt)

> runMarkdownFilter
>   :: ( P.PandocMonad m )
>   => (P.Pandoc -> m P.Pandoc)
>   -> T.Text -> m T.Text
> runMarkdownFilter f txt =
>   readMD txt
>     >>= f
>     >>= P.writeMarkdown P.def

> readMD
>   :: ( P.PandocMonad m )
>   => T.Text -> m P.Pandoc
> readMD = P.readMarkdown opts
>   where
>     opts = P.def
>       { P.readerExtensions = P.extensionsFromList
>           [ P.Ext_fenced_code_attributes	
>           , P.Ext_fenced_code_blocks
>           , P.Ext_literate_haskell
>           ]
>       }

> writeHtml
>   :: ( P.PandocMonad m )
>   => P.Pandoc -> m T.Text
> writeHtml = P.writeHtml5String opts
>   where
>     opts = P.def
>       { P.writerHTMLMathMethod =
>           P.MathJax "https://cdn.jsdelivr.net/npm/mathjax@3/es5/"
>       , P.writerSectionDivs = True
>       }

> testReadMD
>   :: String -> IO ()
> testReadMD str = do
>   doc <- runPandocIO $ readMD $ T.pack str
>   putStrLn $ show doc

> runPandocIO :: P.PandocIO a -> IO a
> runPandocIO x = do
>   result <- P.runIO x
>   case result of
>     Right a -> return a
>     Left err -> do
>       putStrLn $ show err
>       exitFailure

> liftIO :: IO a -> P.PandocIO a
> liftIO x = P.PandocIO $ E.ExceptT $ S.StateT $ \cs -> do
>   { a <- x; return (Right a, cs) }

