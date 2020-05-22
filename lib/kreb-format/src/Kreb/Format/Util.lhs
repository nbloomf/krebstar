> module Kreb.Format.Util (
>     textSpaces
> ) where
> 
> import qualified Data.Text as T

> textSpaces :: Int -> T.Text
> textSpaces n = T.replicate n (T.singleton ' ')
