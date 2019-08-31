> module Kreb.Editor (
>     primaryEventLoop
>   , module Kreb.Editor.Monad
>   , module Kreb.Editor.Action
>   , module Kreb.Editor.Error
>   , module Kreb.Editor.Event
>   , module Kreb.Editor.Handler
>   , module Kreb.Editor.Panel
>   , module Kreb.Editor.Settings
>   , module Kreb.Editor.State
>   , module Kreb.Editor.Tab
>   , module Kreb.Editor.Tile
> ) where

> import Kreb.Editor.Monad
> import Kreb.Editor.Action
> import Kreb.Editor.Error
> import Kreb.Editor.Event
> import Kreb.Editor.Handler
> import Kreb.Editor.Panel
> import Kreb.Editor.Settings
> import Kreb.Editor.State
> import Kreb.Editor.Tab
> import Kreb.Editor.Tile



> primaryEventLoop
>   :: ( Monad m )
>   => App m (Maybe AppError)
> primaryEventLoop = loop'
>   where
>     loop' = do
>       _renderState
>       next <- _getNextEvent >>= _handleEvent
>       _logDebugMessages
>       case next of
>         Bail err -> return (Just err)
>         Stop     -> return Nothing
>         GoOn     -> loop'
