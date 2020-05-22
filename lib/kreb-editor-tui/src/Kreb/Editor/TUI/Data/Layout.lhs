> module Kreb.Editor.TUI.Data.Layout where

Represents abstract axis-aligned rectangular regions on the screen; used when interpreting mouse events.

> data Rect = Rect
>   { xPos :: Int
>   , yPos :: Int
>   , xDim :: Int
>   , yDim :: Int
>   } deriving (Eq, Show)

> isInside :: (Int, Int) -> Rect -> Maybe (Int, Int)
> isInside (h,k) r = 
>   let
>     p = and
>       [ xPos r <= h, h < xPos r + xDim r
>       , yPos r <= k, k < yPos r + yDim r
>       ]
>   in if p
>     then Just (h - xPos r, k - yPos r)
>     else Nothing

> data Layout = Layout
>   { _textRect   :: Rect
>   , _cmdRect    :: Rect
>   , _histRect   :: Rect
>   , _statusRect :: Rect
>   } deriving (Eq, Show)

> emptyRect :: Rect
> emptyRect = Rect 0 0 0 0
> 
> emptyLayout :: Layout
> emptyLayout = Layout
>   { _textRect = emptyRect
>   , _cmdRect = emptyRect
>   , _histRect = emptyRect
>   , _statusRect = emptyRect
>   }
