module Kreb.Editor.Tile (
    Tiled(..)
  , mkTiled

  , getAbsCursorPosTiled
  , alterFocusTiled
  , queryFocusTiled

  , setTiledDim

  , debugShowTiled
) where

data Tiled a
  = Tiled a
  deriving (Eq, Show)

instance Functor Tiled where
  fmap f (Tiled a) = Tiled (f a)

setTiledDim
  :: ((Int, Int) -> a -> (a, (Int, Int)))
  -> (Int, Int) -> Tiled a -> (Tiled a, (Int, Int))
setTiledDim f dim (Tiled x) =
  let (y,z) = f dim x
  in (Tiled y, z)

alterFocusTiled
  :: (a -> a) -> Tiled a -> Tiled a
alterFocusTiled f (Tiled a) = Tiled (f a)

queryFocusTiled
  :: (a -> b) -> Tiled a -> b
queryFocusTiled f (Tiled a) = f a

mkTiled :: a -> Tiled a
mkTiled = Tiled



data Tiling m a
  = Alone a
  | Aside m (Tiling m a) (Tiling m a)
  | Above m (Tiling m a) (Tiling m a)
  deriving (Eq, Show)

data Path m a
  = Top
  | PointAside m Bool (Tiling m a) (Path m a)
  | PointAbove m Bool (Tiling m a) (Path m a)
  deriving (Eq, Show)

data TilingZip m a
  = TilingZip a 

getAbsCursorPosTiled
  :: (a -> (Int, Int))
  -> Tiled a -> (Int, Int)
getAbsCursorPosTiled f (Tiled a) =
  f a



debugShowTiled :: (a -> String) -> Tiled a -> String
debugShowTiled p (Tiled a) = p a
