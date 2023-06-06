{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Geography where

import Control.Lens
import Data.QuadTree

-- | Coordinates data structure
data Coords = Coords
  { -- | X (horizontal) coordinate
    _x :: Integer,
    -- | Y (vertical) coordinate
    _y :: Integer
  }
  deriving (Show, Read, Eq)

makeLenses ''Coords

-- | Enum for 4 basic directions
data Direction = North | South | West | East
  deriving (Show, Read, Eq, Enum, Bounded)

-- | Enum for map field types
data MapField = Grass | Road | Desert | Water | Obstacle | Tree
  deriving (Show, Read, Eq, Enum, Bounded)

-- | Map is 'QuadTree' of 'MapField's
type Map = QuadTree MapField

-- | Tell if given 'MapField' is walkable
walkable :: MapField -> Bool
walkable Grass = True
walkable Desert = True
walkable Road = True
walkable _ = False

-- | Convert given 'Map' to 'Char' matrix by given transformation
--
-- TODO: implement
displayMap :: (MapField -> Char) -> Map -> [String]
displayMap f m = map (map f) (toMatrix m)

-- | Convert given 'Char' matrix to 'Map' by given transformation
--
-- TODO: implement
loadMap :: (Char -> MapField) -> [String] -> Map
loadMap f t = fromMatrix $ map (map f) t

-- | Update coordinates by moving one step with given direction
--
-- TODO: implement
newCoords :: Direction -> Coords -> Coords
newCoords North (Coords x y) = Coords x (y - 1)
newCoords South (Coords x y) = Coords x (y + 1)
newCoords West (Coords x y) = Coords (x - 1) y
newCoords East (Coords x y) = Coords (x + 1) y

-- | Get direction from two coordinates
--
-- 'Nothing' if cannot pick one of directions
-- TODO: implement
getDirection :: Coords -> Coords -> Maybe Direction
getDirection (Coords x y) (Coords tx ty)
  | x == tx && y > ty = Just North
  | x == tx && y < ty = Just South
  | x > tx && y == ty = Just West
  | x < tx && y == ty = Just East
  | otherwise = Nothing
