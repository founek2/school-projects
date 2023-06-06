{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.QuadTree where

import Control.Lens hiding (Empty)

-- | Quadtree data structure
--
--  - Top left corner is x=0 y=0
--  - Top right quadrant can be bigger and bottom left smaller
--
-- See <https://en.wikipedia.org/wiki/Quadtree wikipedia>
data QuadTree a = QuadTree
  { -- | Quad tree data
    _qtTree :: Quadrant a,
    -- | Width of the encoded rectange (or square)
    _qtWidth :: Integer,
    -- | Height of the encoded rectange (or square)
    _qtHeight :: Integer
  }
  deriving (Eq, Show, Read)

-- | Quadtree's quadrant
data Quadrant a
  = -- | Empty quadrant
    Empty
  | -- | Quadrant filled with one element
    Leaf a
  | Node
      { -- | Top left quadrant
        _qaTL :: Quadrant a,
        -- | Top right quadrant
        _qaTR :: Quadrant a,
        -- | Bottom left quadrant
        _qaBL :: Quadrant a,
        -- | Bottom right quadrant
        _qaBR :: Quadrant a
      }
  deriving (Eq, Show, Read)

makeLenses ''QuadTree
makeLenses ''Quadrant

apply :: (a -> b) -> Quadrant a -> Quadrant b
apply _ Empty = Empty
apply f (Leaf a) = Leaf (f a)
apply f (Node tl tr bl br) = Node (x tl) (x tr) (x bl) (x br) where x = apply f

instance Functor QuadTree where
  fmap f = over qtTree $ fmap f

instance Foldable QuadTree where
  -- TODO: implement foldr or foldMap
  foldr f d q@(QuadTree t w h) = foldr (\(x, y) acc -> f (getElementA q x y) acc) d coordinates where coordinates = [(y, x) | x <- [0 .. w - 1], y <- [0 .. h - 1]]

-- This was the hardest thing I ever implemented during this course. Implementation is super easy but sooooo hard to comprehend -> it should be done on multiple steps with tests for operators <$> and <*>.
-- I spend over 3 hours just figuring out how exactly it should work and how to achieve this
-- Definitely recommending including this website as helper https://github.com/xnning/haskell-programming-from-first-principles/blob/master/notes/traversable.md
--                      -> it helped me to understand how it should work and there are many simple examples on which I could test it and play around with it
instance Traversable QuadTree where
  -- TODO: implement traverse or sequenceA
  traverse f q@(QuadTree Empty w h) = pure (QuadTree Empty w h)
  traverse f q@(QuadTree (Leaf v) w h) = (\x -> QuadTree (Leaf x) w h) <$> f v
  traverse f q@(QuadTree (Node tl tr bl br) w h) =
    (\(QuadTree tl _ _) (QuadTree tr _ _) (QuadTree bl _ _) (QuadTree br _ _) -> QuadTree (Node tl tr bl br) w h)
      <$> traverse f (QuadTree tl 0 0)
      <*> traverse f (QuadTree tr 0 0)
      <*> traverse f (QuadTree bl 0 0)
      <*> traverse f (QuadTree br 0 0)

instance Functor Quadrant where
  fmap = apply

-- | Make new 'QuadTree' filled with given element
--
-- TODO: implement
mkQuadTree :: a -> Integer -> Integer -> QuadTree a
mkQuadTree fill w h
  | w == 0 && h == 0 = QuadTree Empty 0 0
  | otherwise = QuadTree (Leaf fill) w h

-- | Check whether 'QuadTree' has given x and y
--
-- TODO: implement
hasElementOn :: QuadTree a -> Integer -> Integer -> Bool
hasElementOn (QuadTree _ w h) x y = x >= 0 && x < w && y >= 0 && y < h

-- | Get element from 'QuadTree' from x and y
--
-- TODO: implement
getElement :: QuadTree a -> Integer -> Integer -> Maybe a
getElement t x y | not $ hasElementOn t x y = Nothing
getElement t x y = Just (getElementA t x y)

getElementA :: QuadTree a -> Integer -> Integer -> a
getElementA (QuadTree (Leaf v) w h) x y = v
getElementA (QuadTree (Node tl tr bl br) w h) x y
  | x < wh && y < hh = getElementA (QuadTree tl wh hh) x y
  | x >= wh && y < hh = getElementA (QuadTree tr wh hh) (x - wh) y
  | x < wh = getElementA (QuadTree bl wh hh) x (y - hh)
  | otherwise = getElementA (QuadTree br wh hh) (x - wh) (y - hh)
  where
    wh = div w 2
    hh = ceiling (fromIntegral h / (2 :: Double))

-- | Convert 'QuadTree' to matrix
--
-- TODO: implement
toMatrix :: QuadTree a -> [[a]]
toMatrix (QuadTree Empty _ _) = [[]]
toMatrix d@(QuadTree t w h) = map (map (uncurry (getElementA d))) coordinates where coordinates = [[(x, y) | x <- [0 .. w - 1]] | y <- [0 .. h - 1]]

-- | Convert matrix to 'QuadTree'
--
-- Should return simplified quad tree
-- Law: toMatrix . fromMatrix == id
-- Law: fromMatrix . toMatrix == id
-- Honestly this was like the most easy thing to do compared to traverse
fromMatrix :: Eq a => [[a]] -> QuadTree a
fromMatrix [] = QuadTree Empty 0 0
fromMatrix [[]] = QuadTree Empty 0 0
fromMatrix [[a]] = QuadTree (Leaf a) 1 1
fromMatrix a | allEqual a = QuadTree (Leaf v) w h
  where
    v = head . head $ a
    w = toInteger . length . head $ a
    h = toInteger . length $ a
fromMatrix a = concatQT (fromMatrix tl) (fromMatrix tr) (fromMatrix bl) (fromMatrix br)
  where
    (tl, tr, bl, br) = toGroups a

concatQT (QuadTree t1 w1 h1) (QuadTree t2 w2 h2) (QuadTree t3 w3 h3) (QuadTree t4 w4 h4) = QuadTree (Node t1 t2 t3 t4) w h
  where
    w = w1 + w2
    h = h1 + h3

splitVertical :: [a] -> ([a], [a])
splitVertical a = splitAt ((length a + 1) `div` 2) a

splitHorizontal :: [a] -> ([a], [a])
splitHorizontal a = splitAt (length a `div` 2) a

splitFold :: [a] -> ([[a]], [[a]]) -> ([[a]], [[a]])
splitFold x (a, b) = (x1 : a, x2 : b) where (x1, x2) = splitHorizontal x

toGroups a = (tl, tr, bl, br)
  where
    (x1, x2) = splitVertical a
    (tl, tr) = foldr splitFold ([], []) x1
    (bl, br) = foldr splitFold ([], []) x2

allEqual :: Eq a => [[a]] -> Bool
allEqual (x : xs) = all (== x) xs

flip3 f x y z = f z y x