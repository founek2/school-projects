module Data.Shapes where

--------------------------------------------------------------------------------
-- DO NOT CHANGE DATA TYPES DEFINITIONS

newtype Circle = Circle {ciRadius :: Double}
  deriving (Show, Read, Eq)

data Triangle
  = EquilateralTriangle {etSide :: Double}
  | IsoscelesTriangle {itBase :: Double, itLeg :: Double}
  | ScaleneTriangle {stSideA :: Double, stSideB :: Double, stSideC :: Double}
  deriving (Show, Read, Eq)

data Quadrilateral
  = Square {sqSide :: Double}
  | Rectangle {reSideA :: Double, reSideB :: Double}
  deriving (Show, Read, Eq)

--------------------------------------------------------------------------------

class Validable a where
  valid :: a -> Bool
  valid = undefined

-- TODO: complete instances for each type to check validity by `valid` function
instance Validable Circle where
  valid Circle {ciRadius = radius} = radius > 0

instance Validable Triangle where
  valid EquilateralTriangle {etSide = a} = a > 0
  valid IsoscelesTriangle {itBase = itBase, itLeg = itLeg} = itBase > 0 && itBase <= 2 * itLeg
  valid ScaleneTriangle {stSideA = a, stSideB = b, stSideC = c} = a + b > c && a + c > b && b + c > a

instance Validable Quadrilateral where
  valid Square {sqSide = a} = a > 0
  valid Rectangle {reSideA = a, reSideB = b} = a > 0 && b > 0

-- TODO: create appropriate typeclass for 2D shapes (subclass of Validable)
-- TODO: write instances for the types to compute circumference and area

class Validable a => Shape2D a where
  area :: a -> Double
  area = undefined

  circumference :: a -> Double
  circumference = undefined

instance Shape2D Circle where
  area Circle {ciRadius = radius} = pi * radius ^ 2
  circumference Circle {ciRadius = radius} = 2 * pi * radius

instance Shape2D Triangle where
  area s@EquilateralTriangle {etSide = a}
    | valid s = (a * v) / 2
    | otherwise = 0
    where
      v = (a * sqrt 3) / 2
  area s@IsoscelesTriangle {itBase = b, itLeg = a}
    | valid s = b / 4 * sqrt (4 * a ^ 2 - b ^ 2)
    | otherwise = 0
  area s@ScaleneTriangle {stSideA = a, stSideB = b, stSideC = c}
    | valid s = sqrt (d * (d - a) * (d - b) * (d - c))
    | otherwise = 0
    where
      d = (a + b + c) / 2

  circumference s@EquilateralTriangle {etSide = a}
    | valid s = 3 * a
    | otherwise = 0
  circumference s@IsoscelesTriangle {itBase = b, itLeg = a}
    | valid s = b + 2 * a
    | otherwise = 0
  circumference s@ScaleneTriangle {stSideA = a, stSideB = b, stSideC = c}
    | valid s = a + b + c
    | otherwise = 0

instance Shape2D Quadrilateral where
  area s@Square {sqSide = a}
    | valid s = a ^ 2
    | otherwise = 0
  area s@Rectangle {reSideA = a, reSideB = b}
    | valid s = a * b
    | otherwise = 0

  circumference s@Square {sqSide = a}
    | valid s = 4 * a
    | otherwise = 0
  circumference s@Rectangle {reSideA = a, reSideB = b}
    | valid s = 2 * (a + b)
    | otherwise = 0