{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module MapTraveler where

import Control.Monad.Loops
import Control.Monad.State
import Control.Lens

import Data.Geography
import Data.Geography.Styles
import Data.QuadTree
import qualified Maps

-- | State of the travel
data TravelState = TravelState { _tsMap :: Map    -- ^ Traveler's map
                               , _tsPos :: Coords -- ^ Traveler's coordinates
                               } deriving (Show, Read, Eq)

makeLenses ''TravelState

-- | Constant character for representing the traveler
travelerChar :: Char
travelerChar = '@'

-- | Place traveler into matrix presenting map
--
-- TODO: implement placing traveler on map matrixs
makeMapWithTraveler :: Coords -> [String] -> [String]
makeMapWithTraveler (Coords x y) m = m & ix rowIdx .~ newRow
  where
    rowIdx = fromInteger y
    colIdx = fromInteger x
    v = m !! rowIdx
    newRow = v & ix colIdx .~ '@'

-- | Print given state (map with traveler on it)
printState :: TravelState -> IO ()
printState (TravelState m c) = sequence_ . fmap putStrLn . makeBorder . makeMapWithTraveler c $ displayMap boxedStyle m

-- | Convert string to direction
--
-- Acceptable are "wasd" (case insensitive) and "8426",
-- otherwise returns 'Nothing'.
--
-- TODO: implement string to direction

northMapping = ['w', 'W', '8']
westMapping =  ['a', 'A', '4']
southMapping = ['s', 'S', '2']
eastMapping =  ['d', 'D', '6']

stringToDirection :: Char -> Maybe Direction
stringToDirection a 
  | a `elem` northMapping = Just North
  | a `elem` westMapping = Just West
  | a `elem` southMapping = Just South
  | a `elem` eastMapping = Just East
  | otherwise = Nothing


-- | Prompt and get the direction from user input
getIODirection :: IO (Maybe Direction)
getIODirection = do
  putStrLn "What direction do you want to go?"
  x <- getLine
  return . stringToDirection . head $ x

-- | Change the traveler state by moving with given direction
--
-- If target is out of map or not walkable, then it won't change
-- the given state.
--
-- TODO: implement moving
move :: Direction -> TravelState -> TravelState
move d s@(TravelState map pos) = case getElement map x y of
  Just v -> if walkable v then TravelState map newPos else s
  Nothing -> s
  where 
    newPos@(Coords x y) = newCoords d pos

-- | Walk the map by giving directions
--
-- Example of State Monad Transformer
-- It is prepared for you, examine it and change if you want
-- but there is no need to do that...
walkTheMap :: StateT TravelState IO ()
walkTheMap = do
  initSt <- get
  lift $ printState initSt
  whileJust_ (lift getIODirection) $ \direction -> do
    modify (move direction)
    st <- get
    lift $ printState st

-- | Basic interface for map traveler
traveler :: IO ()
traveler = do
  putStrLn "The Map Traveler"
  -- TODO: generate initial x,y (but check if walkable!, optional)
  let ts = TravelState Maps.map01 (Coords 10 0)
  runStateT walkTheMap ts
  putStrLn "Byeee!"

-- | Make border around char matrix
makeBorder :: [String] -> [String]
makeBorder m = top ++ inner ++ bot
  where
    top = ["╔" ++ replicate w '═' ++ "╗"]
    bot = ["╚" ++ replicate w '═' ++ "╝"]
    inner = map (\x -> "║" ++ x ++ "║") m
    w = if null m then 0 else length (head m)

-- Optionally you can improve this
-- (and make it a game as term work?)
-- - load map from file
-- - configuration file/env/args
-- - implement some interaction
-- - use ncurses or GUI
