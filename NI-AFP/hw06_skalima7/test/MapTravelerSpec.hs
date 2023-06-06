module MapTravelerSpec (spec) where

import Test.Hspec

import Data.Geography
import qualified Data.Geography.Styles as Styles
import MapTraveler

map3x3A :: Map
map3x3A = loadMap Styles.loadAsciiStyle ["   ","   ","   "]

map3x3B :: Map
map3x3B = loadMap Styles.loadAsciiStyle ["###","# #","###"]

map1x1 :: Map
map1x1 = loadMap Styles.loadAsciiStyle [" "]

spec :: Spec
spec = do
  describe "makeMapWithTraveler" $ do
    it "places traveler on into the matrix" $ do
      makeMapWithTraveler (Coords 0 0) ["   "] `shouldBe` ["@  "]
      makeMapWithTraveler (Coords 1 0) ["   "] `shouldBe` [" @ "]
      makeMapWithTraveler (Coords 0 1) ["   ", "   "] `shouldBe` ["   ", "@  "]
      makeMapWithTraveler (Coords 1 1) ["   ", "   "] `shouldBe` ["   ", " @ "]
    it "ignores bad coords" $ do
      makeMapWithTraveler (Coords 3 3) ["   ", "   "] `shouldBe` ["   ", "   "]
      makeMapWithTraveler (Coords 0 5) ["   ", "   "] `shouldBe` ["   ", "   "]
      makeMapWithTraveler (Coords 5 0) ["   ", "   "] `shouldBe` ["   ", "   "]
      makeMapWithTraveler (Coords (-1) (-1)) ["   ", "   "] `shouldBe` ["   ", "   "]
  describe "stringToDirection" $ do
    it "works with wasd (lower case)" $ do
      stringToDirection 'w' `shouldBe` Just North
      stringToDirection 'a' `shouldBe` Just West
      stringToDirection 's' `shouldBe` Just South
      stringToDirection 'd' `shouldBe` Just East
    it "works with wasd (upper case)" $ do
      stringToDirection 'W' `shouldBe` Just North
      stringToDirection 'A' `shouldBe` Just West
      stringToDirection 'S' `shouldBe` Just South
      stringToDirection 'D' `shouldBe` Just East
    it "works with 8426" $ do
      stringToDirection '8' `shouldBe` Just North
      stringToDirection '4' `shouldBe` Just West
      stringToDirection '2' `shouldBe` Just South
      stringToDirection '6' `shouldBe` Just East
    it "return Nothing otherwise" $ do
      stringToDirection 'e' `shouldBe` Nothing
      stringToDirection ' ' `shouldBe` Nothing
  describe "move" $ do
    it "moves traveler if possible" $ do
      move North (TravelState map3x3A (Coords 1 1)) `shouldBe` TravelState map3x3A (Coords 1 0)
      move South (TravelState map3x3A (Coords 1 1)) `shouldBe` TravelState map3x3A (Coords 1 2)
      move West  (TravelState map3x3A (Coords 1 1)) `shouldBe` TravelState map3x3A (Coords 0 1)
      move East  (TravelState map3x3A (Coords 1 1)) `shouldBe` TravelState map3x3A (Coords 2 1)
    it "doesn't move traveler if not walkable" $ do
      move North (TravelState map3x3B (Coords 1 1)) `shouldBe` TravelState map3x3B (Coords 1 1)
      move South (TravelState map3x3B (Coords 1 1)) `shouldBe` TravelState map3x3B (Coords 1 1)
      move West  (TravelState map3x3B (Coords 1 1)) `shouldBe` TravelState map3x3B (Coords 1 1)
      move East  (TravelState map3x3B (Coords 1 1)) `shouldBe` TravelState map3x3B (Coords 1 1)
    it "doesn't move traveler if out of map" $ do
      move North (TravelState map1x1 (Coords 0 0)) `shouldBe` TravelState map1x1 (Coords 0 0)
      move South (TravelState map1x1 (Coords 0 0)) `shouldBe` TravelState map1x1 (Coords 0 0)
      move West  (TravelState map1x1 (Coords 0 0)) `shouldBe` TravelState map1x1 (Coords 0 0)
      move East  (TravelState map1x1 (Coords 0 0)) `shouldBe` TravelState map1x1 (Coords 0 0)
