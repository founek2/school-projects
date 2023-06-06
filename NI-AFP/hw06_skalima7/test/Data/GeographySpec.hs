module Data.GeographySpec (spec) where

import Test.Hspec

import Data.QuadTree
import Data.Geography
import qualified Data.Geography.Styles as Styles


emptyMap :: Map
emptyMap = QuadTree
         { _qtTree   = Empty
         , _qtWidth  = 0
         , _qtHeight = 0
         }

simpleMap :: Map
simpleMap = QuadTree
          { _qtTree   = Leaf Grass
          , _qtWidth  = 4
          , _qtHeight = 2
          }

complexMap1 :: Map
complexMap1 = QuadTree
            { _qtTree   = Node (Leaf Grass) (Leaf Desert) (Leaf Water) (Leaf Tree)
            , _qtWidth  = 8
            , _qtHeight = 6
            }

complexMap2 :: Map
complexMap2 = QuadTree
           { _qtTree   = Node (Leaf Obstacle) (Leaf Road) (Leaf Water) (Leaf Tree)
           , _qtWidth  = 9
           , _qtHeight = 7
           }

spec :: Spec
spec = do
   describe "displayMap + mkCharStyle" $ do
     it "displays empty map" $
       displayMap Styles.simpleStyle emptyMap `shouldBe` [""]
     it "displays simple map" $ do
       displayMap Styles.simpleStyle simpleMap `shouldBe` ["GGGG",
                                                           "GGGG"]
       displayMap Styles.asciiStyle simpleMap  `shouldBe` ["    ",
                                                           "    "]
       displayMap Styles.boxedStyle simpleMap  `shouldBe` ["    ",
                                                           "    "]
     it "displays complex maps" $ do
       displayMap Styles.simpleStyle complexMap1 `shouldBe` ["GGGGDDDD",
                                                             "GGGGDDDD",
                                                             "GGGGDDDD",
                                                             "WWWWTTTT",
                                                             "WWWWTTTT",
                                                             "WWWWTTTT"]
       displayMap Styles.boxedStyle complexMap1  `shouldBe` ["    ░░░░",
                                                             "    ░░░░",
                                                             "    ░░░░",
                                                             "~~~~^^^^",
                                                             "~~~~^^^^",
                                                             "~~~~^^^^"]
       displayMap Styles.simpleStyle complexMap2 `shouldBe` ["OOOORRRRR",
                                                             "OOOORRRRR",
                                                             "OOOORRRRR",
                                                             "OOOORRRRR",
                                                             "WWWWTTTTT",
                                                             "WWWWTTTTT",
                                                             "WWWWTTTTT"]
       displayMap Styles.asciiStyle complexMap2  `shouldBe` ["####+++++",
                                                             "####+++++",
                                                             "####+++++",
                                                             "####+++++",
                                                             "~~~~xxxxx",
                                                             "~~~~xxxxx",
                                                             "~~~~xxxxx"]
   describe "loadMap + mkCharStyle" $ do
     it "loads empty map" $
       loadMap Styles.loadSimpleStyle [""] `shouldBe` emptyMap
     it "loads simple map" $ do
       loadMap Styles.loadSimpleStyle ["GGGG","GGGG"] `shouldBe` simpleMap
       loadMap Styles.loadAsciiStyle ["    ","    "]  `shouldBe` simpleMap
       loadMap Styles.loadBoxedStyle ["    ","    "]  `shouldBe` simpleMap
   describe "newCoords" $ do
     it "can make step to North (UP)" $ do
       newCoords North (Coords 2 2) `shouldBe` Coords 2 1
       newCoords North (Coords 2 (-2)) `shouldBe` Coords 2 (-3)
       newCoords North (Coords 0 5) `shouldBe` Coords 0 4
     it "can make step to South (DOWN)" $ do
       newCoords South (Coords 2 2) `shouldBe` Coords 2 3
       newCoords South (Coords 2 (-2)) `shouldBe` Coords 2 (-1)
       newCoords South (Coords 0 5) `shouldBe` Coords 0 6
     it "can make step to West (LEFT)" $ do
       newCoords West (Coords 2 2) `shouldBe` Coords 1 2
       newCoords West (Coords 2 (-2)) `shouldBe` Coords 1 (-2)
       newCoords West (Coords 0 5) `shouldBe` Coords (-1) 5
     it "can make step to East (RIGHT)" $ do
       newCoords East (Coords 2 2) `shouldBe` Coords 3 2
       newCoords East (Coords 2 (-2)) `shouldBe` Coords 3 (-2)
       newCoords East (Coords 0 5) `shouldBe` Coords 1 5
   describe "getDirection" $ do
     it "computes direction from two different points" $ do
       getDirection (Coords 2 2) (Coords 2 1) `shouldBe` Just North
       getDirection (Coords 2 2) (Coords 2 3) `shouldBe` Just South
       getDirection (Coords 2 2) (Coords 1 2) `shouldBe` Just West
       getDirection (Coords 2 2) (Coords 3 2) `shouldBe` Just East
     it "returns Nothing if points are the same" $
       getDirection (Coords 2 2) (Coords 2 2) `shouldBe` Nothing
     it "returns Nothing if points are in multiple directions" $ do
        getDirection (Coords 2 2) (Coords 1 1) `shouldBe` Nothing
        getDirection (Coords 2 2) (Coords 1 3) `shouldBe` Nothing
        getDirection (Coords 2 2) (Coords 3 1) `shouldBe` Nothing
        getDirection (Coords 2 2) (Coords 3 3) `shouldBe` Nothing
