module Data.QuadTreeSpec (spec) where

import Data.Foldable
import qualified Control.Lens as Lens
import Test.Hspec

import Data.QuadTree
import qualified Maps

emptyQTree :: QuadTree Int
emptyQTree = QuadTree
           { _qtTree   = Empty
           , _qtWidth  = 0
           , _qtHeight = 0
           }

-- 7 7 7 7
-- 7 7 7 7
simpleQTree1 :: QuadTree Int
simpleQTree1 = QuadTree
             { _qtTree   = Leaf 7
             , _qtWidth  = 4
             , _qtHeight = 2
             }

simpleQTree2 :: QuadTree Int
simpleQTree2 = QuadTree
             { _qtTree   = Leaf 49
             , _qtWidth  = 4
             , _qtHeight = 2
             }

--  7  7  7  7  7  0  0  0  0  0
--  7  7  7  7  7  0  0  0  0  0
--  7  7  7  7  7  0  0  0  0  0
--  7  7  7  7  7  0  0  0  0  0
--  7  7  7  7  7  0  0  0  0  0
-- -2 -2 -2 -2 -2  3  3  3  3  3
-- -2 -2 -2 -2 -2  3  3  3  3  3
-- -2 -2 -2 -2 -2  3  3  3  3  3
-- -2 -2 -2 -2 -2  3  3  3  3  3
-- -2 -2 -2 -2 -2  3  3  3  3  3
simpleQTree3 :: QuadTree Int
simpleQTree3 = QuadTree
             { _qtTree   = Node (Leaf 7) (Leaf 0) (Leaf (-2)) (Leaf 3)
             , _qtWidth  = 10
             , _qtHeight = 10
             }

simpleQTree4 :: QuadTree Int
simpleQTree4 = QuadTree
             { _qtTree   = Node (Leaf 14) (Leaf 0) (Leaf (-4)) (Leaf 6)
             , _qtWidth  = 10
             , _qtHeight = 10
             }

simpleQMaybeTree :: QuadTree (Maybe Int)
simpleQMaybeTree = QuadTree
            { _qtTree   = Node (Leaf (Just 7)) (Leaf (Just 0)) (Leaf Nothing) (Leaf (Just 2))
            , _qtWidth  = 7
            , _qtHeight = 5
            }

-- 14 14 14 14 14  0  0  0  0  0
-- 14 14 14 14 14  0  0  0  0  0
-- 14 14 14 14 14  0  0  0  0  0
-- 14 14 14 14 14  0  0  0  0  0
-- 14 14 14 14 14  0  0  0  0  0
--  3  3 15 15 15  6  6  6  6  6
--  3  3 15 15 15  6  6  6  6  6
--  3  3 15 15 15  6  6  6  6  6
--  5  5 -4 -4 -4  6  6  6  6  6
--  5  5 -4 -4 -4  6  6  6  6  6
complexQTree1 :: QuadTree Int
complexQTree1 = QuadTree
              { _qtTree   = Node (Leaf 14) (Leaf 0) (Node (Leaf 3) (Leaf 15) (Leaf 5) (Leaf (-4))) (Leaf 6)
              , _qtWidth  = 10
              , _qtHeight = 10
              }

complexQTree2 :: QuadTree Int
complexQTree2 = QuadTree
             { _qtTree   = Node (Leaf 9) (Leaf (-5)) (Node (Leaf (-2)) (Leaf 10) (Leaf 0) (Leaf (-9))) (Leaf 1)
             , _qtWidth  = 10
             , _qtHeight = 10
             }

complexMatrix2 :: [[Int]]
complexMatrix2 = [[9,9,9,9,9,-5,-5,-5,-5,-5],
                  [9,9,9,9,9,-5,-5,-5,-5,-5],
                  [9,9,9,9,9,-5,-5,-5,-5,-5],
                  [9,9,9,9,9,-5,-5,-5,-5,-5],
                  [9,9,9,9,9,-5,-5,-5,-5,-5],
                  [-2,-2,10,10,10,1,1,1,1,1],
                  [-2,-2,10,10,10,1,1,1,1,1],
                  [-2,-2,10,10,10,1,1,1,1,1],
                  [ 0, 0,-9,-9,-9,1,1,1,1,1],
                  [ 0, 0,-9,-9,-9,1,1,1,1,1]]

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

spec :: Spec
spec = do
  describe "QuadTree" $ do
    it "is instance of Functor" $ do
      fmap (div 5) emptyQTree `shouldBe` emptyQTree
      fmap (^2) simpleQTree1 `shouldBe` simpleQTree2
      fmap (*2) simpleQTree3 `shouldBe` simpleQTree4
      fmap (\x -> x-5) complexQTree1 `shouldBe` complexQTree2
    it "is instance of Foldable (various folds)" $ do
      null emptyQTree `shouldBe` True
      null complexQTree2 `shouldBe` False
      sum simpleQTree3 `shouldBe` (5^2 * (7 + 0 - 2 + 3))
      sum complexQTree1 `shouldBe` (5^2 * (14 + 0 + 6)) + 6 * 3 + 9 * 15 + 4 * 5 + 6 * (-4)
      maximum complexQTree1 `shouldBe` 15
      length simpleQTree1 `shouldBe` 8
      length complexQTree2 `shouldBe` 100
      elem 12 complexQTree2 `shouldBe` False
      elem 12 emptyQTree `shouldBe` False
      elem 12 simpleQTree1 `shouldBe` False
      elem 10 complexQTree2 `shouldBe` True
      elem 7 simpleQTree1 `shouldBe` True
    it "is instance of Foldable (toList)" $ do
      toList emptyQTree `shouldBe` []
      toList simpleQTree1 `shouldBe` concat [[7,7,7,7],
                                             [7,7,7,7]]
      toList simpleQTree3 `shouldBe` concat [[7,7,7,7,7,0,0,0,0,0],
                                             [7,7,7,7,7,0,0,0,0,0],
                                             [7,7,7,7,7,0,0,0,0,0],
                                             [7,7,7,7,7,0,0,0,0,0],
                                             [7,7,7,7,7,0,0,0,0,0],
                                             [-2,-2,-2,-2,-2,3,3,3,3,3],
                                             [-2,-2,-2,-2,-2,3,3,3,3,3],
                                             [-2,-2,-2,-2,-2,3,3,3,3,3],
                                             [-2,-2,-2,-2,-2,3,3,3,3,3],
                                             [-2,-2,-2,-2,-2,3,3,3,3,3]]
      toList complexQTree2 `shouldBe` concat [[9,9,9,9,9,-5,-5,-5,-5,-5],
                                              [9,9,9,9,9,-5,-5,-5,-5,-5],
                                              [9,9,9,9,9,-5,-5,-5,-5,-5],
                                              [9,9,9,9,9,-5,-5,-5,-5,-5],
                                              [9,9,9,9,9,-5,-5,-5,-5,-5],
                                              [-2,-2,10,10,10,1,1,1,1,1],
                                              [-2,-2,10,10,10,1,1,1,1,1],
                                              [-2,-2,10,10,10,1,1,1,1,1],
                                              [ 0, 0,-9,-9,-9,1,1,1,1,1],
                                              [ 0, 0,-9,-9,-9,1,1,1,1,1]]
    it "is instance of Traversable" $ do
      traverse (safeDiv 5) emptyQTree `shouldBe` Just emptyQTree
      traverse (safeDiv 5) simpleQTree1 `shouldBe` Just (Lens.set qtTree (Leaf 0) simpleQTree1)
      traverse (safeDiv 5) simpleQTree4 `shouldBe` Nothing
      traverse (safeDiv 5) complexQTree2 `shouldBe` Nothing
      sequence (fmap Just complexQTree2) `shouldBe` Just complexQTree2
      sequence (fmap Just complexQTree2) `shouldBe` Just complexQTree2
      sequence simpleQMaybeTree `shouldBe` Nothing
  describe "mkQuadTree" $
    it "simply creates QuadTree" $ do
      mkQuadTree 'A' 0 0 `shouldBe` QuadTree Empty 0 0
      mkQuadTree 'A' 5 5 `shouldBe` QuadTree (Leaf 'A') 5 5
      mkQuadTree True 5 5 `shouldBe` QuadTree (Leaf True) 5 5
      mkQuadTree "Hi" 5 5 `shouldBe` QuadTree (Leaf "Hi") 5 5
  describe "hasElementOn" $
    it "checks if coords are in quad tree (top, left = 0 0)" $ do
      hasElementOn emptyQTree 0 0 `shouldBe` False
      hasElementOn simpleQTree1 (-2) (-1) `shouldBe` False
      hasElementOn simpleQTree1 2 1 `shouldBe` True
      hasElementOn simpleQTree1 4 1 `shouldBe` False
      hasElementOn simpleQTree1 3 2 `shouldBe` False
      hasElementOn complexQTree2 10 10 `shouldBe` False
      hasElementOn complexQTree2  8 12 `shouldBe` False
      hasElementOn complexQTree2 12  8 `shouldBe` False
  describe "getElement" $ do
    it "retrieves Nothing if it is not there" $ do
      getElement emptyQTree 0 0 `shouldBe` Nothing
      getElement simpleQTree1 (-2) (-1) `shouldBe` Nothing
      getElement simpleQTree1 4 1 `shouldBe` Nothing
      getElement simpleQTree1 3 2 `shouldBe` Nothing
      getElement complexQTree2 8 12 `shouldBe` Nothing
    it "retrieves Just element if it is there" $ do
      getElement simpleQTree1 2 1 `shouldBe` Just 7
      getElement simpleQTree3 3 2 `shouldBe` Just 7
      getElement simpleQTree3 4 8 `shouldBe` Just (-2)
      getElement simpleQTree3 5 1 `shouldBe` Just 0
      getElement simpleQTree3 5 9 `shouldBe` Just 3
      getElement complexQTree1 2 7 `shouldBe` Just 15
      getElement complexQTree1 1 8 `shouldBe` Just 5
  describe "toMatrix" $
    it "converts quad tree to matrix (list of lists)" $ do
      toMatrix emptyQTree `shouldBe` [[]]
      toMatrix simpleQTree1 `shouldBe` [[7,7,7,7],
                                        [7,7,7,7]]
      toMatrix simpleQTree3 `shouldBe` [[7,7,7,7,7,0,0,0,0,0],
                                        [7,7,7,7,7,0,0,0,0,0],
                                        [7,7,7,7,7,0,0,0,0,0],
                                        [7,7,7,7,7,0,0,0,0,0],
                                        [7,7,7,7,7,0,0,0,0,0],
                                        [-2,-2,-2,-2,-2,3,3,3,3,3],
                                        [-2,-2,-2,-2,-2,3,3,3,3,3],
                                        [-2,-2,-2,-2,-2,3,3,3,3,3],
                                        [-2,-2,-2,-2,-2,3,3,3,3,3],
                                        [-2,-2,-2,-2,-2,3,3,3,3,3]]
      toMatrix complexQTree2 `shouldBe` complexMatrix2
  describe "fromMatrix" $ do
    it "converts matrix (list of lists) to quad tree" $ do
      fromMatrix [[]] `shouldBe` emptyQTree
      fromMatrix [[7,7,7,7],[7,7,7,7]] `shouldBe` simpleQTree1
      fromMatrix complexMatrix2 `shouldBe` complexQTree2
    it "works with toMatrix as identity (complex map)" $ do
      length (toMatrix . fromMatrix $ Maps.map01data) `shouldBe` 12
      all (\x -> length x == 40) (toMatrix . fromMatrix $ Maps.map01data) `shouldBe` True
