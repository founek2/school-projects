module ResultListSpec (spec) where

import Control.Exception
import Data.Semigroup
import Test.Hspec
import Test.QuickCheck hiding (SortedList(..))

import Data.ResultList

empty :: ResultList Int
empty = Results []

list1 :: ResultList Int
list1 = Results [1..5]

list2 :: ResultList Int
list2 = Results [6,10]

list3 :: ResultList Int
list3 = Results [1,2,3,4,5,6,10]

list4 :: ResultList Int
list4 = Results [6,10,1,2,3,4,5]

list5 :: ResultList Int
list5 = Results [0,1,2,5,15]

list6 :: ResultList String
list6 = return "yolo"

errorList1 :: ResultList Int
errorList1 = Error 7 "Wild error appeared"

errorList2 :: ResultList Int
errorList2 = Error 666 "Higher priority error from hell"

errorList3 :: ResultList Int
errorList3 = Error 7 "Wilder error appeared"

divisionOp :: Int -> Int -> ResultList Int
divisionOp _ 0 = Error 10 "Division by zero"
divisionOp x y = return $ x `div` y

doChain1 :: ResultList Int
doChain1 = do
  x <- divisionOp 5 2 -- 2
  divisionOp 3 x -- 1

doChain2 :: ResultList Int
doChain2 = do
  Error 5 "Operation not permitted"
  divisionOp 3 0
  return (-123)

doChain3 :: ResultList Int
doChain3 = do
  divisionOp 5 2 -- 2
  divisionOp 3 0 -- error
  return 666

doChain4 :: ResultList String
doChain4 = do
  return "what"
  return "the"
  return "hell"


instance (Ord a, Arbitrary a) => Arbitrary (ResultList a) where
  arbitrary = fmap Results rndlst
            where rndlst = sized $ \n -> do
                              k <- choose (0, n)
                              sequence [ arbitrary | _ <- [1..k] ]


prop_AssocInt :: ResultList Int ->  ResultList Int -> ResultList Int -> Bool
prop_AssocInt x y z = x <> (y <> z) == (x <> y) <> z

prop_NeutralInt :: ResultList Int -> Bool
prop_NeutralInt x = x <> mempty == x && mempty <> x == x

prop_FmapIdInt :: ResultList Int -> Bool
prop_FmapIdInt x = fmap id x == id x

prop_FmapComposeInt :: ResultList Int -> Bool
prop_FmapComposeInt x = fmap ((2*) . (\z -> z-5)) x == fmap (2*) (fmap (\z -> z-5) x)

spec :: Spec
spec = describe "data type ResultList" $ do
        it "has working toList" $ do
          toList list1 `shouldBe` [1..5]
          evaluate (toList errorList1) `shouldThrow` errorCall "#7: Wild error appeared"

        it "is instance of Semigroup (<>)" $ do
          empty <> empty `shouldBe` empty
          list1 <> empty `shouldBe` list1
          empty <> list2 `shouldBe` list2
          list1 <> list2 `shouldBe` list3
          list2 <> list1 `shouldBe` list4
        it "is instance of Semigroup (<>) for errors" $ do
          errorList1 <> errorList2 `shouldBe` errorList2 -- higher priority wins
          errorList2 <> errorList1 `shouldBe` errorList2 -- higher priority wins
          errorList1 <> errorList3 `shouldBe` errorList1 -- same priority, first in order wins
          errorList3 <> errorList1 `shouldBe` errorList3 -- same priority, first in order wins
        it "is instance of Semigroup (and <> is associative)" $
          property prop_AssocInt
        it "is instance of Monoid (mempty, mappend, mconcat)" $ do
          mappend list1 mempty `shouldBe` list1
          mappend mempty list2 `shouldBe` list2
          mappend list1 list2 `shouldBe` list3
          mappend list2 list1 `shouldBe` list4
          mconcat [list2, mempty, list1, mempty] `shouldBe` list4
        it "is instance of Monoid (mempty is neutral)" $
          property prop_NeutralInt

        it "is instance of Functor (fmap, <$>)" $ do
          fmap (*2) list1 `shouldBe` Results (map (*2) [1..5])
          fmap (1-) list1 `shouldBe` Results [0, -1, -2, -3, -4]
          const 0 <$> list1 `shouldBe` Results (replicate 5 0)
        it "is instance of Functor (fmap has identity)" $
          property prop_FmapIdInt
        it "is instance of Functor (fmap is homomorphic)" $
          property prop_FmapComposeInt

        it "is instance of Applicative (pure, <*>)" $ do
          pure 5 `shouldBe` Results [5] -- cannot test function equality
          pure (*2) <*> list1 `shouldBe` Results (map (*2) [1..5])
          pure (1-) <*> list1 `shouldBe` Results [0, -1, -2, -3, -4]
          Results [(1-), (*3)] <*> list1 `shouldBe` Results [0, -1, -2, -3, -4, 3, 6, 9, 12, 15]

        it "is instance of Monad (>>)" $ do
          (empty >> empty) `shouldBe` empty
          (empty >> list5) `shouldBe` empty
          (list3 >> empty) `shouldBe` empty
          (list6 >> list6) `shouldBe` list6
          -- repeats like normal [] monad
          (list2 >> list1) `shouldBe` Results [1, 2, 3, 4, 5, 1, 2, 3, 4, 5]
          (errorList1 >> list1) `shouldBe` errorList1
          (list1 >> errorList1 >> empty) `shouldBe` errorList1
        it "is instance of Monad (>>=)" $ do
          (empty >>= (\x -> return (x*x))) `shouldBe` empty
          (list1 >>= (\x -> return (x*x))) `shouldBe` Results (map (^2) [1..5])
          (list1 >>= const errorList1) `shouldBe` errorList1
        it "chains with 'do'" $ do
          doChain1 `shouldBe` Results [1]
          doChain2 `shouldBe` Error 5 "Operation not permitted"
          doChain3 `shouldBe` Error 10 "Division by zero"
          doChain4 `shouldBe` Results ["hell"]
