module FormattingSpec (spec) where

import Control.Exception (evaluate)
import Data.Time (UTCTime)
import Lib.DataTypes
import Lib.Formatting
import Test.Hspec

record :: Debt
record = Debt "John Doe" 14.14 Nothing (read "2011-11-19 18:28:52.607875 UTC" :: UTCTime)

record2 :: Debt
record2 = Debt "John Doe" 22.222222 Nothing (read "2022-11-19 18:28:52.607875 UTC" :: UTCTime)

emptyDiary :: Diary
emptyDiary = Diary "Empty diary" "note" []

twoRecordDiary :: Diary
twoRecordDiary = Diary "two rec" "note" [record, record2]

spec :: Spec
spec = do
  describe "Helper functions" $ do
    it "newLines" $ do
      newLines [] `shouldBe` ""
      newLines ["a"] `shouldBe` "a"
      newLines ["a", "b", "c"] `shouldBe` "a\nb\nc"
  describe "Pretiffy" $ do
    it "empty diary" $ do
      prettify emptyDiary `shouldBe` unlines [title emptyDiary, "note", "records:"]
    it "diary" $ do
      prettify twoRecordDiary
        `shouldBe` newLines
          [ "two rec",
            "note",
            "records:",
            "Idx: 0",
            " person: John Doe",
            " note: ",
            " amount: 14.14",
            " ts: 2011-11-19",
            "Idx: 1",
            " person: John Doe",
            " note: ",
            " amount: 22.22",
            " ts: 2022-11-19"
          ]

  describe "Pretiffy summed table" $ do
    it "empty diary" $ do
      prettifyTable emptyDiary
        `shouldBe` "| Name | Amount |"
    it "two record diary" $ do
      prettifyTable twoRecordDiary
        `shouldBe` newLines
          [ "| Name     | Amount |",
            "| John Doe | 36.36  |"
          ]

  describe "Pretiffy balance" $ do
    it "empty and two record" $ do
      prettifyBalance emptyDiary `shouldBe` "Balance: 0.00"
      prettifyBalance twoRecordDiary `shouldBe` "Balance: 36.36"

  describe "Pretiffy nth record" $ do
    it "empty diary" $ do
      evaluate (prettifyNth 0 emptyDiary) `shouldThrow` anyErrorCall
    it "invalid index" $ do
      evaluate (prettifyNth 10 twoRecordDiary) `shouldThrow` anyErrorCall
    it "two record diary" $ do
      prettifyNth 1 twoRecordDiary
        `shouldBe` newLines
          [ "Idx: 1",
            " person: John Doe",
            " note: ",
            " amount: 22.22",
            " ts: 2022-11-19"
          ]