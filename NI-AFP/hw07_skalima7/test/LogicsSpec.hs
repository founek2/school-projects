module LogicsSpec (spec) where

import Data.Time (UTCTime)
import Lib.DataTypes
import Lib.Logics
import Test.Hspec

record :: Debt
record = Debt "Person" 14.01 Nothing (read "2011-11-19 18:28:52.607875 UTC" :: UTCTime)

record2 :: Debt
record2 = Debt "Person2" 22.01 Nothing (read "2022-11-19 18:28:52.607875 UTC" :: UTCTime)

emptyDiary :: Diary
emptyDiary = Diary "Fancy title" "note" []

oneRecordDiary :: Diary
oneRecordDiary = Diary "Fancy title" "note" [record]

twoRecordDiary :: Diary
twoRecordDiary = Diary "Fancy title" "note" [record, record2]

spec :: Spec
spec = do
  describe "deleteRecord" $ do
    it "delete record from empty" $ do
      deleteRecord 0 emptyDiary `shouldBe` emptyDiary
    it "delete only record" $ do
      deleteRecord 0 oneRecordDiary `shouldBe` emptyDiary
    it "delete from invalid index" $ do
      deleteRecord 10 oneRecordDiary `shouldBe` oneRecordDiary

  describe "appendRecord" $ do
    it "append to empty" $ do
      appendRecord record emptyDiary `shouldBe` oneRecordDiary

    it "append to non-empty" $ do
      appendRecord record2 oneRecordDiary `shouldBe` twoRecordDiary

  describe "updateRecord" $ do
    it "update existing record" $ do
      let updatedDiary = oneRecordDiary {records = [record2]}
      updateRecord 0 record2 oneRecordDiary `shouldBe` updatedDiary

    it "update non-existing by inserting" $ do
      updateRecord 0 record emptyDiary `shouldBe` oneRecordDiary

  it "get max record index" $ do
    maxRecordIdx emptyDiary `shouldBe` (-1)
    maxRecordIdx oneRecordDiary `shouldBe` 0
    maxRecordIdx twoRecordDiary `shouldBe` 1