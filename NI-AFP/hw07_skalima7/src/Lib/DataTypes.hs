{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Lib.DataTypes where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | Container for Debt record data
data Debt = Debt
  { person :: String,
    amount :: Double,
    note :: Maybe String,
    timestamp :: UTCTime
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

-- | Container for Debt diary data
data Diary = Diary
  { title :: String,
    note :: String,
    records :: [Debt]
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

data Error
  = IndexOutOfRange
  | IndexMustBeNumber
  | InputMustBeNumber
  | UnableReadFile
  | UnableWriteFile
  | UnableToDecodeJSON
  deriving (Show, Read, Eq)