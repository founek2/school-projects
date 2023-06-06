{-# LANGUAGE ImportQualifiedPost #-}

module Lib.Operations
  ( parseDiary,
    apply,
    apply2,
    loadIndex,
    saveDiary,
    promptDebtRecord,
    promptDiary,
    exitWithErrorMessage,
  )
where

import Control.Exception (IOException, try)
import Data.Aeson
import Data.ByteString.Lazy qualified as B
import Data.Time (getCurrentTime)
import Lib.DataTypes
import Lib.DataTypes qualified as DataTypes
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hPrint, stderr)
import Text.Read (readMaybe)

justToRight :: a -> Maybe b -> Either a b
justToRight a val = case val of
  Just b -> Right b
  Nothing -> Left a

decodeDiary :: B.ByteString -> Maybe DataTypes.Diary
decodeDiary x = decode x :: Maybe DataTypes.Diary

-- | The 'parseDiary' function reads file from specified file path and decodes Diary from it's content (in JSON format).
-- Error handles invalid access to file or invalid format.
--
-- >>> parseDiary "debt-diary.json"
-- Diary "Fancy title" "note" []
parseDiary :: FilePath -> IO (Either Error DataTypes.Diary)
parseDiary jsonFile = do
  fileOrEXc <- (try $ B.readFile jsonFile) :: IO (Either IOException B.ByteString)
  case fileOrEXc of
    Left _ -> return (Left UnableReadFile)
    Right file -> return (justToRight UnableToDecodeJSON $ decodeDiary file)

-- | The 'saveDiary' function encodes diary into JSON and saves to filePath.
-- Error handles invalid write access.
--
-- >>> saveDiary "debt-diary.json" diary
saveDiary :: FilePath -> DataTypes.Diary -> IO (Either Error ())
saveDiary filePath diary = do
  fileOrEXc <- (try $ B.writeFile filePath (encode diary)) :: IO (Either IOException ())

  case fileOrEXc of
    Left _ -> return (Left UnableWriteFile)
    Right _ -> return (Right ())

-- | The 'exitWithErrorMessage' function prints message to stderr and exit with status 2
--
-- >>> exitWithErrorMessage UnableReadFile
exitWithErrorMessage :: Show s => s -> IO ()
exitWithErrorMessage str = hPrint stderr str >> exitWith (ExitFailure 2)

-- | The 'apply2' function takes function and parameter, extracts Right from parameter and pass it's value to function.
--  Checks result of function and returns Right value. Whenever Left is encountered, it calls 'exitWithErrorMessage'
apply2 :: Show a => (b -> IO (Either Error c)) -> Either a b -> IO ()
apply2 f pOpt = case pOpt of
  Right a -> do
    result <- f a
    case result of
      Right _ -> return ()
      Left err -> exitWithErrorMessage err
  Left err -> exitWithErrorMessage err

-- | The 'apply' function takes function and value, passes value to function and checks result
--  Whenever Left is encountered, it calls 'exitWithErrorMessage'
apply :: (a -> IO (Either Error b)) -> a -> IO ()
apply f a = do
  result <- f a
  case result of
    Right _ -> return ()
    Left err -> exitWithErrorMessage err

-- | The 'loadIndex' function checks wheter value can be used as index
--  Checks idx >= 0 && <= upperBound
loadIndex :: String -> Int -> Either Error Int
loadIndex idx upperBound = case readMaybe idx :: Maybe Int of
  Just i -> if i >= 0 && i <= upperBound then Right i else Left IndexOutOfRange
  Nothing -> Left IndexMustBeNumber

stringToFloat :: String -> Double
stringToFloat = read

-- | The 'promptDebtRecord' function promts user for data and returns new Debt
promptDebtRecord :: IO Debt
promptDebtRecord = do
  putStrLn "Person: "
  person <- getLine
  putStrLn "Note: "
  note <- getLine
  putStrLn "amount: "
  amount <- getLine

  Debt person (stringToFloat amount) (if note == "" then Nothing else Just note) <$> getCurrentTime

-- | The 'promptDiary' function promts user for data and returns new Diary
promptDiary :: IO Diary
promptDiary = do
  putStrLn "Title: "
  title <- getLine
  putStrLn "note: "
  note <- getLine

  return (Diary title note [])
