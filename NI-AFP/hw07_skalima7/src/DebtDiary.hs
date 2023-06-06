module DebtDiary (runCli) where

import Lib.Formatting qualified as Formatting
import Lib.Logics qualified as Logics
import Lib.Operations qualified as Operations
import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit

flip3 :: (a -> b -> c -> d) -> c -> b -> a -> d
flip3 f a b c = f c b a

arguments :: Mode [(String, String)]
arguments =
  mode
    "debt-diary"
    []
    "Debt Diary program"
    (flagArg (upd "action") "list|item|overview|balance|create|add|edit|delete")
    [ flagReq ["file", "f"] (upd "file") "path_to_file.json" "Debt diary file",
      flagHelpSimple (("help", "") :)
    ]
  where
    upd msg x v = Right $ (msg, x) : v

putStrRight :: String -> IO (Either a ())
putStrRight a = Right <$> putStrLn a

-- | The 'runCli' function parses cli arguments and runs appropriate actions.
runCli :: IO ()
runCli = do
  xs <- reverse <$> processArgs arguments
  case xs of
    [("file", file), ("action", "list")] -> do
      diaryOpt <- Operations.parseDiary file

      Operations.apply2 (putStrRight . Formatting.prettify) diaryOpt
    [("file", file), ("action", "overview")] -> do
      diaryOpt <- Operations.parseDiary file

      Operations.apply2 (putStrRight . Formatting.prettifyTable) diaryOpt
    [("file", file), ("action", "balance")] -> do
      diaryOpt <- Operations.parseDiary file

      Operations.apply2 (putStrRight . Formatting.prettifyBalance) diaryOpt
    [("file", file), ("action", "create")] -> do
      diary <- Operations.promptDiary

      Operations.apply (Operations.saveDiary file) diary
    [("file", file), ("action", "add")] -> do
      diaryOpt <- Operations.parseDiary file

      case diaryOpt of
        Right diary -> do
          newRecord <- Operations.promptDebtRecord
          Operations.apply (Operations.saveDiary file . Logics.appendRecord newRecord) diary
        Left err -> Operations.exitWithErrorMessage err
    [("file", file), ("action", "item"), (_, index)] -> do
      diaryOpt <- Operations.parseDiary file

      case diaryOpt of
        Right diary -> do
          let idx = Operations.loadIndex index (Logics.maxRecordIdx diary)
          Operations.apply2 (putStrRight . flip Formatting.prettifyNth diary) idx
        Left err -> Operations.exitWithErrorMessage err
    [("file", file), ("action", "edit"), (_, index)] -> do
      diaryOpt <- Operations.parseDiary file

      case diaryOpt of
        Right diary -> do
          let idxOpt = Operations.loadIndex index (Logics.maxRecordIdx diary)

          case idxOpt of
            Right idx -> do
              newRecord <- Operations.promptDebtRecord
              Operations.apply (Operations.saveDiary file . flip3 Logics.updateRecord diary newRecord) idx
            Left err -> Operations.exitWithErrorMessage err
        Left err -> Operations.exitWithErrorMessage err
    [("file", file), ("action", "delete"), (_, index)] -> do
      diaryOpt <- Operations.parseDiary file

      case diaryOpt of
        Right diary -> do
          let idx = Operations.loadIndex index (Logics.maxRecordIdx diary)

          Operations.apply2 (Operations.saveDiary file . flip Logics.deleteRecord diary) idx
        Left err -> Operations.exitWithErrorMessage err
    _ -> if ("help", "") `elem` xs then print $ helpText [] HelpFormatDefault arguments else print $ helpText ["Invalid arguments provided"] HelpFormatDefault arguments
