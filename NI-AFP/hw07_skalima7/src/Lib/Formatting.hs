module Lib.Formatting
  ( prettify,
    prettifyNth,
    prettifyTable,
    prettifyBalance,
    newLines,
  )
where

import Data.Bifunctor qualified
import Data.List (find, groupBy, intercalate, sortBy)
import Data.Maybe
import Data.Ord (comparing)
import Data.Time
import Lib.DataTypes (Debt (Debt), Diary (Diary), amount, person, records)
import Text.Printf

-- | The 'newLines' function joins list of string with new line character.
--
-- >>> newLines ["a", "b", "c"]
-- "a\nb\nc"
newLines :: [String] -> String
newLines = intercalate "\n"

-- | The 'prettify' function converts debt diary into human readable text
--
-- >>> prettify diary
-- "Diary title
--  diary note
--  records:
--  Idx: 0
--    person: John Doe",
--    note:
--    amount: 14.14
--    ts: 2011-11-19"
prettify :: Diary -> String
prettify (Diary t n r) =
  newLines
    [t, n, "records:", newLines $ zipWith (curry prettifyRecord) [0 :: Integer ..] r]

-- | The 'prettifyNth' function converts nth debt record into human readable text
--
-- >>> prettifyNth idx diary
-- "Idx: 1
--   person: John Doe
--   note:
--   amount: 22.22
--   ts: 2022-11-1"
prettifyNth :: Int -> Diary -> String
prettifyNth idx (Diary _ _ r) = case itemOpt of
  Just item -> prettifyRecord item
  Nothing -> error "item was not found"
  where
    itemOpt = find (\x -> fst x == idx) (zip [0 ..] r)

-- | The 'prettifyRecord' function converts debt record into human readable text
--
-- >>> prettifyRecord (idx, record)
-- "Idx: 1
--   person: John Doe
--   note:
--   amount: 22.22
--   ts: 2022-11-1"
prettifyRecord :: Show a => (a, Debt) -> String
prettifyRecord (i, Debt p a n ts) =
  newLines
    [ "Idx: " <> show i,
      " person: " <> p,
      " note: " <> fromMaybe "" n,
      " amount: " <> showFloat a,
      " ts: " <> formatTime defaultTimeLocale "%Y-%m-%d" ts
    ]

-- | The 'prettifyTable' sum debt per user and converts into human readable table
--
-- >>> prettifyTable diary
-- "| Name     | Amount |
--  | John Doe | 36.36  |"
prettifyTable :: Diary -> String
prettifyTable Diary {records = r} =
  newLines $
    map
      (\(p, a) -> "| " <> spacePadding lenFstColumn p <> " | " <> spacePadding lenSndColumn a <> " |")
      lst
  where
    summed = map (Data.Bifunctor.second showFloat) $ sumPerUser r
    lst = ("Name", "Amount") : summed
    lenFstColumn = maximum $ map (length . fst) lst
    lenSndColumn = maximum $ map (length . snd) lst

-- | The 'prettifyBalance' sum all debt and create human readable string
--
-- >>> prettifyBalance diary
-- "Balance: 300.33"
prettifyBalance :: Diary -> String
prettifyBalance Diary {records = r} =
  "Balance: " <> showFloat totalAmount
  where
    summed = sumPerUser r
    totalAmount = sum $ map snd summed

-- | Add space padding after text
--
-- >>> spacePadding 4 "hi"
-- "hi  "
spacePadding :: Int -> String -> String
spacePadding l a = a ++ replicate (l - length a) ' '

sumPerUser :: [Debt] -> [(String, Double)]
sumPerUser r = summed
  where
    sorted = sortBy (comparing snd) $ map (\Debt {person = p, amount = a} -> (p, a)) r
    grouped = groupBy (\a b -> fst a == fst b) sorted
    summed = map (\x@((name, _) : _) -> (name, sum $ map snd x)) grouped

showFloat :: Double -> String
showFloat = printf "%.2f"
