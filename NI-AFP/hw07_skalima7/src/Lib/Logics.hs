module Lib.Logics
  ( deleteRecord,
    appendRecord,
    updateRecord,
    maxRecordIdx,
  )
where

import Lib.DataTypes (Debt, Diary (Diary), records)

-- | The 'deleteRecord' function deletes specified record by index from diary
--
-- >>> deleteRecord 0 diary
deleteRecord :: Int -> Diary -> Diary
deleteRecord idx diary@(Diary {records = r}) = diary {records = take idx r ++ drop (idx + 1) r}

-- | The 'appendRecord' function append debt record to diary
--
-- >>> appendRecord record diary
appendRecord :: Debt -> Diary -> Diary
appendRecord record diary@(Diary {records = r}) = diary {records = r ++ [record]}

-- | The 'updateRecord' function updates record in diary at specified index.
-- If index does not exists, it will append record
--
-- >>> updateRecord 0 record diary
updateRecord :: Int -> Debt -> Diary -> Diary
updateRecord idx record diary@(Diary {records = r}) = diary {records = take idx r ++ [record] ++ drop (idx + 1) r}

-- | The 'maxRecordIdx' function calculates maximal record index.
--
-- >>> maxRecordIdx diary
-- 5
maxRecordIdx :: Diary -> Int
maxRecordIdx Diary {records = r} = length r - 1