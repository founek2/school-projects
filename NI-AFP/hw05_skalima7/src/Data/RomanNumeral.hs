module Data.RomanNumeral where

import Data.Maybe (fromMaybe)
-- Use Data.RomanNumeral.Helpers submodule
-- In case of need, feel free to change or enhance Helpers or create own
-- submodule
--
-- DO NOT HARDCODE ANY STRINGs/CHARs IN THIS MODULE!
import qualified Data.RomanNumeral.Helpers as Helpers
import Text.Regex.Posix

-- | RomanNumeral type (wrapper) for English numerals
newtype RomanNumeral = RomanNumeral String
  deriving (Show, Read)

-- \| Pack Integer into RomanNumeral (English numeral string)
pack :: (Integral a, Show a) => a -> RomanNumeral
pack integral = RomanNumeral $ fromMaybe err (integral2RomanNumeral integral)
  where
    err = error $ Helpers.messageBadIntegral integral

-- | Unpack RomanNumeral (English numeral string) to Integer
unpack :: RomanNumeral -> Integer
unpack (RomanNumeral numeral) = fromMaybe err (romanNumeral2Integral numeral)
  where
    err = error $ Helpers.messageBadNumeral numeral

-- | Translate Integral value to Roman Numeral String (if possible)
-- TODO: implement Integral->String translation
integral2RomanNumeral :: (Integral a, Show a) => a -> Maybe String
integral2RomanNumeral a | a < Helpers.minimal || a > Helpers.maximal = error $ Helpers.messageBadIntegral a
integral2RomanNumeral a = Just $ snd (foldr do' acc Helpers.intToChar)
  where
    prefix = if a < 0 then Helpers.negativePrefix else Helpers.zero
    acc = (abs a, prefix)
    do' :: Integral a => (a, String) -> (a, String) -> (a, String)
    do' (n, r) (a, y) = (a `mod` n, y ++ (concat $ replicate (fromIntegral diff) r)) where diff = a `div` n

romanRegex = "^-?M{0,4}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})$"

-- | Translate Roman Numeral String to Integral value (if possible)
-- Algorithm https://levelup.gitconnected.com/solution-roman-to-integer-python-30b01b182655
romanNumeral2Integral :: (Integral a, Show a) => String -> Maybe a
romanNumeral2Integral a | not (a =~ romanRegex :: Bool) = error $ Helpers.messageBadNumeral a
romanNumeral2Integral a = Just $ fst (foldr do' acc letters)
  where
    -- prefix = if fst a == "-" then -1 else 1
    letters = map (\x -> (0, [x])) a
    acc = (0, "")
    do' :: Integral a => (a, String) -> (a, String) -> (a, String)
    do' (_, c) (n, _)
      | c == "-" = (-n, "")
      | otherwise = (sum, "")
      where
        (Just v) = lookup c Helpers.charToInt
        sum = if 4 * v < n then n - v else n + v

-- TODO: implement RomanNumeral instances of Bounde, Num, Ord, Eq, Enum, Real, and Integral
instance Bounded RomanNumeral where
  minBound = RomanNumeral min where (Just min) = integral2RomanNumeral Helpers.minimal
  maxBound = RomanNumeral max where (Just max) = integral2RomanNumeral Helpers.maximal

instance Eq RomanNumeral where
  (==) (RomanNumeral a) (RomanNumeral b) = a == b

instance Ord RomanNumeral where
  compare (RomanNumeral a) (RomanNumeral b) = romanNumeral2Integral a `compare` romanNumeral2Integral b

instance Num RomanNumeral where
  (+) a b = pack $ unpack a + unpack b
  (*) a b = pack $ unpack a * unpack b
  negate (RomanNumeral "") = RomanNumeral ""
  negate (RomanNumeral a@(x : xs)) = if x == '-' then RomanNumeral xs else RomanNumeral $ "-" ++ a
  abs (RomanNumeral "") = RomanNumeral ""
  abs (RomanNumeral a@(x : xs)) = if x == '-' then RomanNumeral xs else RomanNumeral a
  signum a
    | v < 0 = -1
    | v == 0 = 0
    | v > 0 = 1
    where
      v = unpack a
  fromInteger = pack

instance Enum RomanNumeral where
  -- Int -> RomanNumeral
  toEnum a = pack a

  -- RomanNumeral -> Int
  fromEnum a = fromInteger $ unpack a

instance Real RomanNumeral where
  toRational a = toRational $ unpack a

instance Integral RomanNumeral where
  quotRem y z = (pack $ a `quot` b, pack $ a `rem` b)
    where
      a = unpack y
      b = unpack z
  toInteger = unpack
