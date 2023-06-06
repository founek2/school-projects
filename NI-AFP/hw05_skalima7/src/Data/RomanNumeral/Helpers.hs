module Data.RomanNumeral.Helpers where

import Data.Tuple (swap)

-- these are to help with RomanNumerals, feel free to edit it if needed
-- you don't have to use them at all... but you can

-- see: http://www.csgnetwork.com/csgromancnv.html

maximal :: Integral a => a
maximal = 4999

minimal :: Integral a => a
minimal = -4999

zero = "" -- zero is very special value

negativePrefix = "-"

messageBadIntegral integral = "Cannot convert to Roman Numeral: '" ++ show integral ++ "'"

messageBadNumeral numeral = "Illegal Roman Numeral: '" ++ numeral ++ "'"

intToChar :: Integral a => [(a, String)]
intToChar =
  reverse
    [ (1000, "M"),
      (900, "CM"),
      (500, "D"),
      (400, "CD"),
      (100, "C"),
      (90, "XC"),
      (50, "L"),
      (40, "XL"),
      (10, "X"),
      (9, "IX"),
      (5, "V"),
      (4, "IV"),
      (1, "I")
    ]

charToInt :: Integral a => [(String, a)]
charToInt = map swap intToChar

fromJust :: Maybe a -> a
fromJust Nothing = error "Maybe.fromJust: Nothing"