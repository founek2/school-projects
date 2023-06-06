module Data.Geography.Styles where

import Data.Geography
import Data.Tuple

-- | Default char if unknown 'MapField' is encountered
defaultChar = '?'

-- | Correspoinding characters
simpleChars :: [(MapField, Char)]
simpleChars = [ (Grass, 'G')
              , (Road, 'R')
              , (Desert, 'D')
              , (Water, 'W')
              , (Obstacle, 'O')
              , (Tree, 'T')
              ]

-- | Basic ASCII symbols
ascii :: [(MapField, Char)]
ascii = [ (Grass, ' ')
        , (Road, '+')
        , (Desert, '*')
        , (Water, '~')
        , (Obstacle, '#')
        , (Tree, 'x')
        ]

-- | Extended ASCII symbols
boxed :: [(MapField, Char)]
boxed = [ (Grass, ' ')
        , (Road, '▓')
        , (Desert, '░')
        , (Water, '~')
        , (Obstacle, '█')
        , (Tree, '^')
        ]

-- | Create transformation from 'MapField' to 'Char'
--
-- TODO: implement
mkCharStyle :: [(MapField, Char)] -> MapField -> Char
mkCharStyle m x = case lookup x m of 
        Just c -> c
        Nothing -> error "Mapping not found"

simpleStyle = mkCharStyle simpleChars
asciiStyle = mkCharStyle ascii
boxedStyle = mkCharStyle boxed

-- | Create transformation from 'Char' to 'MapField'
--
-- May cause some error if char not defined
-- TODO: implement
mkLoadStyle :: [(MapField, Char)] -> Char -> MapField
mkLoadStyle fieldToChar x = case lookup x charToField of 
        Just c -> c
        Nothing -> error "Mapping not found"
        where 
              charToField = map swap fieldToChar

loadSimpleStyle = mkLoadStyle simpleChars
loadAsciiStyle = mkLoadStyle ascii
loadBoxedStyle = mkLoadStyle boxed
