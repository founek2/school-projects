module Lib where

import Data.List
import Data.DummyList.Examples (example1)
import qualified Data.MyString.Examples as MyString
-------------------------------------------------------------------------------
-- DO NOT EDIT DATA TYPES!
data MaritalStatus = Single | Married | Widowed
                   deriving (Show, Read, Eq)

data Gender = Male | Female
            deriving (Show, Read, Eq)

data AcademicTitle = DiS | Bc | Mgr | Ing | PhDr | MUDr | PhD | Doc | Prof
                   deriving (Show, Read, Ord, Eq, Bounded, Enum)

data Person = Person { pFirstname     :: String
                     , pLastname      :: String
                     , pGender        :: Gender
                     , pMaritalStatus :: MaritalStatus
                     , pAge           :: Int
                     , pATitles       :: [AcademicTitle]
                     }

-- | Full czech salutation (in nominative - i.e. první pád)
-- |
-- | "pan doktor Pavel Novák", "paní inženýrka Karolína Šťastná"
-- | "slečna Petra Králová", "Jan" (kid)
-- | if younger than 15 -> kid -> just firstname
-- | if female younger than 25 without academic title and single -> "slečna"
-- | otherwise "pan/paní" depending on the gender
-- | if academic titles, pick the most important (nothing for DiS and Bc)
-- |
-- | https://www.muni.cz/o-univerzite/uredni-deska/oslovovani-akademickych-pracovniku
-- | http://www.etiketavse.estranky.cz/clanky/etiketa/4.-oslovovani-a-spolecenska-vyznamnost.html
-- | http://www.studenta.cz/vysokoskolske-tituly-jak-oslovovat-na-akademicke-pude/magazin/article/587
-- TODO: implement czech salutation which passes the tests
czechSalutation :: Person -> String
czechSalutation Person{pAge=age,pFirstname=firstname} | age < 15 = firstname
czechSalutation p@Person{ pFirstname=firstname
                        , pLastname=lastname
                        , pATitles=[]
                        } = salut ++ " " ++ firstname ++ " " ++ lastname where salut = salutation p
czechSalutation p@Person{ pFirstname=firstname
                        , pLastname=lastname
                        , pATitles=titles
                        , pGender=gender
                        } = salut ++ " " ++ (if title /= "" then title ++ " " else "") ++ firstname ++ " " ++ lastname 
                        where 
                            title = (czechTitle gender). highestTitle $ titles
                            salut = salutation p


salutation :: Person -> String
salutation Person{pAge=age
                , pGender=Female
                , pMaritalStatus=status
                , pATitles=titles
                } = if age < 25 && status == Single && titles == [] then "slečna" else "paní"
salutation Person{pAge=age} = "pan"

highestTitle :: [AcademicTitle] -> AcademicTitle
highestTitle = last . sort

isDoctor :: AcademicTitle -> Bool
isDoctor title = if title == MUDr || title == PhDr || title == PhD then True else False

czechTitle :: Gender -> AcademicTitle -> String
czechTitle Female Mgr = "magistra"
czechTitle Male Mgr = "magistr"
czechTitle Female Ing  = "inženýrka"
czechTitle Male Ing = "inženýr"
czechTitle Female title | isDoctor title  = "doktorka"
czechTitle Male title | isDoctor title = "doktor"
czechTitle Female Doc  = "docentka"
czechTitle Male Doc = "docent"
czechTitle Female Prof  = "profesorka"
czechTitle Male Prof = "profesor"
czechTitle _ _ = ""

-------------------------------------------------------------------------------
-- DO NOT EDIT DATA TYPE!
-- https://en.wikipedia.org/wiki/Interval_(mathematics)
data IntervalBoundary = PositiveInfinity
                      | NegativeInfinity
                      | Inclusive Double
                      | Exclusive Double
                      deriving (Show, Read, Eq)

data Interval = Empty
              | Interval IntervalBoundary IntervalBoundary
              | Union [Interval]
              | Intersection [Interval]
              | AllNumbers
              deriving (Show, Read, Eq)

-- | Check if number is in given interval
intervalContains :: Interval -> Double -> Bool
intervalContains Empty _ = False
intervalContains AllNumbers _ = True
intervalContains (Interval NegativeInfinity PositiveInfinity) _ = True

intervalContains (Interval NegativeInfinity (Inclusive ub)) a = a <= ub
intervalContains (Interval NegativeInfinity (Exclusive ub)) a = a <= ub

intervalContains (Interval (Inclusive lb) PositiveInfinity) a = lb <= a
intervalContains (Interval (Exclusive lb) PositiveInfinity) a = lb < a

intervalContains (Interval (Inclusive lb) (Inclusive ub)) a = lb <= a && a <= ub
intervalContains (Interval (Exclusive lb) (Exclusive ub)) a = lb < a && a < ub

intervalContains (Interval (Inclusive lb) (Exclusive ub)) a = lb <= a && a < ub
intervalContains (Interval (Exclusive lb) (Inclusive ub)) a = lb < a && a <= ub

intervalContains (Union intervals) a = any (\x -> intervalContains x a) intervals 
intervalContains (Intersection intervals) a = all (\x -> intervalContains x a) intervals 

intervalContains _ _ = False

-------------------------------------------------------------------------------
-- DO NOT EDIT DATA TYPE!
data Shape2D = Circle { ciRadius :: Double }
             | Square { sqSide :: Double }
             | Rectangle { reWidth :: Double, reHeight :: Double }
             | Triangle { triSideA :: Double, triSideB :: Double, triSideC :: Double }
             deriving (Show, Read, Eq)

-- TODO: implement circumference calculation for 2D shapes
shapeCircumference :: Shape2D -> Double
shapeCircumference (Circle radius) = 2 * pi * radius
shapeCircumference (Square a) = 4 * a
shapeCircumference (Rectangle a b) = 2*(a + b)
shapeCircumference (Triangle a b c) = a + b + c 

-- TODO: implement area calculation for 2D shapes
shapeArea :: Shape2D -> Double
shapeArea (Circle radius) = pi*radius^2
shapeArea (Square a) = a^2
shapeArea (Rectangle a b) = a * b
-- with use of the Heron's formula
shapeArea (Triangle a b c) = sqrt(s*(s-a)*(s-b)*(s-c)) where s = (a+b+c)/2

-------------------------------------------------------------------------------
-- | Geometric sequence as infinite list
-- | https://en.wikipedia.org/wiki/Geometric_progression
-- TODO: implement geometric series
geometricSequence :: Num b => b -> b -> [b]
geometricSequence a r = [a * r ^i| i <- [0..]]


-- TODO: implement infinite list of fibonacciNumbers [0, 1, 1, 2, 3, 5, ...]
fibonacciNumbers :: [Integer]
-- I have tried to do this with list comprehensions but since they can't run paralel loops, it is not possible
--              although there is ParallelListComprehension extension which would allow it
-- So I've asked my friend Google for help - with some minor modifications
fibonacciNumbers = [0, 1] ++ fib'
fib' = zipWith (+) (1:fib') (0:1:fib')
-- TODO: multiply matrices x and y
-- TODO: use list comprehension!!!
-- https://en.wikipedia.org/wiki/Matrix_multiplication
-- Note: sublists are rows
-- if wrong sizes, raise error "Incorrect matrix sizes" (use "error" function)
matrixMultiplication :: Num a => [[a]] -> [[a]] -> [[a]]
matrixMultiplication x y = if length (x !! 0) == length y 
                            then matrixMultiplication' x y 
                            else error "Incorrect matrix sizes"

matrixMultiplication' :: Num a => [[a]] -> [[a]] -> [[a]]
matrixMultiplication' x y = map (\row -> map (multiplySumRows row) ty ) x where ty = transpose y

multiplySumRows :: Num a => [a] -> [a] -> a
multiplySumRows a b = sum $ map (\(a,b) -> a*b ) (zip a b)
-------------------------------------------------------------------------------
-- !!! DO NOT COPY, JUST IMPORT (avoid conflicts, pick the best option for you)
-- iii visit the content of modules
-- TODO: replace undefined with "example1" from Data.DummyList.Examples module
dummyListExample1 = example1
-- TODO: replace undefined with "example2" from Data.MyString.Examples module
stringExample2 = MyString.example2
-- TODO: replace undefined with "example3" from Data.MyString.Examples module
stringExample3 = MyString.example3
