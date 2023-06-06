import Test.Hspec

import qualified ShapesSpec
import qualified ResultListSpec
import qualified RomanNumeralSpec

main :: IO () 
main = hspec spec

spec :: Spec
spec = do
  describe "Shapes"       ShapesSpec.spec
  describe "ResultList"   ResultListSpec.spec
  describe "RomanNumeral" RomanNumeralSpec.spec
