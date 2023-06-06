import Test.Hspec

import qualified Data.QuadTreeSpec
import qualified Data.GeographySpec
import qualified MapTravelerSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Data.QuadTree"   Data.QuadTreeSpec.spec
  describe "Data.Geography"  Data.GeographySpec.spec
  describe "MapTraveler"     MapTravelerSpec.spec
