import Test.Hspec

import qualified StackSpec
import qualified StackMachineSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Data.Stack"   StackSpec.spec
  describe "StackMachine" StackMachineSpec.spec
