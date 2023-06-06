import qualified FormattingSpec
import qualified LogicsSpec
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "LogicsSpec" LogicsSpec.spec
  describe "FormattingSpec" FormattingSpec.spec
