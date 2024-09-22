import qualified InferenceSpec
import qualified ParserSpec
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  ParserSpec.spec
  InferenceSpec.spec
