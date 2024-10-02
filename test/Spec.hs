import qualified MNML.InferenceSpec as InferenceSpec
import qualified MNML.ParserSpec as ParserSpec
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  ParserSpec.spec
  InferenceSpec.spec
