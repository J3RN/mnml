import qualified MNML.ParserSpec as ParserSpec
import qualified MNML.UnificationSpec as UnificationSpec
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  ParserSpec.spec
  UnificationSpec.spec
