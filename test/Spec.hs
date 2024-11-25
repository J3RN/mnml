import qualified MNML.ParseSpec as ParserSpec
import qualified MNML.UnifySpec as UnificationSpec
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  ParserSpec.spec

-- UnificationSpec.spec
