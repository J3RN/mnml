module InferenceSpec
    ( spec
    ) where

import qualified Data.Map   as Map
import           Data.Text  (Text)
import           Inference  (infer)
import           Parser     (Type (..))
import           Test.Hspec

infer' :: Text -> Text -> Either Text Type
infer' t = infer (Map.fromList [("test", t)]) "test"

spec :: Spec
spec =
  describe "Inference" $ do
    describe "literals" $ do
      it "infers TInt" $ do
        (infer' "main = 12" "main") `shouldBe` Right TInt

      it "infers TFloat" $ do
        (infer' "main = 1.2" "main") `shouldBe` Right TFloat

      it "infers TChar" $ do
        (infer' "main = 'c'" "main") `shouldBe` Right TChar

      it "infers TString" $ do
        (infer' "main = \"hello\"" "main") `shouldBe` Right TString

    describe "records" $ do
      it "infers records" $ do
        (infer' "main = {name: \"Jon\"}" "main") `shouldBe` Right (TRecord [("name", TString)])
