module MNML.InferenceSpec
    ( spec
    ) where

import           Control.Monad.State (evalState)
import qualified Data.Map            as Map
import           Data.Text           (Text, unlines)
import           MNML                (CompilerState (..))
import           MNML.Inference      (infer)
import           MNML.Parser         (Type (..))
import           Test.Hspec

infer' :: Text -> Text -> Either Text Type
infer' modText var = evalState (infer "test" var) defaultState
  where defaultState = (CompilerState { _stateSpans = (Map.fromList []), _stateModules =  (Map.fromList [("test", modText)])})

spec :: Spec
spec =
  describe "Inference" $ do
    describe "literals" $ do
      it "infers TInt" $ do
        infer' "main = 12" "main" `shouldBe` Right TInt

      it "infers TFloat" $ do
        infer' "main = 1.2" "main" `shouldBe` Right TFloat

      it "infers TChar" $ do
        infer' "main = 'c'" "main" `shouldBe` Right TChar

      it "infers TString" $ do
        infer' "main = \"hello\"" "main" `shouldBe` Right TString

    describe "records" $ do
      it "infers records" $ do
        infer' "main = {name: \"Jon\"}" "main" `shouldBe` Right (TRecord [("name", TString)])

    describe "constructors" $ do
      it "infers a bare constructor" $ do
        infer'
          ( Data.Text.unlines
              [ "MaybeInt = Just(Int) | Nothing",
                "main = Nothing"
              ]
          )
          "main"
          `shouldBe` Right (TNamedType "MaybeInt")

      it "infers a parameterized constructor without application" $ do
        infer'
          ( Data.Text.unlines
              [ "MaybeInt = Just(Int) | Nothing",
                "main = Just"
              ]
          )
          "main"
          `shouldBe` Right (TFun [TInt] (TNamedType "MaybeInt"))

      it "infers an applied parameterized constructor" $ do
        infer'
          ( Data.Text.unlines
              [ "MaybeInt = Just(Int) | Nothing",
                "main = Just(5)"
              ]
          )
          "main"
          `shouldBe` Right (TNamedType "MaybeInt")

      it "doesn't comprehend parens on bare constructor" $ do
        infer'
          ( Data.Text.unlines
              [ "MaybeInt = Just(Int) | Nothing",
                "main = Nothing()"
              ]
          )
          "main"
          `shouldBe` Left "TNamedType \"MaybeInt\" is not a function"

    describe "functions" $ do
      it "infers nullary lambdas as functions" $ do
        infer' "main = () => { 1 }" "main" `shouldBe` Right (TFun [] TInt)

      it "infers parameterized lambdas as functions" $ do
        infer' "main = (x) => { x + 1 }" "main" `shouldBe` Right (TFun [TInt] TInt)
