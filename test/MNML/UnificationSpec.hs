module MNML.UnificationSpec
    ( spec
    ) where

import           Control.Monad.State (runState)
import qualified Data.Map            as Map
import           Data.Text
import           MNML                (CompilerState (..))
import qualified MNML.Type           as T
import           MNML.Unification
import           Test.Hspec

unify' :: Text -> (Either UnificationError T.Type, CompilerState)
unify' source = runState (valueType "test" "main") compilerState
  where
    compilerState = CompilerState { _stateSpans = Map.empty
                                  , _stateModules = Map.fromList [("test", source)]
                                  , _stateTypes = Map.empty}

spec :: Spec
spec =
  describe "Unification" $ do
    describe "literals" $ do
      it "unifies ints" $ do
        let (res, cs) = unify' "main = 42"
        res `shouldBe` Right T.Int

      it "unifies floats" $ do
        let (res, cs) = unify' "main = 3.14"
        res `shouldBe` Right T.Float

      it "unifies chars" $ do
        let (res, cs) = unify' "main = 'c'"
        res `shouldBe` Right T.Char

      it "unifies strings" $ do
        let (res, cs) = unify' "main = \"hello world\""
        res `shouldBe` Right T.String

    describe "lambdas" $ do
      it "unifies nullary lambdas" $ do
        let (res, cs) = unify' "main = () => { 1 }"
        res `shouldBe` Right (T.Fun [] T.Int)

      it "unifies identity" $ do
        let (res, cs) = unify' "main = (x) => { x }"
        res `shouldBe` Right (T.Fun [T.Generic] T.Generic)
