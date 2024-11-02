module MNML.UnifySpec
    ( spec
    ) where

import           Control.Monad.State (runState)
import qualified Data.Map            as Map
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           MNML                (CompilerState (..), emptyState)
import qualified MNML.Type           as T
import           MNML.Unify
import           Test.Hspec

unify' :: Text -> (Either UnificationError T.Type, CompilerState)
unify' source = runState (valueType "test" "main") compilerState
  where
    compilerState = emptyState {_modules = Map.fromList [("test", source)]}

spec :: Spec
spec =
  describe "Unification" $ do
    describe "literals" $ do
      it "unifies 'nUmeRiCs'" $ do
        let (res, _) = unify' "main = 42"
        res `shouldBe` Right (T.Var "num" [T.Numeric] 0)

      it "unifies floats" $ do
        let (res, _) = unify' "main = 3.14"
        res `shouldBe` Right T.Float

      it "unifies chars" $ do
        let (res, _) = unify' "main = 'c'"
        res `shouldBe` Right T.Char

      it "unifies strings" $ do
        let (res, _) = unify' "main = \"hello world\""
        res `shouldBe` Right T.String

    describe "lambdas" $ do
      it "unifies nullary lambdas" $ do
        let (res, _cs) = unify' "main = () => { 1 }"
        res `shouldBe` Right (T.Fun [] (T.Var "num" [T.Numeric] 0))

      it "unifies identity" $ do
        let (res, _cs) = unify' "main = (x) => { x }"
        res `shouldBe` Right (T.Fun [T.Var "x" [] 0] (T.Var "x" [] 0))

      it "infers parameter types" $ do
        let (res, _cs) = unify' "main = (x) => { x + 42 }"
        res `shouldBe` Right (T.Fun [T.Var "num" [T.Numeric] 1] (T.Var "num" [T.Numeric] 1))

      it "infers (float) parameter type" $ do
        let (res, _cs) = unify' "main = (x) => { x + 3.14 }"
        res `shouldBe` Right (T.Fun [T.Float] T.Float)

    describe "lists" $ do
      it "unifies empty list" $ do
        let (res, _cs) = unify' "main = []"
        res `shouldBe` Right (T.List (T.Var "elem" [] 0))

      it "unifies singleton list" $ do
        let (res, _cs) = unify' "main = [1]"
        res `shouldBe` Right (T.List (T.Var "num" [T.Numeric] 0))

      it "unifies multiple list" $ do
        let (res, _cs) = unify' "main = [1.0, 2.0]"
        res `shouldBe` Right (T.List T.Float)

      it "does not unify inconsistent list" $ do
        let (res, _cs) = unify' "main = [1, \"Hello\"]"
        res `shouldBe` Left (UnificationError (T.Var "num" [T.Numeric] 0) T.String 1)

    describe "records" $ do
      it "unifies singleton record" $ do
        let (res, _cs) = unify' "main = {foo: \"Bar\"}"
        res `shouldBe` Right (T.Record [("foo", T.String)])

      it "unifies multiple field records" $ do
        let (res, _cs) = unify' "main = {foo: \"Bar\", bar: 1.0, baz: 'c'}"
        res `shouldBe` Right (T.Record [("foo", T.String), ("bar", T.Float), ("baz", T.Char)])

    describe "constructors" $ do
      it "unifies constructor application" $ do
        let (res, _cs) = unify' (Text.unlines ["MaybeInt = Just(Int) | None", "main = Just(1)"])
        res `shouldBe` Right (T.AlgebraicType "MaybeInt")

      it "unifies raw constructor as function" $ do
        let (res, _cs) = unify' (Text.unlines ["MaybeInt = Just(Int) | None", "main = Just"])
        res `shouldBe` Right (T.Fun [T.Int] (T.AlgebraicType "MaybeInt"))

      it "unifies raw, nullary constructor" $ do
        let (res, _cs) = unify' (Text.unlines ["MaybeInt = Just(Int) | None", "main = None"])
        res `shouldBe` Right (T.AlgebraicType "MaybeInt")

    describe "case" $ do
      it "unifies identity case" $ do
        let (res, _cs) = unify' (Text.unlines ["main = case 4 of", "a -> a"])
        res `shouldBe` Right (T.Var "num" [T.Numeric] 0)

      it "unifies two branch case" $ do
        let (res, _cs) = unify' (Text.unlines ["main = case foo of", "Just(n) -> n", "None -> 5"])
        res `shouldBe` Right (T.Var "num" [T.Numeric] 4)

      it "unifies disjoint record patterns" $ do
        let (res, _cs) = unify' (Text.unlines ["main = (x) => {", "case x of", "{foo: \"bar\"} -> 1", "{bar: 1.0} -> 2", "}"])
        res `shouldBe` Right (T.Fun [T.Record [("foo", T.String), ("bar", T.Float)]] (T.Var "num" [T.Numeric] 3))

      it "does not unify inconsistent record patterns" $ do
        let (res, _cs) = unify' (Text.unlines ["main = case foo of", "{foo: \"bar\"} -> 1", "{foo: 1.0} -> 2"])
        res `shouldBe` Left (UnificationError T.Float T.String 8)

      it "does not unify non-matching patterns" $ do
        let (res, _cs) = unify' (Text.unlines ["main = case foo of", "3 -> \"Fizz\"", "'5' -> \"Buzz\""])
        res `shouldBe` Left (UnificationError (T.Var "num" [T.Numeric] 1) T.Char 1)

      it "does not unify non-matching return types" $ do
        let (res, _cs) = unify' (Text.unlines ["main = case foo of", "3 -> \"Fizz\"", "5 -> 'B'"])
        res `shouldBe` Left (UnificationError T.String T.Char 1)
