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

unify' :: Text -> (Either [UnificationError] T.Type, CompilerState)
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

      it "infers parameters and result may be the same" $ do
        let (res, _cs) = unify' "main = (x, y) => { x + y }"
        res `shouldBe` Right (T.Fun [T.Var "ret" [T.Numeric] 2, T.Var "ret" [T.Numeric] 2] (T.Var "ret" [T.Numeric] 2))

      it "unifies value reference" $ do
        let (res, _cs) = unify' (Text.unlines ["main = () => { foo }", "foo = 5"] )
        res `shouldBe` Right (T.Fun [] (T.Var "num" [T.Numeric] 2))

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
        res `shouldBe` Left [UnificationError (T.Var "num" [T.Numeric] 0) T.String 1]

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

      it "unifies recursive types" $ do
        let (res, _cs) = unify' (Text.unlines ["IntList = Empty | Cons(Int, IntList)", "main = Cons(5, Cons(6, Empty))"])
        res `shouldBe` Right (T.AlgebraicType "IntList")

    describe "case" $ do
      it "unifies identity case" $ do
        let (res, _cs) = unify' (Text.unlines ["main = case 4 of", "a -> a"])
        res `shouldBe` Right (T.Var "num" [T.Numeric] 0)

      it "unifies two branch case" $ do
        let (res, _cs) = unify' (Text.unlines ["MaybeInt = Just(Int) | None", "foo = None", "main = case foo of", "Just(n) -> n", "None -> 5"])
        res `shouldBe` Right T.Int

      it "unifies branches with different literals for the same record field" $ do
        let (res, _cs) = unify' (Text.unlines ["main = (foo) => {", "case foo of", "{foo: \"bar\"} -> 1", "{foo: \"baz\"} -> 2", "}"])
        res `shouldBe` Right (T.Fun [T.PartialRecord [("foo", T.String)] 2] (T.Var "num" [T.Numeric] 3))

      it "unifies disjoint record patterns" $ do
        let (res, _cs) = unify' (Text.unlines ["main = (x) => {", "case x of", "{foo: \"bar\"} -> 1", "{bar: 1.0} -> 2", "}"])
        res `shouldBe` Right (T.Fun [T.PartialRecord [("foo", T.String), ("bar", T.Float)] 2] (T.Var "num" [T.Numeric] 3))

      it "does not unify inconsistent record patterns" $ do
        let (res, _cs) = unify' (Text.unlines ["main = (foo) => {", "case foo of", "{foo: \"bar\"} -> 1", "{foo: 1.0} -> 2", "}"])
        res `shouldBe` Left [UnificationError T.String T.Float 9]

      it "does not unify non-matching patterns" $ do
        let (res, _cs) = unify' (Text.unlines ["main = (foo) => {", "case foo of", "3 -> \"Fizz\"", "'5' -> \"Buzz\"", "}"])
        res `shouldBe` Left [UnificationError (T.Var "num" [T.Numeric] 1) T.Char 2]

      it "does not unify non-matching return types" $ do
        let (res, _cs) = unify' (Text.unlines ["main = (foo) => {", "case foo of", "3 -> \"Fizz\"", "5 -> 'B'", "}"])
        res `shouldBe` Left [UnificationError T.String T.Char 2]

    describe "binary numeric operators" $ do
      it "unifies binary of two floats as floats" $ do
        let (res, _cs) = unify' "main = 5.5 * 6.0"
        res `shouldBe` Right T.Float

      it "unifies binary of float and 'numeric' as float" $ do
        let (res, _cs) = unify' "main = 5.5 * 6"
        res `shouldBe` Right T.Float

      it "unifies binary of two 'numeric's as numeric" $ do
        let (res, _cs) = unify' "main = 5 * 6"
        res `shouldBe` Right (T.Var "num" [T.Numeric] 0)

      it "unifies module-level value" $ do
        let (res, _cs) = unify' (Text.unlines ["main = foo * 5", "foo = 6.1"])
        res `shouldBe` Right T.Float

    describe "application" $ do
      it "unifies value reference invocation" $ do
        let (res, _cs) = unify' (Text.unlines ["main = foo(6.1)", "foo = (x) => {x * 5}"] )
        res `shouldBe` Right T.Float

      it "allows functions with type constraints to stay generic" $ do
        let (res, _cs) = unify' (Text.unlines ["main = () => { {float: foo(6.1), numeric: foo(5)} }", "foo = (x) => {x * 5}"] )
        res `shouldBe` Right (T.Fun [] (T.Record [("float", T.Float), ("numeric", T.Var "num" [T.Numeric] 3)]))

      it "allows functions with partial records to stay generic" $ do
        let (res, _cs) = unify' (Text.unlines ["main = () => { {string: foo({abc: \"def\", name: \"foo\"}), float: foo({baz: 2, name: 5.1})} }", "foo = (x) => {", "case x of", "{name: a} -> a", "}"] )
        res `shouldBe` Right (T.Fun [] (T.Record [("string", T.String), ("float", T.Float)]))

      -- it "unifies circular reference" $ do
      --   let (res, _cs) = unify' (Text.unlines ["main = () => { foo() }", "foo = () => { main() }"])
      --   res `shouldBe` Right (T.Fun [] (T.Var "ret" [] 0))
