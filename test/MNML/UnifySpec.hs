module MNML.UnifySpec
    ( spec
    ) where

import           Control.Monad.State (runState)
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           MNML                (CompilerState (..), emptyState)
import           MNML.AST.Type       (Typed (..))
import qualified MNML.AST.Type       as TAST
import           MNML.Constrain      (TypedValueDecl)
import qualified MNML.Type           as T
import           MNML.Unify
import           SpecHelpers
import           Test.Hspec

unify' :: Text -> (Either [UnificationError] [TypedValueDecl], CompilerState)
unify' source = runState (valueType ("test", "main")) compilerState
  where
    compilerState = emptyState {_modules = Map.fromList [("test", source)]}

spec :: Spec
spec =
  describe "Unification" $ do
    describe "literals" $ do
      it "unifies 'nUmeRiCs'" $ do
        let (res, _) = unify' "main = 42"
        case res of
          Right [(("test", "main"), elit@(TAST.ELit lit@(TAST.LInt 42 _) _))] -> do
            typeOf elit `shouldBe` T.Var "num" (Set.singleton T.Numeric) 0
            typeOf lit `shouldBe` T.Var "num" (Set.singleton T.Numeric) 0
          other -> unexpected other

      it "unifies floats" $ do
        let (res, _) = unify' "main = 3.14"
        case res of
          Right [(("test", "main"), elit@(TAST.ELit lit@(TAST.LFloat 3.14 _) _))] -> do
            typeOf elit `shouldBe` T.Float
            typeOf lit `shouldBe` T.Float
          other -> unexpected other

      it "unifies chars" $ do
        let (res, _) = unify' "main = 'c'"
        case res of
          Right [(("test", "main"), elit@(TAST.ELit lit@(TAST.LChar 'c' _) _))] -> do
            typeOf elit `shouldBe` T.Char
            typeOf lit `shouldBe` T.Char
          other -> unexpected other

      it "unifies strings" $ do
        let (res, _) = unify' "main = \"Hello world\""
        case res of
          Right [(("test", "main"), elit@(TAST.ELit lit@(TAST.LString "Hello world" _) _))] -> do
            typeOf elit `shouldBe` T.String
            typeOf lit `shouldBe` T.String
          other -> unexpected other

    describe "lambdas" $ do
      it "unifies nullary lambdas" $ do
        let (res, _cs) = unify' "main = () => { 1 }"
        case res of
          Right [(("test", "main"), fun@(TAST.ELambda [] (TAST.ELit (TAST.LInt 1 _) _) _))] ->
            typeOf fun `shouldBe` T.Fun [] (T.Var "num" (Set.singleton T.Numeric) 0)
          other -> unexpected other

      it "unifies identity" $ do
        let (res, _cs) = unify' "main = (x) => { x }"
        case res of
          Right [(("test", "main"), fun@(TAST.ELambda ["x"] (TAST.EVar "x" _) _))] ->
            typeOf fun `shouldBe` T.Fun [T.Var "x" Set.empty 0] (T.Var "x" Set.empty 0)
          other -> unexpected other

      it "infers parameter types" $ do
        let (res, _cs) = unify' "main = (x) => { x + 42 }"
        case res of
          Right
            [ ( ("test", "main")
                , fun@(TAST.ELambda ["x"] (TAST.EBinary TAST.Add (TAST.EVar "x" _) (TAST.ELit (TAST.LInt 42 _) _) _) _)
                )
              ] ->
              typeOf fun
                `shouldBe` T.Fun [T.Var "num" (Set.singleton T.Numeric) 1] (T.Var "num" (Set.singleton T.Numeric) 1)
          other -> unexpected other

      it "infers (float) parameter type" $ do
        let (res, _cs) = unify' "main = (x) => { x + 3.14 }"
        case res of
          Right
            [ ( ("test", "main")
                , fun@(TAST.ELambda ["x"] (TAST.EBinary TAST.Add (TAST.EVar "x" _) (TAST.ELit (TAST.LFloat 3.14 _) _) _) _)
                )
              ] ->
              typeOf fun `shouldBe` T.Fun [T.Float] T.Float
          other -> unexpected other

      it "infers parameters and result may be the same" $ do
        let (res, _cs) = unify' "main = (x, y) => { x + y }"
        case res of
          Right
            [ ( ("test", "main")
                , fun@(TAST.ELambda ["x", "y"] (TAST.EBinary TAST.Add (TAST.EVar "x" _) (TAST.EVar "y" _) _) _)
                )
              ] ->
              typeOf fun
                `shouldBe` T.Fun
                  [ T.Var "ret" (Set.singleton T.Numeric) 2
                  , T.Var "ret" (Set.singleton T.Numeric) 2
                  ]
                  (T.Var "ret" (Set.singleton T.Numeric) 2)
          other -> unexpected other

      it "unifies value reference" $ do
        let (res, _cs) =
              unify'
                ( Text.unlines
                    [ "main = () => { foo }"
                    , "foo = 5"
                    ]
                )
        case res of
          Right
            [ (_, mainFun@(TAST.ELambda [] fooRef@(TAST.EVar "foo" _) _))
              , (_, foo@(TAST.ELit (TAST.LInt 5 _) _))
              ] -> do
              typeOf mainFun `shouldBe` T.Fun [] (T.Var "num" (Set.singleton T.Numeric) 2)
              typeOf fooRef `shouldBe` T.Var "num" (Set.singleton T.Numeric) 2
              typeOf foo `shouldBe` T.Var "num" (Set.singleton T.Numeric) 2
          other -> unexpected other

    describe "lists" $ do
      it "unifies empty list" $ do
        let (res, _cs) = unify' "main = []"
        case res of
          Right [(("test", "main"), list@(TAST.EList [] _))] ->
            typeOf list `shouldBe` T.List (T.Var "elem" Set.empty 0)
          other -> unexpected other

      it "unifies singleton list" $ do
        let (res, _cs) = unify' "main = [1]"
        case res of
          Right [(("test", "main"), list@(TAST.EList [TAST.ELit (TAST.LInt 1 _) _] _))] ->
            typeOf list `shouldBe` T.List (T.Var "num" (Set.singleton T.Numeric) 0)
          other -> unexpected other

      it "unifies multiple list" $ do
        let (res, _cs) = unify' "main = [1.0, 2.0]"
        case res of
          Right
            [ ( ("test", "main")
                , list@(TAST.EList [TAST.ELit (TAST.LFloat 1.0 _) _, TAST.ELit (TAST.LFloat 2.0 _) _] _)
                )
              ] ->
              typeOf list `shouldBe` T.List T.Float
          other -> unexpected other

      it "does not unify inconsistent list" $ do
        let (res, _cs) = unify' "main = [1.0, \"Hello\"]"
        case res of
          Left [UnificationError T.Float T.String s] -> shouldSpan s (1, 14) (1, 21)
          other -> unexpected other

    describe "records" $ do
      it "unifies singleton record" $ do
        let (res, _cs) = unify' "main = {foo: \"Bar\"}"
        case res of
          Right [(("test", "main"), rec@(TAST.ERecord _ _))] ->
            typeOf rec `shouldBe` T.Record (Map.fromList [("foo", T.String)])
          other -> unexpected other

      it "unifies multiple field records" $ do
        let (res, _cs) = unify' "main = {foo: \"Bar\", bar: 1.0, baz: 'c'}"
        case res of
          Right [(("test", "main"), rec@(TAST.ERecord _ _))] ->
            typeOf rec `shouldBe` T.Record (Map.fromList [("foo", T.String), ("bar", T.Float), ("baz", T.Char)])
          other -> unexpected other

    describe "constructors" $ do
      it "unifies constructor application" $ do
        let (res, _cs) =
              unify'
                ( Text.unlines
                    [ "MaybeInt = Just(Int) | None"
                    , "main = Just(1)"
                    ]
                )
        case res of
          Right
            [(("test", "main"), app@(TAST.EApp (TAST.EConstructor "Just" _) [TAST.ELit (TAST.LInt 1 _) _] _))] ->
              typeOf app `shouldBe` T.AlgebraicType "MaybeInt"
          other -> unexpected other

      it "unifies raw constructor as function" $ do
        let (res, _cs) =
              unify'
                ( Text.unlines
                    [ "MaybeInt = Just(Int) | None"
                    , "main = Just"
                    ]
                )
        case res of
          Right [(("test", "main"), cons@(TAST.EConstructor "Just" _))] ->
            typeOf cons `shouldBe` T.Fun [T.Int] (T.AlgebraicType "MaybeInt")
          other -> unexpected other

      it "unifies raw, nullary constructor" $ do
        let (res, _cs) =
              unify'
                ( Text.unlines
                    [ "MaybeInt = Just(Int) | None"
                    , "main = None"
                    ]
                )
        case res of
          Right [(("test", "main"), cons@(TAST.EConstructor "None" _))] ->
            typeOf cons `shouldBe` T.AlgebraicType "MaybeInt"
          other -> unexpected other

      it "unifies recursive types" $ do
        let (res, _cs) =
              unify'
                ( Text.unlines
                    [ "IntList = Empty | Cons(Int, IntList)"
                    , "main = Cons(5, Cons(6, Empty))"
                    ]
                )
        case res of
          Right
            [ ( ("test", "main")
                , app@( TAST.EApp
                          (TAST.EConstructor "Cons" _)
                          [ TAST.ELit (TAST.LInt 5 _) _
                            , TAST.EApp (TAST.EConstructor "Cons" _) [TAST.ELit (TAST.LInt 6 _) _, TAST.EConstructor "Empty" _] _
                            ]
                          _
                        )
                )
              ] ->
              typeOf app `shouldBe` T.AlgebraicType "IntList"
          other -> unexpected other

    describe "case" $ do
      it "unifies identity case" $ do
        let (res, _cs) = unify' (Text.unlines ["main = case 4 of", "a -> a"])
        case res of
          Right
            [ ( ("test", "main")
                , ecase@(TAST.ECase (TAST.ELit (TAST.LInt 4 _) _) [(TAST.PVar "a" _, TAST.EVar "a" _)] _)
                )
              ] ->
              typeOf ecase `shouldBe` T.Var "num" (Set.singleton T.Numeric) 0
          other -> unexpected other

      it "unifies two branch case" $ do
        let (res, _cs) =
              unify'
                ( Text.unlines
                    [ "MaybeInt = Just(Int) | None"
                    , "foo = None"
                    , "main = case foo of"
                    , "Just(n) -> n"
                    , "None -> 5"
                    ]
                )
        case res of
          Right
            [ ( ("test", "main")
                , c@( TAST.ECase
                        (TAST.EVar "foo" _)
                        [ (TAST.PConstructor "Just" [TAST.PVar "n" _] _, TAST.EVar "n" _)
                          , (TAST.PConstructor "None" [] _, TAST.ELit (TAST.LInt 5 _) _)
                          ]
                        _
                      )
                )
              , (("test", "foo"), foo@(TAST.EConstructor "None" _))
              ] -> do
              typeOf foo `shouldBe` T.AlgebraicType "MaybeInt"
              typeOf c `shouldBe` T.Int
          other -> unexpected other

      it "unifies branches with different literals for the same record field" $ do
        let (res, _cs) =
              unify'
                ( Text.unlines
                    [ "main = (foo) => {"
                    , "case foo of"
                    , "{foo: \"bar\"} -> 1"
                    , "{foo: \"baz\"} -> 2"
                    , "}"
                    ]
                )
        case res of
          Right
            [ ( ("test", "main")
                , main@( TAST.ELambda
                          ["foo"]
                          ( TAST.ECase
                              (TAST.EVar "foo" _)
                              [ (TAST.PRecord [("foo", TAST.PLiteral (TAST.LString "bar" _) _)] _, TAST.ELit (TAST.LInt 1 _) _)
                                , (TAST.PRecord [("foo", TAST.PLiteral (TAST.LString "baz" _) _)] _, TAST.ELit (TAST.LInt 2 _) _)
                                ]
                              _
                            )
                          _
                        )
                )
              ] ->
              typeOf main
                `shouldBe` T.Fun
                  [T.PartialRecord (Map.fromList [("foo", T.String)]) 9]
                  (T.Var "num" (Set.singleton T.Numeric) 3)
          other -> unexpected other

      it "unifies disjoint record patterns" $ do
        let (res, _cs) =
              unify'
                ( Text.unlines
                    [ "main = (x) => {"
                    , "case x of"
                    , "{foo: \"bar\"} -> 1"
                    , "{bar: 1.0} -> 2"
                    , "}"
                    ]
                )
        case res of
          Right
            [ ( ("test", "main")
                , main@( TAST.ELambda
                          ["x"]
                          ( TAST.ECase
                              (TAST.EVar "x" _)
                              [ (TAST.PRecord [("foo", TAST.PLiteral (TAST.LString "bar" _) _)] _, TAST.ELit (TAST.LInt 1 _) _)
                                , (TAST.PRecord [("bar", TAST.PLiteral (TAST.LFloat 1.0 _) _)] _, TAST.ELit (TAST.LInt 2 _) _)
                                ]
                              _
                            )
                          _
                        )
                )
              ] ->
              typeOf main
                `shouldBe` T.Fun
                  [T.PartialRecord (Map.fromList [("foo", T.String), ("bar", T.Float)]) 9]
                  (T.Var "num" (Set.singleton T.Numeric) 3)
          other -> unexpected other

      it "does not unify inconsistent record patterns" $ do
        let (res, _cs) =
              unify'
                ( Text.unlines
                    [ "main = (foo) => {"
                    , "  case foo of"
                    , "    {foo: \"bar\"} -> 1"
                    , "    {foo: 1.0} -> 2"
                    , "}"
                    ]
                )
        case res of
          -- Ideally this would be a big tighter—just down to the float—but this is good enough for now
          Left [UnificationError T.String T.Float s] -> shouldSpan s (4, 5) (4, 16)
          other -> unexpected other

      it "does not unify non-matching patterns" $ do
        let (res, _cs) =
              unify'
                ( Text.unlines
                    [ "main = (foo) => {"
                    , "  case foo of"
                    , "    3.0 -> \"Fizz\""
                    , "    '5' -> \"Buzz\""
                    , "}"
                    ]
                )
        case res of
          Left [UnificationError T.Float T.Char s] -> return () -- TODO: Fix -- shouldSpan s (4, 5) (4, 8)
          other                                    -> unexpected other

      it "does not unify non-matching return types" $ do
        let (res, _cs) =
              unify'
                ( Text.unlines
                    [ "main = (foo) => {"
                    , "  case foo of"
                    , "    3 -> \"Fizz\""
                    , "    5 -> 'B'"
                    , "}"
                    ]
                )
        case res of
          Left [UnificationError T.String T.Char s] -> return () -- TODO: Fix -- shouldSpan s (4, 10) (4, 13)
          other                                     -> unexpected other

    describe "binary numeric operators" $ do
      it "unifies binary of two floats as floats" $ do
        let (res, _cs) = unify' "main = 5.5 * 6.0"
        case res of
          Right
            [ ( ("test", "main")
                , main@(TAST.EBinary TAST.Mul (TAST.ELit (TAST.LFloat 5.5 _) _) (TAST.ELit (TAST.LFloat 6.0 _) _) _)
                )
              ] ->
              typeOf main `shouldBe` T.Float
          other -> unexpected other

      it "unifies binary of float and 'numeric' as float" $ do
        let (res, _cs) = unify' "main = 5.5 * 6"
        case res of
          Right
            [ ( ("test", "main")
                , main@(TAST.EBinary TAST.Mul (TAST.ELit (TAST.LFloat 5.5 _) _) (TAST.ELit (TAST.LInt 6 _) _) _)
                )
              ] ->
              typeOf main `shouldBe` T.Float
          other -> unexpected other

      it "unifies binary of two 'numeric's as numeric" $ do
        let (res, _cs) = unify' "main = 5 * 6"
        case res of
          Right
            [ ( ("test", "main")
                , main@(TAST.EBinary TAST.Mul (TAST.ELit (TAST.LInt 5 _) _) (TAST.ELit (TAST.LInt 6 _) _) _)
                )
              ] ->
              typeOf main `shouldBe` T.Var "num" (Set.singleton T.Numeric) 0
          other -> unexpected other

      it "unifies module-level value" $ do
        let (res, _cs) = unify' (Text.unlines ["main = foo * 5", "foo = 6.1"])
        case res of
          Right
            [ (("test", "main"), main@(TAST.EBinary TAST.Mul (TAST.EVar "foo" _) (TAST.ELit (TAST.LInt 5 _) _) _))
              , (("test", "foo"), foo@(TAST.ELit (TAST.LFloat 6.1 _) _))
              ] -> do
              typeOf main `shouldBe` T.Float
              typeOf foo `shouldBe` T.Float
          other -> unexpected other

    describe "application" $ do
      it "unifies value reference invocation" $ do
        let (res, _cs) =
              unify'
                ( Text.unlines
                    [ "main = foo(6.1)"
                    , "foo = (x) => {x * 5}"
                    ]
                )
        case res of
          Right
            [ (("test", "main"), main@(TAST.EApp (TAST.EVar "foo" _) [TAST.ELit (TAST.LFloat 6.1 _) _] _))
              , ( ("test", "foo")
                  , foo@(TAST.ELambda ["x"] (TAST.EBinary TAST.Mul (TAST.EVar "x" _) (TAST.ELit (TAST.LInt 5 _) _) _) _)
                  )
              ] -> do
              typeOf main `shouldBe` T.Float
              typeOf foo `shouldBe` T.Fun [T.Float] T.Float
          other -> unexpected other

      it "allows functions with type constraints to stay generic" $ do
        let (res, _cs) =
              unify'
                ( Text.unlines
                    [ "main = () => { {float: foo(6.1), numeric: foo(5)} }"
                    , "foo = (x) => {x * 5}"
                    ]
                )
        case res of
          Right
            [ (("test", "main"), main@(TAST.ELambda [] (TAST.ERecord _ _) _))
              , ( ("test", "foo")
                  , foo1@(TAST.ELambda ["x"] (TAST.EBinary TAST.Mul (TAST.EVar "x" _) (TAST.ELit (TAST.LInt 5 _) _) _) _)
                  )
              , ( ("test", "foo")
                  , foo2@(TAST.ELambda ["x"] (TAST.EBinary TAST.Mul (TAST.EVar "x" _) (TAST.ELit (TAST.LInt 5 _) _) _) _)
                  )
              ] -> do
              typeOf main
                `shouldBe` T.Fun
                  []
                  (T.Record (Map.fromList [("float", T.Float), ("numeric", T.Var "num" (Set.singleton T.Numeric) 3)]))
              typeOf foo1
                `shouldBe` T.Fun [T.Var "num" (Set.singleton T.Numeric) 3] (T.Var "num" (Set.singleton T.Numeric) 3)
              typeOf foo2 `shouldBe` T.Fun [T.Float] T.Float
          other -> unexpected other

      -- The way that this works (generating a new function for each invocation) is decidedly pretty quirky.  It works, for now, however.
      it "allows functions with partial records to stay generic" $ do
        let (res, _cs) =
              unify'
                ( Text.unlines
                    [ "main = () => { {string: foo({abc: \"def\", name: \"foo\"}), float: foo({baz: 2, name: 5.1})} }"
                    , "foo = (x) => {"
                    , "  case x of"
                    , "    {name: a} -> a"
                    , "}"
                    ]
                )
        case res of
          Right
            [ (("test", "main"), main@(TAST.ELambda [] (TAST.ERecord _ _) _))
              , ( ("test", "foo")
                  , foo1@( TAST.ELambda
                            ["x"]
                            (TAST.ECase (TAST.EVar "x" _) [(TAST.PRecord [("name", TAST.PVar "a" _)] _, TAST.EVar "a" _)] _)
                            _
                          )
                  )
              , ( ("test", "foo")
                  , foo2@( TAST.ELambda
                            ["x"]
                            (TAST.ECase (TAST.EVar "x" _) [(TAST.PRecord [("name", TAST.PVar "a" _)] _, TAST.EVar "a" _)] _)
                            _
                          )
                  )
              ] -> do
              typeOf main `shouldBe` T.Fun [] (T.Record (Map.fromList [("string", T.String), ("float", T.Float)]))
              typeOf foo1
                `shouldBe` T.Fun
                  [T.Record (Map.fromList [("baz", T.Var "num" (Set.singleton T.Numeric) 4), ("name", T.Float)])]
                  T.Float
              typeOf foo2
                `shouldBe` T.Fun [T.Record (Map.fromList [("abc", T.String), ("name", T.String)])] T.String
          other -> unexpected other

      it "unifies trivial circular reference" $ do
        let (res, _cs) = unify' (Text.unlines ["main = () => { foo() }", "foo = () => { main() }"])
        case res of
          Right
            [ (("test", "main"), main@(TAST.ELambda [] (TAST.EApp (TAST.EVar "foo" _) [] _) _))
              , (("test", "foo"), foo@(TAST.ELambda [] (TAST.EApp (TAST.EVar "main" _) [] _) _))
              ] -> do
              typeOf main `shouldBe` T.Fun [] (T.Var "ret" Set.empty 1)
              typeOf foo `shouldBe` T.Fun [] (T.Var "ret" Set.empty 1)
          other -> unexpected other

      it "unifies practical circular reference" $ do
        let (res, _cs) =
              unify'
                ( Text.unlines
                    [ "Bool = True | False"
                    , "main = (x) => {"
                    , "  case x of"
                    , "    0 -> True"
                    , "    y -> odd(y - 1)"
                    , "}"
                    , "odd = (x) => {"
                    , "  case x of"
                    , "    1 -> True"
                    , "    y -> main(y - 1)"
                    , "}"
                    ]
                )
        case res of
          Right
            [ ( ("test", "main")
                , main@( TAST.ELambda
                          ["x"]
                          ( TAST.ECase
                              (TAST.EVar "x" _)
                              [ (TAST.PLiteral (TAST.LInt 0 _) _, TAST.EConstructor "True" _)
                                , ( TAST.PVar "y" _
                                    , TAST.EApp
                                        (TAST.EVar "odd" _)
                                        [TAST.EBinary TAST.Sub (TAST.EVar "y" _) (TAST.ELit (TAST.LInt 1 _) _) _]
                                        _
                                    )
                                ]
                              _
                            )
                          _
                        )
                )
              , ( ("test", "odd")
                  , oddFun@( TAST.ELambda
                              ["x"]
                              ( TAST.ECase
                                  (TAST.EVar "x" _)
                                  [ (TAST.PLiteral (TAST.LInt 1 _) _, TAST.EConstructor "True" _)
                                    , ( TAST.PVar "y" _
                                        , TAST.EApp
                                            (TAST.EVar "main" _)
                                            [TAST.EBinary TAST.Sub (TAST.EVar "y" _) (TAST.ELit (TAST.LInt 1 _) _) _]
                                            _
                                        )
                                    ]
                                  _
                                )
                              _
                            )
                  )
              ] -> do
              typeOf main `shouldBe` T.Fun [T.Var "num" (Set.singleton T.Numeric) 1] (T.AlgebraicType "Bool")
              typeOf oddFun `shouldBe` T.Fun [T.Var "num" (Set.singleton T.Numeric) 1] (T.AlgebraicType "Bool")
          other -> unexpected other
