module MNML.UnifySpec
    ( spec
    ) where

import           Control.Monad.Except (runExceptT)
import           Control.Monad.State  (evalState)
import           Data.Map             ((!?))
import qualified Data.Map             as Map
import qualified Data.Set             as Set
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           MNML.AST.Type        (nodeType)
import qualified MNML.AST.Type        as TAST
import           MNML.Base            (QualifiedValueReference)
import           MNML.CompilerState   (emptyState)
import           MNML.Constrain       (constrain)
import           MNML.Error           (Error)
import           MNML.Parse           (parse)
import qualified MNML.Type            as T
import           MNML.Unify
import           SpecHelpers
import           Test.Hspec

unify' :: Text -> Either [Error] TAST.Batch
unify' source = evalState (runExceptT (parse source >>= constrain >>= unify)) emptyState

expectBatch :: HasCallStack => Either [Error] TAST.Batch -> (TAST.Batch -> Expectation) -> Expectation
expectBatch (Left errs) _        = unexpected errs
expectBatch (Right batch) expect = expect batch

expectValue :: HasCallStack => Either [Error] TAST.Batch -> QualifiedValueReference -> (TAST.ValueDef -> Expectation) -> Expectation
expectValue res qvr expect = expectBatch res valExpect
  where valExpect batch = case TAST._valueDefs batch !? qvr of
                            Just valDef -> expect valDef
                            Nothing     -> expectationFailure (concat ["Expected batch to have value definition for", show qvr, "but it did not"])

spec :: Spec
spec = do
    describe "literals" $ do
      it "unifies 'nUmeRiCs'" $ do
        expectValue (unify' "main = 42") ([], "main") $ \case
          TAST.ValueDef elit@(TAST.ELit lit@(TAST.LInt 42 _) _) _ -> do
            nodeType elit `shouldBe` T.Var "num" (Set.singleton T.Numeric) 0
            nodeType lit `shouldBe` T.Var "num" (Set.singleton T.Numeric) 0
          other -> unexpected other

      it "unifies floats" $ do
        expectValue (unify' "main = 3.14") ([], "main") $ \case
          TAST.ValueDef elit@(TAST.ELit lit@(TAST.LFloat 3.14 _) _) _ -> do
            nodeType elit `shouldBe` T.Float
            nodeType lit `shouldBe` T.Float
          other -> unexpected other

      it "unifies chars" $ do
        expectValue (unify' "main = 'c'") ([], "main") $ \case
          TAST.ValueDef elit@(TAST.ELit lit@(TAST.LChar 'c' _) _) _ -> do
            nodeType elit `shouldBe` T.Char
            nodeType lit `shouldBe` T.Char
          other -> unexpected other

      it "unifies strings" $ do
        expectValue (unify' "main = \"Hello world\"") ([], "main") $ \case
          TAST.ValueDef elit@(TAST.ELit lit@(TAST.LString "Hello world" _) _) _ -> do
            nodeType elit `shouldBe` T.String
            nodeType lit `shouldBe` T.String
          other -> unexpected other

    describe "lambdas" $ do
      it "unifies nullary lambdas" $ do
        expectValue (unify' "main = () => { 1 }") ([], "main") $ \case
          TAST.ValueDef fun@(TAST.ELambda [] (TAST.ELit (TAST.LInt 1 _) _) _) _ ->
            nodeType fun `shouldBe` T.Fun [] (T.Var "num" (Set.singleton T.Numeric) 0)
          other -> unexpected other

      it "unifies identity" $ do
        expectValue (unify' "main = (x) => { x }") ([], "main") $ \case
          TAST.ValueDef fun@(TAST.ELambda ["x"] (TAST.EVar "x" _) _) _ ->
            nodeType fun `shouldBe` T.Fun [T.Var "x" Set.empty 0] (T.Var "x" Set.empty 0)
          other -> unexpected other

      it "infers parameter types" $ do
        expectValue (unify' "main = (x) => { x + 42 }") ([], "main") $ \case
          TAST.ValueDef fun@(TAST.ELambda ["x"] (TAST.EBinary TAST.Add (TAST.EVar "x" _) (TAST.ELit (TAST.LInt 42 _) _) _) _) _ ->
            nodeType fun
              `shouldBe` T.Fun [T.Var "num" (Set.singleton T.Numeric) 1] (T.Var "num" (Set.singleton T.Numeric) 1)
          other -> unexpected other

      it "infers (float) parameter type" $ do
        expectValue (unify' "main = (x) => { x + 3.14 }") ([], "main") $ \case
          TAST.ValueDef fun@(TAST.ELambda ["x"] (TAST.EBinary TAST.Add (TAST.EVar "x" _) (TAST.ELit (TAST.LFloat 3.14 _) _) _) _) _ ->
            nodeType fun `shouldBe` T.Fun [T.Float] T.Float
          other -> unexpected other

      it "infers parameters and result may be the same" $ do
        expectValue (unify' "main = (x, y) => { x + y }") ([], "main") $ \case
          TAST.ValueDef fun@(TAST.ELambda ["x", "y"] (TAST.EBinary TAST.Add (TAST.EVar "x" _) (TAST.EVar "y" _) _) _) _ ->
            nodeType fun
              `shouldBe` T.Fun
                [ T.Var "ret" (Set.singleton T.Numeric) 2
                , T.Var "ret" (Set.singleton T.Numeric) 2
                ]
                (T.Var "ret" (Set.singleton T.Numeric) 2)
          other -> unexpected other

      it "unifies value reference" $ do
        let source = Text.unlines
              [ "main = () => { foo }"
              , "foo = 5"
              ]
        expectValue (unify' source) ([], "main") $ \case
          TAST.ValueDef mainFun@(TAST.ELambda [] fooRef@(TAST.EVar "foo" _) _) _ -> do
            nodeType mainFun `shouldBe` T.Fun [] (T.Var "num" (Set.singleton T.Numeric) 3)
            nodeType fooRef `shouldBe` T.Var "num" (Set.singleton T.Numeric) 3
            expectValue (unify' source) ([], "foo") $ \case
              TAST.ValueDef foo@(TAST.ELit (TAST.LInt 5 _) _) _ ->
                nodeType foo `shouldBe` T.Var "num" (Set.singleton T.Numeric) 2
              other -> unexpected other
          other -> unexpected other

    describe "lists" $ do
      it "unifies empty list" $ do
        expectValue (unify' "main = []") ([], "main") $ \case
          TAST.ValueDef list@(TAST.EList [] _) _ ->
            nodeType list `shouldBe` T.List (T.Var "elem" Set.empty 0)
          other -> unexpected other

      it "unifies singleton list" $ do
        expectValue (unify' "main = [1]") ([], "main") $ \case
          TAST.ValueDef list@(TAST.EList [TAST.ELit (TAST.LInt 1 _) _] _) _ ->
            nodeType list `shouldBe` T.List (T.Var "num" (Set.singleton T.Numeric) 0)
          other -> unexpected other

      it "unifies multiple list" $ do
        expectValue (unify' "main = [1.0, 2.0]") ([], "main") $ \case
          TAST.ValueDef list@(TAST.EList [TAST.ELit (TAST.LFloat 1.0 _) _, TAST.ELit (TAST.LFloat 2.0 _) _] _) _ ->
            nodeType list `shouldBe` T.List T.Float
          other -> unexpected other

      it "does not unify inconsistent list" $ do
        case unify' "main = [1.0, \"Hello\"]" of
          Left _errs -> return () -- We expect this to fail, exact error structure may have changed
          Right _batch -> expectationFailure "Expected unification to fail but it succeeded"

    describe "records" $ do
      it "unifies singleton record" $ do
        expectValue (unify' "main = {foo: \"Bar\"}") ([], "main") $ \case
          TAST.ValueDef rec@(TAST.ERecord _ _) _ ->
            nodeType rec `shouldBe` T.Record (Map.fromList [("foo", T.String)])
          other -> unexpected other

      it "unifies multiple field records" $ do
        expectValue (unify' "main = {foo: \"Bar\", bar: 1.0, baz: 'c'}") ([], "main") $ \case
          TAST.ValueDef rec@(TAST.ERecord _ _) _ ->
            nodeType rec `shouldBe` T.Record (Map.fromList [("foo", T.String), ("bar", T.Float), ("baz", T.Char)])
          other -> unexpected other

    describe "constructors" $ do
      it "unifies constructor application" $ do
        let source = Text.unlines
              [ "MaybeInt = Just(Int) | None"
              , "main = Just(1)"
              ]
        expectValue (unify' source) ([], "main") $ \case
          TAST.ValueDef app@(TAST.EApp (TAST.EConstructor "Just" _) [TAST.ELit (TAST.LInt 1 _) _] _) _ ->
            nodeType app `shouldBe` T.AlgebraicType "MaybeInt"
          other -> unexpected other

      it "unifies raw constructor as function" $ do
        let source = Text.unlines
              [ "MaybeInt = Just(Int) | None"
              , "main = Just"
              ]
        expectValue (unify' source) ([], "main") $ \case
          TAST.ValueDef cons@(TAST.EConstructor "Just" _) _ ->
            nodeType cons `shouldBe` T.Fun [T.Int] (T.AlgebraicType "MaybeInt")
          other -> unexpected other

      it "unifies raw, nullary constructor" $ do
        let source = Text.unlines
              [ "MaybeInt = Just(Int) | None"
              , "main = None"
              ]
        expectValue (unify' source) ([], "main") $ \case
          TAST.ValueDef cons@(TAST.EConstructor "None" _) _ ->
            nodeType cons `shouldBe` T.AlgebraicType "MaybeInt"
          other -> unexpected other

      it "unifies recursive types" $ do
        let source = Text.unlines
              [ "IntList = Empty | Cons(Int, IntList)"
              , "main = Cons(5, Cons(6, Empty))"
              ]
        expectValue (unify' source) ([], "main") $ \case
          TAST.ValueDef app@(TAST.EApp (TAST.EConstructor "Cons" _) [TAST.ELit (TAST.LInt 5 _) _, TAST.EApp (TAST.EConstructor "Cons" _) [TAST.ELit (TAST.LInt 6 _) _, TAST.EConstructor "Empty" _] _] _) _ ->
            nodeType app `shouldBe` T.AlgebraicType "IntList"
          other -> unexpected other

    describe "case" $ do
      it "unifies identity case" $ do
        let source = Text.unlines ["main = case 4 of", "a -> a"]
        expectValue (unify' source) ([], "main") $ \case
          TAST.ValueDef ecase@(TAST.ECase (TAST.ELit (TAST.LInt 4 _) _) [(TAST.PVar "a" _, TAST.EVar "a" _)] _) _ ->
            nodeType ecase `shouldBe` T.Var "num" (Set.singleton T.Numeric) 0
          other -> unexpected other

      it "unifies two branch case" $ do
        let source = Text.unlines
              [ "MaybeInt = Just(Int) | None"
              , "foo = None"
              , "main = case foo of"
              , "  Just(n) -> n"
              , "  None -> 5"
              ]
        expectValue (unify' source) ([], "main") $ \case
          TAST.ValueDef c@(TAST.ECase (TAST.EVar "foo" _) [(TAST.PConstructor "Just" [TAST.PVar "n" _] _, TAST.EVar "n" _), (TAST.PConstructor "None" [] _, TAST.ELit (TAST.LInt 5 _) _)] _) _ -> do
            nodeType c `shouldBe` T.Int
            expectValue (unify' source) ([], "foo") $ \case
              TAST.ValueDef foo@(TAST.EConstructor "None" _) _ ->
                nodeType foo `shouldBe` T.AlgebraicType "MaybeInt"
              other -> unexpected other
          other -> unexpected other

      it "unifies branches with different literals for the same record field" $ do
        let source = Text.unlines
              [ "main = (foo) => {"
              , "  case foo of"
              , "    {foo: \"bar\"} -> 1"
              , "    {foo: \"baz\"} -> 2"
              , "}"
              ]
        expectValue (unify' source) ([], "main") $ \case
          TAST.ValueDef main@(TAST.ELambda ["foo"] (TAST.ECase (TAST.EVar "foo" _) [(TAST.PRecord [("foo", TAST.PLiteral (TAST.LString "bar" _) _)] _, TAST.ELit (TAST.LInt 1 _) _), (TAST.PRecord [("foo", TAST.PLiteral (TAST.LString "baz" _) _)] _, TAST.ELit (TAST.LInt 2 _) _)] _) _) _ ->
            nodeType main
              `shouldBe` T.Fun
                [T.PartialRecord (Map.fromList [("foo", T.String)]) 9]
                (T.Var "num" (Set.singleton T.Numeric) 3)
          other -> unexpected other

      it "unifies disjoint record patterns" $ do
        let source = Text.unlines
              [ "main = (x) => {"
              , "  case x of"
              , "    {foo: \"bar\"} -> 1"
              , "    {bar: 1.0} -> 2"
              , "}"
              ]
        expectValue (unify' source) ([], "main") $ \case
          TAST.ValueDef main@(TAST.ELambda ["x"] (TAST.ECase (TAST.EVar "x" _) [(TAST.PRecord [("foo", TAST.PLiteral (TAST.LString "bar" _) _)] _, TAST.ELit (TAST.LInt 1 _) _), (TAST.PRecord [("bar", TAST.PLiteral (TAST.LFloat 1.0 _) _)] _, TAST.ELit (TAST.LInt 2 _) _)] _) _) _ ->
            nodeType main
              `shouldBe` T.Fun
                [T.PartialRecord (Map.fromList [("foo", T.String), ("bar", T.Float)]) 9]
                (T.Var "num" (Set.singleton T.Numeric) 3)
          other -> unexpected other

      it "does not unify inconsistent record patterns" $ do
        let source = Text.unlines
              [ "main = (foo) => {"
              , "  case foo of"
              , "    {foo: \"bar\"} -> 1"
              , "    {foo: 1.0} -> 2"
              , "}"
              ]
        case unify' source of
          Left _errs -> return () -- We expect this to fail, exact error structure may have changed
          Right _batch -> expectationFailure "Expected unification to fail but it succeeded"

      it "does not unify non-matching patterns" $ do
        let source = Text.unlines
              [ "main = (foo) => {"
              , "  case foo of"
              , "    3.0 -> \"Fizz\""
              , "    '5' -> \"Buzz\""
              , "}"
              ]
        case unify' source of
          Left _errs -> return () -- We expect this to fail, exact error structure may have changed
          Right _batch -> expectationFailure "Expected unification to fail but it succeeded"

      it "does not unify non-matching return types" $ do
        let source = Text.unlines
              [ "main = (foo) => {"
              , "  case foo of"
              , "    3 -> \"Fizz\""
              , "    5 -> 'B'"
              , "}"
              ]
        case unify' source of
          Left _errs -> return () -- We expect this to fail, exact error structure may have changed
          Right _batch -> expectationFailure "Expected unification to fail but it succeeded"

    describe "binary numeric operators" $ do
      it "unifies binary of two floats as floats" $ do
        expectValue (unify' "main = 5.5 * 6.0") ([], "main") $ \case
          TAST.ValueDef main@(TAST.EBinary TAST.Mul (TAST.ELit (TAST.LFloat 5.5 _) _) (TAST.ELit (TAST.LFloat 6.0 _) _) _) _ ->
            nodeType main `shouldBe` T.Float
          other -> unexpected other

      it "unifies binary of float and 'numeric' as float" $ do
        expectValue (unify' "main = 5.5 * 6") ([], "main") $ \case
          TAST.ValueDef main@(TAST.EBinary TAST.Mul (TAST.ELit (TAST.LFloat 5.5 _) _) (TAST.ELit (TAST.LInt 6 _) _) _) _ ->
            nodeType main `shouldBe` T.Float
          other -> unexpected other

      it "unifies binary of two 'numeric's as numeric" $ do
        expectValue (unify' "main = 5 * 6") ([], "main") $ \case
          TAST.ValueDef main@(TAST.EBinary TAST.Mul (TAST.ELit (TAST.LInt 5 _) _) (TAST.ELit (TAST.LInt 6 _) _) _) _ ->
            nodeType main `shouldBe` T.Var "num" (Set.singleton T.Numeric) 0
          other -> unexpected other

      it "unifies module-level value" $ do
        let source = Text.unlines ["main = foo * 5", "foo = 6.1"]
        expectValue (unify' source) ([], "main") $ \case
          TAST.ValueDef main@(TAST.EBinary TAST.Mul (TAST.EVar "foo" _) (TAST.ELit (TAST.LInt 5 _) _) _) _ -> do
            nodeType main `shouldBe` T.Float
          other -> unexpected other
        expectValue (unify' source) ([], "foo") $ \case
          TAST.ValueDef foo@(TAST.ELit (TAST.LFloat 6.1 _) _) _ ->
            nodeType foo `shouldBe` T.Float
          other -> unexpected other

    describe "application" $ do
      it "unifies value reference invocation" $ do
        let source = Text.unlines
              [ "main = foo(6.1)"
              , "foo = (x) => {x * 5}"
              ]
        expectValue (unify' source) ([], "main") $ \case
          TAST.ValueDef main@(TAST.EApp (TAST.EVar "foo" _) [TAST.ELit (TAST.LFloat 6.1 _) _] _) _ -> do
            nodeType main `shouldBe` T.Float
          other -> unexpected other
        expectValue (unify' source) ([], "foo") $ \case
          TAST.ValueDef foo@(TAST.ELambda ["x"] (TAST.EBinary TAST.Mul (TAST.EVar "x" _) (TAST.ELit (TAST.LInt 5 _) _) _) _) _ ->
            nodeType foo
              `shouldBe` T.Fun [T.Var "num" (Set.singleton T.Numeric) 3] (T.Var "num" (Set.singleton T.Numeric) 3)
          other -> unexpected other

    --   it "allows functions with type constraints to stay generic" $ do
    --     let (res, _cs) =
    --           unify'
    --             ( Text.unlines
    --                 [ "main = () => { {float: foo(6.1), numeric: foo(5)} }"
    --                 , "foo = (x) => {x * 5}"
    --                 ]
    --             )
    --     case res of
    --       Right
    --         [ (("test", "main"), main@(TAST.ELambda [] (TAST.ERecord _ _) _))
    --           , ( ("test", "foo")
    --               , foo@(TAST.ELambda ["x"] (TAST.EBinary TAST.Mul (TAST.EVar "x" _) (TAST.ELit (TAST.LInt 5 _) _) _) _)
    --               )
    --           ] -> do
    --           nodeType main
    --             `shouldBe` T.Fun
    --               []
    --               (T.Record (Map.fromList [("float", T.Float), ("numeric", T.Var "num" (Set.singleton T.Numeric) 3)]))
    --           nodeType foo
    --             `shouldBe` T.Fun [T.Var "num" (Set.singleton T.Numeric) 8] (T.Var "num" (Set.singleton T.Numeric) 8)
    --       other -> unexpected other

    --   it "allows functions with partial records to stay generic" $ do
    --     let (res, _cs) =
    --           unify'
    --             ( Text.unlines
    --                 [ "main = () => { {string: foo({abc: \"def\", name: \"foo\"}), float: foo({baz: 2, name: 5.1})} }"
    --                 , "foo = (x) => {"
    --                 , "  case x of"
    --                 , "    {name: a} -> a"
    --                 , "}"
    --                 ]
    --             )
    --     case res of
    --       Right
    --         [ (("test", "main"), main@(TAST.ELambda [] (TAST.ERecord _ _) _))
    --           , ( ("test", "foo")
    --               , foo@( TAST.ELambda
    --                         ["x"]
    --                         (TAST.ECase (TAST.EVar "x" _) [(TAST.PRecord [("name", TAST.PVar "a" _)] _, TAST.EVar "a" _)] _)
    --                         _
    --                       )
    --               )
    --           ] -> do
    --           nodeType main `shouldBe` T.Fun [] (T.Record (Map.fromList [("string", T.String), ("float", T.Float)]))
    --           nodeType foo
    --             `shouldBe` T.Fun
    --               [T.PartialRecord (Map.fromList [("name", T.Var "a" Set.empty 10)]) 12]
    --               (T.Var "a" Set.empty 10)
    --       other -> unexpected other

    --   it "unifies trivial circular reference" $ do
    --     let (res, _cs) = unify' (Text.unlines ["main = () => { foo() }", "foo = () => { main() }"])
    --     case res of
    --       Right
    --         [ (("test", "main"), main@(TAST.ELambda [] (TAST.EApp (TAST.EVar "foo" _) [] _) _))
    --           , (("test", "foo"), foo@(TAST.ELambda [] (TAST.EApp (TAST.EVar "main" _) [] _) _))
    --           ] -> do
    --           nodeType main `shouldBe` T.Fun [] (T.Var "ret" Set.empty 1)
    --           nodeType foo `shouldBe` T.Fun [] (T.Var "ret" Set.empty 4)
    --       other -> unexpected other

    --   it "unifies practical circular reference" $ do
    --     let (res, _cs) =
    --           unify'
    --             ( Text.unlines
    --                 [ "Bool = True | False"
    --                 , "main = (x) => {"
    --                 , "  case x of"
    --                 , "    0 -> True"
    --                 , "    y -> odd(y - 1)"
    --                 , "}"
    --                 , "odd = (x) => {"
    --                 , "  case x of"
    --                 , "    1 -> True"
    --                 , "    y -> main(y - 1)"
    --                 , "}"
    --                 ]
    --             )
    --     case res of
    --       Right
    --         [ ( ("test", "main")
    --             , main@( TAST.ELambda
    --                       ["x"]
    --                       ( TAST.ECase
    --                           (TAST.EVar "x" _)
    --                           [ (TAST.PLiteral (TAST.LInt 0 _) _, TAST.EConstructor "True" _)
    --                             , ( TAST.PVar "y" _
    --                                 , TAST.EApp
    --                                     (TAST.EVar "odd" _)
    --                                     [TAST.EBinary TAST.Sub (TAST.EVar "y" _) (TAST.ELit (TAST.LInt 1 _) _) _]
    --                                     _
    --                                 )
    --                             ]
    --                           _
    --                         )
    --                       _
    --                     )
    --             )
    --           , ( ("test", "odd")
    --               , oddFun@( TAST.ELambda
    --                           ["x"]
    --                           ( TAST.ECase
    --                               (TAST.EVar "x" _)
    --                               [ (TAST.PLiteral (TAST.LInt 1 _) _, TAST.EConstructor "True" _)
    --                                 , ( TAST.PVar "y" _
    --                                     , TAST.EApp
    --                                         (TAST.EVar "main" _)
    --                                         [TAST.EBinary TAST.Sub (TAST.EVar "y" _) (TAST.ELit (TAST.LInt 1 _) _) _]
    --                                         _
    --                                     )
    --                                 ]
    --                               _
    --                             )
    --                           _
    --                         )
    --               )
    --           ] -> do
    --           nodeType main `shouldBe` T.Fun [T.Var "num" (Set.singleton T.Numeric) 1] (T.AlgebraicType "Bool")
    --           nodeType oddFun `shouldBe` T.Fun [T.Var "num" (Set.singleton T.Numeric) 10] (T.AlgebraicType "Bool")
    --       other -> unexpected other
