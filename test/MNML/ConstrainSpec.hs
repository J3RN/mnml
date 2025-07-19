module MNML.ConstrainSpec
    ( spec
    ) where

import           Control.Monad.Except (runExceptT)
import           Control.Monad.State  (evalState)
import           Data.Map             ((!?))
import qualified Data.Map             as Map
import qualified Data.Set             as Set
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           MNML.AST.Type        as TAST
import           MNML.Base            (QualifiedTypeReference,
                                       QualifiedValueReference)
import           MNML.CompilerState   (emptyState)
import           MNML.Constrain       (constrain)
import           MNML.Constraint      (Constraint (..))
import           MNML.Error           (Error)
import           MNML.Parse           (parse)
import qualified MNML.Type            as T
import           SpecHelpers
import           Test.Hspec

-- Helper function to parse and constrain in one step
parseAndConstrain :: Text -> Either [Error] (TAST.Batch, [Constraint])
parseAndConstrain source =
  case evalState (runExceptT (parse source >>= constrain)) emptyState of
    Left errors  -> Left errors
    Right result -> Right result

expectBatch :: HasCallStack => Either [Error] (TAST.Batch, [Constraint]) -> ((TAST.Batch, [Constraint]) -> Expectation) -> Expectation
expectBatch (Left err)   _      = unexpected err
expectBatch (Right res)  expect = expect res

expectValue :: HasCallStack => TAST.Batch -> QualifiedValueReference -> (TAST.ValueDef -> Expectation) -> Expectation
expectValue batch qvr expect =
  case TAST._valueDefs batch !? qvr of
    (Just val) -> expect val
    Nothing -> expectationFailure $ "Expected batch to contain " <> show qvr <> " but it did not"

expectTypeDef :: HasCallStack => TAST.Batch -> QualifiedTypeReference -> (TAST.TypeDef -> Expectation) -> Expectation
expectTypeDef batch qvr expect =
  case TAST._typeDefs batch !? qvr of
    (Just typeDef) -> expect typeDef
    Nothing -> expectationFailure $ "Expected batch to contain type " <> show qvr <> " but it did not"

spec :: Spec
spec = do
  describe "value definitions" $ do
    it "constrains float literal" $ do
      let source = "main = 2.7"
      expectBatch (parseAndConstrain source) $ \(batch, cs) -> do
        expectValue batch ([], "main") $ \case
          TAST.ValueDef lit@(TAST.ELit float@(TAST.LFloat 2.7 _) _) _ -> do
            nodeType lit `shouldBe` T.Float
            nodeType float `shouldBe` T.Float
          other -> unexpected other
        cs `shouldBe` []

    it "constrains string literal" $ do
      let source = "main = \"hello\""
      expectBatch (parseAndConstrain source) $ \(batch, cs) -> do
        expectValue batch ([], "main") $ \case
          TAST.ValueDef lit@(TAST.ELit string@(TAST.LString "hello" _) _) _ -> do
            nodeType lit `shouldBe` T.String
            nodeType string `shouldBe` T.String
          other -> unexpected other
        cs `shouldBe` []

    it "constrains binary expression" $ do
      let source = "main = 5.0 + 3.0"
      expectBatch (parseAndConstrain source) $ \(batch, cs) -> do
        expectValue batch ([], "main") $ \case
          TAST.ValueDef bin@(TAST.EBinary TAST.Add (TAST.ELit (TAST.LFloat 5.0 _) _) (TAST.ELit (TAST.LFloat 3.0 _) _) _) _ -> do
            case nodeType bin of
              (T.Var "ret" traits _) -> traits `shouldBe` Set.fromList [T.Numeric]
              other                  -> unexpected other

            case cs of
              [CEqual _ lhs@(T.Var "ret" _ _) T.Float, CEqual _ rhs@(T.Var "ret" _ _) T.Float] -> do
                lhs `shouldBe` nodeType bin
                rhs `shouldBe` nodeType bin
              other -> unexpected other
          other -> unexpected other

    it "constrains binary expressions with variables" $ do
      let source = "main = (x) => {x + 5.0}"
      expectBatch (parseAndConstrain source) $ \(batch, cs) -> do
        expectValue batch ([], "main") $ \case
          TAST.ValueDef (TAST.ELambda ["x"] bin@(TAST.EBinary TAST.Add (TAST.EVar "x" _) (TAST.ELit (TAST.LFloat 5.0 _) _) _) _) _ -> do
            case nodeType bin of
              (T.Var "ret" traits _) -> traits `shouldBe` Set.fromList [T.Numeric]
              other                  -> unexpected other

            case cs of
              [  CEqual _ (T.Var "fun" _ _) (T.Fun [xVar@(T.Var "x" _ _)] ret@(T.Var "ret" _ _))
               , CEqual _ lhs@(T.Var "ret" _ _) xVar'@(T.Var "x" _ _)
               , CEqual _ rhs@(T.Var "ret" _ _) T.Float] -> do
                xVar' `shouldBe` xVar
                ret `shouldBe` nodeType bin
                lhs `shouldBe` nodeType bin
                rhs `shouldBe` nodeType bin
              other -> unexpected other
          other -> unexpected other

    it "constrains local references" $ do
      let source = Text.unlines ["main = foo", "foo = 5.0"]
      expectBatch (parseAndConstrain source) $ \(batch, cs) -> do
        expectValue batch ([], "foo") $ \case
          TAST.ValueDef (TAST.ELit (TAST.LFloat _ _) _) _ -> pure()
          other -> unexpected other

        expectValue batch ([], "main") $ \case
          TAST.ValueDef foo@(TAST.EVar "foo" _) _ ->
            case cs of
              [CEqual _ fooType@(T.Var "foo" _ _) T.Float] -> fooType `shouldBe` nodeType foo
              other                                        -> unexpected other
          other -> unexpected other


    -- xit "constrains remote references" $ do
    --   return ()

    it "constrains function application" $ do
      let source = Text.unlines ["main = foo(42)", "foo = (x) => { x }"]
      expectBatch (parseAndConstrain source) $ \(batch, cs) -> do
        expectValue batch ([], "main") $ \case
          TAST.ValueDef (TAST.EApp (TAST.EVar "foo" _) [TAST.ELit (TAST.LInt 42 _) _] _) _ -> pure ()
          other -> unexpected other

        case cs of
          [  CEqual _ (T.Var "foo" _ _) (T.Var "fun" _ _)
           , CEqual _ (T.Fun [T.Var {}] (T.Var {})) (T.Var "foo" _ _)
           , CEqual _ (T.Var "fun" _ _) (T.Fun [T.Var {}] (T.Var {}))] -> pure ()

          other -> unexpected other

    it "constrains constructor invocation" $ do
      let source = Text.unlines ["Maybe = Just(Int) | None", "main = Just(5)"]
      expectBatch (parseAndConstrain source) $ \(batch, cs) -> do
        expectValue batch ([], "main") $ \case
          TAST.ValueDef (TAST.EApp (TAST.EConstructor "Just" _) [TAST.ELit (TAST.LInt 5 _) _] _) _ -> pure ()
          other -> unexpected other

        case cs of
          [CEqual _ (T.Fun [T.Var {}] (T.Var {})) (T.Fun _ _)] -> return ()
          other -> unexpected other

  describe "type definitions" $ do
    it "converts simple type definition" $ do
      let source = "Maybe = Just(Int) | None"
      expectBatch (parseAndConstrain source) $ \(batch, _cs) -> do
        expectTypeDef batch ([], "Maybe") $ \case
          TAST.TypeDef t@(T.AlgebraicType "Maybe" _) _ -> do
            expectValue batch ([], "Just") $ \case
              TAST.ValueDef (TAST.EConstructor "Just" (TAST.SourceSpanType {_type = (T.Fun [T.Int] consT)})) _ -> consT `shouldBe` t
              other -> unexpected other

            expectValue batch ([], "None") $ \case
              TAST.ValueDef (TAST.EConstructor "None" (TAST.SourceSpanType {_type = consT})) _ -> consT `shouldBe` t
              other -> unexpected other

          other -> unexpected other

    it "converts type definition with multiple constructors" $ do
      let source = "Result = Success(String) | Error(Int, String)"
      expectBatch (parseAndConstrain source) $ \(batch, _cs) -> do
        expectTypeDef batch ([], "Result") $ \case
          TAST.TypeDef t@(T.AlgebraicType "Result" _) _ -> do
            expectValue batch ([], "Success") $ \case
              TAST.ValueDef (TAST.EConstructor "Success" (TAST.SourceSpanType {_type = (T.Fun [T.String] consT)})) _ -> consT `shouldBe` t
              other -> unexpected other

            expectValue batch ([], "Error") $ \case
              TAST.ValueDef (TAST.EConstructor "Error" (TAST.SourceSpanType {_type = (T.Fun [T.Int, T.String] consT)})) _ -> consT `shouldBe` t
              other -> unexpected other
          other -> unexpected other


    it "converts recursive type definitions" $ do
      -- Sure, this type is not constructable but it should convert
      let source = Text.unlines ["Foo = Foo(Bar)", "Bar = Bar(Foo)"]
      expectBatch (parseAndConstrain source) $ \(batch, _cs) -> do
        let maybeDefs = (,) <$> Map.lookup ([], "Foo") (_typeDefs batch) <*> Map.lookup ([], "Bar") (_typeDefs batch)
        case maybeDefs of
          Just (TAST.TypeDef fooT _, TAST.TypeDef barT _) -> do
            let maybeVals = (,) <$> Map.lookup ([], "Foo") (_valueDefs batch) <*> Map.lookup ([], "Bar") (_valueDefs batch)
            case maybeVals of
              Just (TAST.ValueDef fooConstructor _, TAST.ValueDef barConstructor _) -> do
                nodeType barConstructor `shouldBe` T.Fun [fooT] barT
                nodeType fooConstructor `shouldBe` T.Fun [barT] fooT
              Nothing -> expectationFailure "Expected constructors Foo and Bar to be defined, but at least one was not"
          Nothing -> expectationFailure "Expected types Foo and Bar to be defined, but at least one was not"

  describe "type alias definitions" $ do
    it "converts simple type alias" $ do
      let source = "alias String as Name"
      expectBatch (parseAndConstrain source) $ \(batch, _cs) -> do
        expectTypeDef batch ([], "Name") $ \case
          TAST.TypeDef T.String _ -> pure ()
          other                   -> unexpected other

  describe "mixed definitions" $ do
    it "constrains multiple definitions together" $ do
      let source = Text.unlines [ "MyType = Value(Int)" , "alias String as Name" , "main = 42" , "add = Value(5)" ]
      expectBatch (parseAndConstrain source) $ \(batch, _cs) -> do
        Map.size (TAST._typeDefs batch) `shouldBe` 2
        Map.size (TAST._valueDefs batch) `shouldBe` 3

        expectTypeDef batch ([], "MyType") $ \case
          TAST.TypeDef myType@(T.AlgebraicType "MyType" _) _ -> do
            expectValue batch ([], "Value") $ \case
              TAST.ValueDef (TAST.EConstructor "Value" (SourceSpanType {_type = (T.Fun [T.Int] retT)})) _ -> do
                retT `shouldBe` myType
              other -> unexpected other

            expectValue batch ([], "add") $ \case
              TAST.ValueDef (TAST.EApp (TAST.EConstructor "Value" (SourceSpanType {_type = (T.Fun [T.Int] retT)})) [TAST.ELit (TAST.LInt 5 _) _] _) _ -> do
                retT `shouldBe` myType
              other -> unexpected other
          other -> unexpected other

        expectTypeDef batch ([], "Name") $ \case
          TAST.TypeDef T.String _ -> pure ()
          other -> unexpected other

        expectValue batch ([], "main") $ \case
          TAST.ValueDef (TAST.ELit (TAST.LInt 42 _) _) _ -> pure ()
          other -> unexpected other

    it "constrains recursive types" $ do
      let source = Text.unlines
                     [ "IntList = Empty | Cons(Int, IntList)"
                     , "main = Cons(5, Cons(6, Empty))"
                     ]
      expectBatch (parseAndConstrain source) $ \(batch, cs) -> do
        expectTypeDef batch ([], "IntList") $ \case
          TAST.TypeDef intListT@(T.AlgebraicType "IntList" _) _ -> do
            expectValue batch ([], "Cons") $ \case
              TAST.ValueDef (TAST.EConstructor "Cons" (SourceSpanType {_type = T.Fun [T.Int, intListT'] intListT''})) _ -> do
                intListT' `shouldBe` intListT
                intListT'' `shouldBe` intListT
              other -> unexpected other

            expectValue batch ([], "Empty") $ \case
              TAST.ValueDef (TAST.EConstructor "Empty" (SourceSpanType {_type = intListT'})) _ -> do
                intListT' `shouldBe` intListT
              other -> unexpected other
          other -> unexpected other

        case cs of
          [CEqual _ (T.Fun [T.Var "num" _ 2, T.Var "ret"     _ 4        ] (T.Var "ret"     _ 5))
                    (T.Fun [T.Int,           T.AlgebraicType "IntList" 0] (T.AlgebraicType "IntList" 0)),
           CEqual _ (T.Fun [T.Var "num" _ 3, T.AlgebraicType "IntList" 0] (T.Var "ret"     _ 4))
                    (T.Fun [T.Int,           T.AlgebraicType "IntList" 0] (T.AlgebraicType "IntList" 0))] -> return ()
          other  -> unexpected other
