module MNML.ConstrainSpec
    ( spec
    ) where

import           Control.Monad.Except (runExceptT)
import           Control.Monad.State  (evalState)
import           Data.Map             ((!?))
import qualified Data.Map             as Map
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           MNML.AST.Type        as TAST
import           MNML.Base            (QualifiedTypeReference,
                                       QualifiedValueReference)
import           MNML.CompilerState   (emptyState)
import           MNML.Constrain       (constrain)
import           MNML.Constraint      (Constraint (..))
import           MNML.Parse           (parse)
import qualified MNML.Type            as T
import           SpecHelpers
import           Test.Hspec

-- Helper function to parse and constrain in one step
parseAndConstrain :: Text -> Either Text (TAST.Batch, [Constraint])
parseAndConstrain source =
  case evalState (runExceptT (parse source >>= constrain)) emptyState of
  Left errors  -> Left (Text.pack (show errors))
  Right result -> Right result

expectBatch :: HasCallStack => Either Text (TAST.Batch, [Constraint]) -> ((TAST.Batch, [Constraint]) -> Expectation) -> Expectation
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

expectTypeAliasDef :: HasCallStack => TAST.Batch -> QualifiedTypeReference -> (TAST.TypeAliasDef -> Expectation) -> Expectation
expectTypeAliasDef batch qvr expect =
  case TAST._typeAliasDefs batch !? qvr of
    (Just typeAliasDef) -> expect typeAliasDef
    Nothing -> expectationFailure $ "Expected batch to contain type alias " <> show qvr <> " but it did not"

spec :: Spec
spec = do
  describe "value definitions" $ do
    it "constrains simple integer literal" $ do
      let source = "main = 42"
      expectBatch (parseAndConstrain source) $ \(batch, cs) -> do
        expectValue batch ([], "main") $ \case
          TAST.ValueDef (TAST.ELit (TAST.LInt 42 _) _) _ -> pure ()
          other -> unexpected other
        cs `shouldBe` []

    it "constrains simple string literal" $ do
      let source = "main = \"hello\""
      expectBatch (parseAndConstrain source) $ \(batch, cs) -> do
        expectValue batch ([], "main") $ \case
          TAST.ValueDef (TAST.ELit (TAST.LString "hello" _) _) _ -> pure ()
          other -> unexpected other
        cs `shouldBe` []

    it "constrains binary expression" $ do
      let source = "main = 5.0 + 3.0"
      expectBatch (parseAndConstrain source) $ \(batch, cs) -> do
        expectValue batch ([], "main") $ \case
          TAST.ValueDef (TAST.EBinary TAST.Add (TAST.ELit (TAST.LFloat 5.0 _) _) (TAST.ELit (TAST.LFloat 3.0 _) _) _) _ -> pure ()
          other -> unexpected other

        case cs of
          [CEqual _ (T.Var "ret" _ _) T.Float, CEqual _ (T.Var "ret" _ _) T.Float] -> pure ()
          other -> unexpected other

    it "constrains function application" $ do
      let source = "main = foo(42)"
      expectBatch (parseAndConstrain source) $ \(batch, cs) -> do
        expectValue batch ([], "main") $ \case
          TAST.ValueDef (TAST.EApp (TAST.EVar "foo" _) [TAST.ELit (TAST.LInt 42 _) _] _) _ -> pure ()
          other -> unexpected other

        case cs of
          [CEqual _ (T.Fun [T.Var {}] (T.Var {})) (T.Var "foo" _ _)] -> pure ()
          other                                    -> unexpected other

  describe "type definitions" $ do
    it "converts simple type definition" $ do
      let source = "Maybe = Just(Int) | None"
      expectBatch (parseAndConstrain source) $ \(batch, _cs) -> do
        expectTypeDef batch ([], "Maybe") $ \case
          TAST.TypeDef [TAST.Constructor "Just" _ _, TAST.Constructor "None" [] _] _ -> pure ()
          other -> unexpected other

    it "converts type definition with multiple constructors" $ do
      let source = "Result = Success(String) | Error(Int, String)"
      expectBatch (parseAndConstrain source) $ \(batch, _cs) -> do
        expectTypeDef batch ([], "Result") $ \case
          TAST.TypeDef [TAST.Constructor "Success" [(T.String, _)] _, TAST.Constructor "Error" [(T.Int, _), (T.String, _)] _] _ -> pure ()
          other -> unexpected other

  describe "type alias definitions" $ do
    it "converts simple type alias" $ do
      let source = "alias String as Name"
      expectBatch (parseAndConstrain source) $ \(batch, _cs) -> do
        expectTypeAliasDef batch ([], "Name") $ \case
          TAST.TypeAliasDef T.String _ -> pure ()
          other                        -> unexpected other

  describe "mixed definitions" $ do
    it "constrains multiple definitions together" $ do
      let source = Text.unlines [ "MyType = Value(Int)" , "alias String as Name" , "main = 42" , "add = 1 + 2" ]
      expectBatch (parseAndConstrain source) $ \(batch, _cs) -> do
        Map.size (TAST._typeDefs batch) `shouldBe` 1
        Map.size (TAST._typeAliasDefs batch) `shouldBe` 1
        Map.size (TAST._valueDefs batch) `shouldBe` 2

        expectTypeDef batch ([], "MyType") $ \case
          TAST.TypeDef [TAST.Constructor "Value" _ _] _ -> pure ()
          other -> unexpected other

        expectTypeAliasDef batch ([], "Name") $ \case
          TAST.TypeAliasDef T.String _ -> pure ()
          other -> unexpected other

        expectValue batch ([], "main") $ \case
          TAST.ValueDef (TAST.ELit (TAST.LInt 42 _) _) _ -> pure ()
          other -> unexpected other

        expectValue batch ([], "add") $ \case
          TAST.ValueDef (TAST.EBinary TAST.Add (TAST.ELit (TAST.LInt 1 _) _) (TAST.ELit (TAST.LInt 2 _) _) _) _ -> pure ()
          other -> unexpected other
