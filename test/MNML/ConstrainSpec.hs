module MNML.ConstrainSpec
    ( spec
    ) where

import           Control.Monad.Except (runExceptT)
import           Control.Monad.State  (evalState)
import qualified Data.Map             as Map
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           MNML.AST.Type        as TAST
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

spec :: Spec
spec = do
    describe "value definitions" $ do
      it "constrains simple integer literal" $ do
        let result = parseAndConstrain "main = 42"
        case result of
          Right (batch, constraints) -> do
            case Map.lookup ([], "main") (TAST._valueDefs batch) of
              Just (TAST.ValueDef (TAST.ELit (TAST.LInt 42 _) _) _) -> pure()
              other                                                 -> unexpected other

            case constraints of
                [] -> pure ()
                cs -> unexpected cs
          Left err -> unexpected err

      it "constrains simple string literal" $ do
        let result = parseAndConstrain "main = \"hello\""
        case result of
          Right (batch, constraints) -> do
            case Map.lookup ([], "main") (TAST._valueDefs batch) of
              Just (TAST.ValueDef (TAST.ELit (TAST.LString "hello" _) _) _) -> pure ()
              other -> unexpected other

            case constraints of
                [] -> pure ()
                cs -> unexpected cs
          Left err -> unexpected err

      it "constrains binary expression" $ do
        let result = parseAndConstrain "main = 5.0 + 3.0"
        case result of
          Right (batch, constraints) -> do
            case Map.lookup ([], "main") (TAST._valueDefs batch) of
              Just (TAST.ValueDef (TAST.EBinary TAST.Add (TAST.ELit (TAST.LFloat 5.0 _) _) (TAST.ELit (TAST.LFloat 3.0 _) _) _) _) -> pure ()
              other -> unexpected other

            case constraints of
                [CEqual _ (T.Var "ret" _ _) T.Float, CEqual _ (T.Var "ret" _ _) T.Float] -> pure ()
                other                                      -> unexpected other
          Left err -> unexpected err

      it "constrains function application" $ do
        let result = parseAndConstrain "main = foo(42)"
        case result of
          Right (batch, constraints) -> do
            case Map.lookup ([], "main") (TAST._valueDefs batch) of
              Just (TAST.ValueDef (TAST.EApp (TAST.EVar "foo" _) [TAST.ELit (TAST.LInt 42 _) _] _) _) -> do
                -- Function application should generate constraints
                length constraints `shouldSatisfy` (> 0)
              other -> unexpected other
          Left err -> unexpected err

    describe "type definitions" $ do
      it "constrains simple type definition" $ do
        let result = parseAndConstrain "Maybe = Just(Int) | None"
        case result of
          Right (batch, _) -> do
            case Map.lookup ([], "Maybe") (TAST._typeDefs batch) of
              Just (TAST.TypeDef [TAST.Constructor "Just" _ _, TAST.Constructor "None" [] _] _) -> pure ()
              other -> unexpected other
          Left err -> unexpected err

      it "constrains type definition with multiple constructors" $ do
        let result = parseAndConstrain "Result = Success(String) | Error(Int, String)"
        case result of
          Right (batch, _) -> do
            case Map.lookup ([], "Result") (TAST._typeDefs batch) of
              Just (TAST.TypeDef [TAST.Constructor "Success" _ _, TAST.Constructor "Error" _ _] _) -> pure ()
              other -> unexpected other
          Left err -> unexpected err

    describe "type alias definitions" $ do
      it "constrains simple type alias" $ do
        let result = parseAndConstrain "alias String as Name"
        case result of
          Right (batch, _) -> do
            case Map.lookup ([], "Name") (TAST._typeAliasDefs batch) of
              Just (TAST.TypeAliasDef _ _) -> pure ()
              other                        -> unexpected other
          Left err -> unexpected err

    describe "mixed definitions" $ do
      it "constrains multiple definitions together" $ do
        let source = Text.unlines
              [ "MyType = Value(Int)"
              , "alias String as Name"
              , "main = 42"
              , "add = 1 + 2"
              ]
        let result = parseAndConstrain source
        case result of
          Right (batch, _) -> do
            -- Check that we have all types of definitions
            Map.size (TAST._typeDefs batch) `shouldBe` 1
            Map.size (TAST._typeAliasDefs batch) `shouldBe` 1
            Map.size (TAST._valueDefs batch) `shouldBe` 2

            -- Check specific definitions exist and have correct structure
            case Map.lookup ([], "MyType") (TAST._typeDefs batch) of
              Just (TAST.TypeDef [TAST.Constructor "Value" _ _] _) -> pure ()
              other -> unexpected other

            case Map.lookup ([], "Name") (TAST._typeAliasDefs batch) of
              Just (TAST.TypeAliasDef _ _) -> pure ()
              other                        -> unexpected other

            case Map.lookup ([], "main") (TAST._valueDefs batch) of
              Just (TAST.ValueDef (TAST.ELit (TAST.LInt 42 _) _) _) -> pure ()
              other -> unexpected other

            case Map.lookup ([], "add") (TAST._valueDefs batch) of
              Just (TAST.ValueDef (TAST.EBinary TAST.Add (TAST.ELit (TAST.LInt 1 _) _) (TAST.ELit (TAST.LInt 2 _) _) _) _) -> pure ()
              other -> unexpected other
          Left err -> unexpected err

-- Helper function to check if a constraint is an equality constraint
isEqualityConstraint :: Constraint -> Bool
isEqualityConstraint (CEqual _ _ _) = True
