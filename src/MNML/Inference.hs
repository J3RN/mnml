module MNML.Inference
    ( infer
    ) where

import           Control.Monad.State (State, get)
import           Data.Bifunctor      (first)
import           Data.List           (find)
import qualified Data.Map            as Map
import           Data.Maybe
import           Data.Text           (Text, pack)
import           Lens.Micro          (Lens', lens, over)
import           MNML                (CompilerState (..), Type (..))
import           MNML.Parser         (Declaration (..), Expr (..), Literal (..),
                                      parse)

type Modules = Map.Map Text Text

-- Currently only for variables, maybe more general later
type Bindings = Map.Map Text Type

data Context
  = Context
      { _modules   :: Map.Map Text Text
      , _bindings  :: Bindings
      , _modName   :: Text
      , _valueName :: Text
      }

bindings :: Lens' Context Bindings
bindings = lens _bindings (\context newBindings -> context {_bindings = newBindings})

-- Infer the type for a value
infer :: Text -> Text -> State CompilerState (Either Text Type)
infer modName valueName = do
  CompilerState { _stateModules = modules } <- get
  return $ loadValueDef modules modName valueName
    >>= inferFromExpr
      ( Context
          { _modules = modules,
            _bindings = Map.empty,
            _modName = modName,
            _valueName = valueName
          }
      )

inferFromExpr :: Context -> Expr -> Either Text Type
inferFromExpr context (EVar name _) =
  maybe
    (Left (mconcat ["Variable ", name, " not found."]))
    Right
    ((Map.!?) (_bindings context) name)
inferFromExpr context (EConstructor name _) = constructorType (_modules context) (_modName context) name
inferFromExpr _context (ELit literal _) = Right (litType literal)
-- TODO: We need to be able to refine arg types...
inferFromExpr context (ELambda argNames body _) =
  let scopedContext = addBindings context argNames
   in TFun (map (const TGeneric) argNames) <$> inferFromExpr scopedContext body
inferFromExpr context (EApp funExpr _args _) =
  inferFromExpr context funExpr
    >>= ( \case
            (TFun _args ret) -> Right ret
            expr -> Left (mconcat [pack . show $ expr, " is not a function"])
        )
inferFromExpr context (ECase _ branches _) = maybe (Right TGeneric) (inferFromExpr context . snd) (listToMaybe branches)
inferFromExpr _context (EBinary _ _ _ _) = Right TInt
inferFromExpr context (ERecord fields _) = buildRecType context fields
inferFromExpr context (EList elements _) = TList <$> maybe (Right TGeneric) (inferFromExpr context) (listToMaybe elements)

addBindings :: Context -> [Text] -> Context
addBindings context varNames =
  over bindings (Map.union (Map.fromList (map (,TGeneric) varNames))) context

litType :: Literal -> Type
litType (LInt _ _)    = TInt
litType (LFloat _ _)  = TFloat
litType (LChar _ _)   = TChar
litType (LString _ _) = TString

buildRecType :: Context -> [(Text, Expr)] -> Either Text Type
buildRecType context fields =
  TRecord
    <$> mapM
      ( \(name, expr) ->
          (,) name <$> inferFromExpr context expr
      )
      fields

constructorType :: Modules -> Text -> Text -> Either Text Type
constructorType modules modName constructorName = do
  -- TODO: Check cache, have a cache, etc
  ast <- loadModuleAst modules modName
  case findConstructorType ast constructorName of
    Just expr -> Right expr
    Nothing -> Left (mconcat ["Constructor '", modName, ".", constructorName, "' not found"])

findConstructorType :: [Declaration] -> Text -> Maybe Type
findConstructorType modu constructorName =
  listToMaybe
    ( mapMaybe
        ( \case
            (TypeDecl name constructors _) ->
              ( \case
                  (_, []) -> TNamedType name
                  (_, args) -> TFun args (TNamedType name)
              )
                <$> find ((constructorName ==) . fst) constructors
            _ -> Nothing
        )
        modu
    )

loadValueDef :: Modules -> Text -> Text -> Either Text Expr
loadValueDef modules modName valueName = do
  -- TODO: Check cache, have a cache, etc
  ast <- loadModuleAst modules modName
  case findValue ast valueName of
    Just expr -> Right expr
    Nothing -> Left (mconcat ["Value '", modName, ".", valueName, "' not found"])

loadModuleAst :: Modules -> Text -> Either Text [Declaration]
loadModuleAst modules modName =
  -- TODO: Check cache, have a cache, etc
  case (Map.!?) modules modName of
    Just text -> first (pack . show) $ parse modName text
    Nothing   -> Left (mconcat ["Module '", modName, "' not found"])

findValue :: [Declaration] -> Text -> Maybe Expr
findValue modu valueName =
  listToMaybe $
    mapMaybe
      ( \case
          (ValueDecl vn expr _) | vn == valueName -> Just expr
          _ -> Nothing
      )
      modu
