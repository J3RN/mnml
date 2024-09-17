module Inference
    ( Context
    , _bindings
    , infer
    ) where

import qualified Data.Map   as Map
import           Data.Maybe
import           Data.Text
import           Lens.Micro (Lens', lens, over)
import           Parser     (Declaration (..), Expr (..), Literal (..),
                             Type (..), parse)

-- TODO: Name bad, fix later
type Modules = Map.Map Text Text

type Bindings = Map.Map Text Type

data Context
  = Context
      { _modules   :: Modules
      , _bindings  :: Bindings
      , _modName   :: Text
      , _valueName :: Text
      }

bindings :: Lens' Context Bindings
bindings = lens _bindings (\context newBindings -> context {_bindings = newBindings})

-- Infer the type for a value
infer :: Modules -> Text -> Text -> Either Text Type
infer modules modName valueName = do
  ast <- maybe (Left (mconcat [modName, ".", valueName, " not found"])) Right $ loadValueDef modules modName valueName
  inferFromExpr
    ( Context
        { _modules = modules,
          _bindings = Map.empty,
          _modName = modName,
          _valueName = valueName
        }
    )
    ast

inferFromExpr :: Context -> Expr -> Either Text Type
inferFromExpr context (EVar name) =
  maybe
    (Left (mconcat ["Variable ", name, " not found."]))
    Right
    ((Map.!?) (_bindings context) name)
inferFromExpr _context (ELit literal) = Right (litType literal)
-- TODO: We need to be able to refine arg types...
inferFromExpr context (ELambda argNames body) =
  let scopedContext = addBindings context argNames
   in inferFromExpr scopedContext body
inferFromExpr context (EApp funExpr _args) =
  case funExpr of
    lambda@(ELambda _ _) -> inferFromExpr context lambda
    EVar funName ->
      maybe
        ( maybe
            (Left (mconcat ["Function ", funName, " not found"]))
            (inferFromExpr context)
            (loadValueDef (_modules context) (_modName context) funName)
        )
        ( \case
            (TFun _args ret) -> Right ret
            t -> Left (mconcat [funName, " is not a function, it is ", pack . show $ t])
        )
        ((Map.!?) (_bindings context) funName)
    expr -> Left (mconcat [pack . show $ expr, " is not a function"])
inferFromExpr context (ECase _ branches) = maybe (Right TGeneric) (inferFromExpr context . snd) (listToMaybe branches)
inferFromExpr _context (EBinary _ _ _) = Right TInt
inferFromExpr context (ERecord fields) = buildRecType context fields
inferFromExpr context (EList elements) = TList <$> maybe (Right TGeneric) (inferFromExpr context) (listToMaybe elements)

addBindings :: Context -> [Text] -> Context
addBindings context varNames =
  over bindings (Map.union (Map.fromList (Prelude.zip varNames (repeat TGeneric)))) context

litType :: Literal -> Type
litType (LInt _)    = TInt
litType (LFloat _)  = TFloat
litType (LChar _)   = TChar
litType (LString _) = TString

buildRecType :: Context -> [(Text, Expr)] -> Either Text Type
buildRecType context fields =
  TRecord
    <$> mapM
      ( \(name, expr) ->
          (,) name <$> inferFromExpr context expr
      )
      fields

loadValueDef :: Modules -> Text -> Text -> Maybe Expr
loadValueDef modules modName valueName = do
  modAst <- loadModuleAst modules modName
  findValue modAst valueName

loadModuleAst :: Modules -> Text -> Maybe [Declaration]
loadModuleAst modules modName = do
  -- TODO: Check cache, have a cache, etc
  moduleText <- (Map.!?) modules modName
  either (const Nothing) Just (Parser.parse modName moduleText)

findValue :: [Declaration] -> Text -> Maybe Expr
findValue modu valueName =
  -- TODO: Check cache, have a cache, etc
  listToMaybe $
    mapMaybe
      ( \case
          (ValueDecl vn expr) | vn == valueName -> Just expr
          _ -> Nothing
      )
      modu
