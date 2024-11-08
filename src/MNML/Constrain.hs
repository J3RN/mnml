module MNML.Constrain
    ( ConstraintError
    , valueConstraints
    ) where

import           Control.Monad       (foldM)
import           Control.Monad.State (State, StateT, evalStateT, gets, lift,
                                      modify, state)
import           Data.Bifunctor      (first)
import           Data.Map            (Map, (!?))
import qualified Data.Map            as Map
import           Data.Maybe          (listToMaybe, mapMaybe)
import           Data.Text           (Text)
import           Lens.Micro          (Lens', lens, over, set)
import           MNML                (CompilerState (..))
import qualified MNML.AST            as AST
import qualified MNML.Parse          as P
import qualified MNML.Type           as T

type Bindings = Map Text T.Type

data ConstraintError
  = UnknownVar AST.NodeId
  | UnknownVal Text
  | UnknownConstructor Text
  | UnknownType Text -- AST.NodeId
  | ArgumentLengthMismatch AST.NodeId
  | OccursError T.Type T.Type AST.NodeId
  | ExpectedTraits T.Type [T.Trait] AST.NodeId
  | ParseError P.ParseError
  deriving (Eq, Show)

data ConstrainEnv
  = ConstrainEnv
      { _bindings  :: Bindings
      , _module    :: Text
      , _nextVarId :: T.VarId
      , _errors    :: [ConstraintError]
      }

initialEnv :: Text -> ConstrainEnv
initialEnv modu =
  ConstrainEnv
    { _bindings = Map.empty,
      _module = modu,
      _nextVarId = 0,
      _errors = []
    }

bindings :: Lens' ConstrainEnv Bindings
bindings = lens _bindings (\us bin -> us {_bindings = bin})

type Constrain a = StateT ConstrainEnv (State CompilerState) a

addError :: ConstraintError -> Constrain ()
addError err = modify (\s -> s {_errors = err : _errors s})

giveUp :: ConstraintError -> Text -> Constrain (T.Type, [T.Constraint])
giveUp err name = addError err >> (,[]) <$> freshTypeVar name []

constrain :: AST.Expr -> Constrain (T.Type, [T.Constraint])
constrain (AST.EVar name nodeId) = do
  lookupRes <- gets ((!? name) . _bindings)
  case lookupRes of
    -- The presence of a var doesn't inform it's type
    Just t -> return (t, [])
    Nothing -> do
      addError (UnknownVar nodeId)
      newTVar <- freshTypeVar name []
      return (newTVar, [])
constrain (AST.EConstructor name _nodeId) = do
  modu <- gets _module
  expectedTypeRes <- constructorType modu name
  case expectedTypeRes of
    Left err           -> giveUp err "name"
    Right expectedType -> return (expectedType, [])
constrain (AST.ELit lit _nodeId) =
  (,[]) <$> litType lit
constrain (AST.ELambda args body nodeId) = do
  (argVars, bodyType, bodyConstraints) <- withNewScope $ do
    argVars <- mapM declareVar args
    (bt, bc) <- constrain body
    return (argVars, bt, bc)
  retType <- freshTypeVar "fun" []
  return (retType, T.CEqual nodeId retType (T.Fun argVars bodyType) : bodyConstraints)
constrain (AST.EApp funExpr argExprs nodeId) = do
  (funType, fc) <- constrain funExpr
  argResults <- mapM constrain argExprs
  let argTypes = map fst argResults
      argConstraints = concatMap snd argResults
  retType <- freshTypeVar "ret" []
  return (retType, T.CEqual nodeId (T.Fun argTypes retType) funType : fc ++ argConstraints)
constrain (AST.ECase subj branches nodeId) = do
  (subjType, subjConstraints) <- constrain subj
  branchRes <- mapM constrainBranch branches
  retType <- freshTypeVar "ret" []
  let (patternRes, clauseRes) = unzip branchRes
      -- The subject type will need to match every pattern
      patternConstraints = map (T.CEqual nodeId subjType . fst) patternRes
      patternSubConstraints = concatMap snd patternRes
      -- Every clause type must match the return type of the case expression
      clauseConstraints = map (T.CEqual nodeId retType . fst) clauseRes
      clauseSubConstraints = concatMap snd clauseRes
  return (retType, concat [patternConstraints, clauseConstraints, subjConstraints, patternSubConstraints, clauseSubConstraints])
  where
    constrainBranch :: (AST.Pattern, AST.Expr) -> Constrain ((T.Type, [T.Constraint]), (T.Type, [T.Constraint]))
    constrainBranch (bPattern, bExpr) = withNewScope $ do
        patternConstraints <- constrainPattern bPattern
        clauseConstraints <- constrain bExpr
        return (patternConstraints, clauseConstraints)
constrain (AST.EBinary _op left right nodeId) = do
  (lType, lConstraints) <- constrain left
  (rType, rConstraints) <- constrain right
  retVar <- freshTypeVar "ret" [T.Numeric]
  let lConstraint = T.CEqual nodeId retVar lType
      rConstraint = T.CEqual nodeId retVar rType
  return (retVar, lConstraint : rConstraint : lConstraints ++ rConstraints)
constrain (AST.ERecord fields nodeId) = do
  fieldResults <- mapM (constrain . snd) fields
  let typedFields = zip (map fst fields) (map fst fieldResults)
      fieldConstraints = concatMap snd fieldResults
  retType <- freshTypeVar "ret" []
  return (retType, T.CEqual nodeId retType (T.Record typedFields) : fieldConstraints)
constrain (AST.EList elems nodeId) = do
  elemResults <- mapM constrain elems
  elemType <- freshTypeVar "elem" []
  retType <- freshTypeVar "ret" []
  let consistencyConstraints = map (T.CEqual nodeId elemType . fst) elemResults
      elemConstraints = concatMap snd elemResults
  return (retType, T.CEqual nodeId (T.List elemType) retType : consistencyConstraints ++ elemConstraints)

-- Create constraints based on patterns
constrainPattern :: AST.Pattern -> Constrain (T.Type, [T.Constraint])
constrainPattern (AST.PVar t _) = (,[]) <$> declareVar t
constrainPattern (AST.PDiscard _) = (,[]) <$> freshTypeVar "_" []
constrainPattern (AST.PConstructor name argPatterns nodeId) = do
  modu <- gets _module
  funTypeRes <- constructorType modu name
  case funTypeRes of
    Left err -> giveUp err "name"
    Right funType@(T.Fun _ _) -> do
      retType <- freshTypeVar name []
      argCons <- mapM constrainPattern argPatterns
      let argTypes = map fst argCons
          argSubCons = concatMap snd argCons
      return (retType, T.CEqual nodeId (T.Fun argTypes retType) funType : argSubCons)
    Right retType -> return (retType, [])
constrainPattern (AST.PRecord fieldSpec nodeId) = do
  (fieldTypes, fieldConstraints) <- foldM foldRecord ([], []) fieldSpec
  retType <- freshTypeVar "record" []
  return (retType, T.CEqual nodeId (T.Record fieldTypes) retType : fieldConstraints)
  where
    foldRecord :: ([(Text, T.Type)], [T.Constraint]) -> (Text, AST.Pattern) -> Constrain ([(Text, T.Type)], [T.Constraint])
    foldRecord (fields, fieldCons) (fieldName, fieldPattern) = do
      (fieldType, fieldSubCons) <- constrainPattern fieldPattern
      return ((fieldName, fieldType) : fields, fieldSubCons ++ fieldCons)
constrainPattern (AST.PList elemPats nodeId) = do
  elemCons <- mapM constrainPattern elemPats
  elemType <- freshTypeVar "ret" []
  let retCons = map (T.CEqual nodeId elemType . fst) elemCons
      elemSubCons = concatMap snd elemCons
  return (elemType, retCons ++ elemSubCons)
constrainPattern (AST.PLiteral lit _) = (,[]) <$> litType lit

freshTypeVar :: Text -> [T.Trait] -> Constrain T.Type
freshTypeVar name traits = state (freshTypeVar' name traits)
  where
    freshTypeVar' n ts s =
      let newTVId = _nextVarId s
       in (T.Var n ts newTVId, s {_nextVarId = newTVId + 1})

-- Runs a function within its own scope (inheriting the existing scope)
withNewScope ::  Constrain a -> Constrain a
withNewScope f = do
  oldBindings <- gets _bindings
  result <- f
  modify (set bindings oldBindings)
  return result

declareVar :: Text -> Constrain T.Type
declareVar name = do
  newVarType <- freshTypeVar name []
  modify (over bindings (Map.insert name newVarType))
  return newVarType

litType :: AST.Literal -> Constrain T.Type
-- For now, an "int" literal (e.g. "5") will be interpreted as "numeric type" (could be float)
litType (AST.LInt _ _)    = freshTypeVar "num" [T.Numeric]
litType (AST.LFloat _ _)  = return T.Float
litType (AST.LChar _ _)   = return T.Char
litType (AST.LString _ _) = return T.String

constructorType :: Text -> Text -> Constrain (Either ConstraintError T.Type)
constructorType modu conName = do
  mDef <- lift (moduleDef modu)
  case mDef of
    Left err    -> return (Left err)
    Right decls -> findConType decls conName

findConType :: [AST.Declaration] -> Text -> Constrain (Either ConstraintError T.Type)
findConType decls conName =
  foldl (<>) (Left (UnknownConstructor conName))
    <$> mapM
      ( \case
          (AST.TypeDecl tName constructors _) -> conTypeFromConstructors conName tName constructors
          _ -> return (Left (UnknownConstructor conName))
      )
      decls

conTypeFromConstructors :: Text -> Text -> [(Text, [AST.Type])] -> Constrain (Either ConstraintError T.Type)
conTypeFromConstructors conName tName constructors = do
  foldl (<>) (Left (UnknownConstructor conName))
    <$> mapM
      ( \case
          (cName, argTypes) | cName == conName -> typifyConstructor argTypes tName
          _ -> return (Left (UnknownConstructor conName))
      )
      constructors

typifyConstructor :: [AST.Type] -> Text -> Constrain (Either ConstraintError T.Type)
typifyConstructor [] tName = return (Right (T.AlgebraicType tName))
typifyConstructor argTypes tName = do
  typifiedTypes <- mapM typify argTypes
  return (T.Fun <$> sequence typifiedTypes <*> return (T.AlgebraicType tName))

findMaybe :: (a -> Maybe b) -> [a] -> Maybe b
findMaybe f = listToMaybe . mapMaybe f

typify :: AST.Type -> Constrain (Either ConstraintError T.Type)
typify (AST.TInt _) = return (Right T.Int)
typify (AST.TFloat _) = return (Right T.Float)
typify (AST.TChar _) = return (Right T.Char)
typify (AST.TString _) = return (Right T.String)
typify (AST.TNamedType name _) = do
  modName <- gets _module
  moduleNamedType modName name
typify (AST.TList t _) = (T.List <$>) <$> typify t
typify (AST.TFun argTypes resType _) = do
  maybeArgTypes <- mapM typify argTypes
  maybeResType <- typify resType
  return (T.Fun <$> sequence maybeArgTypes <*> maybeResType)
-- There might be a more elegant way, not sure
typify (AST.TRecord fields _) = do
  fieldTypes <- mapM (\(fieldName, astType) -> ((fieldName,) <$>) <$> typify astType) fields
  return (T.Record <$> sequence fieldTypes)
typify (AST.TVar name _) = Right <$> freshTypeVar name []

moduleNamedType :: Text -> Text -> Constrain (Either ConstraintError T.Type)
moduleNamedType modu typeName = do
  mDefRes <- lift (moduleDef modu)
  case mDefRes of
    Right mDef -> findDef typeName mDef
    Left _     -> return (Left (UnknownType typeName))
  where
    findDef :: Text -> [AST.Declaration] -> Constrain (Either ConstraintError T.Type)
    findDef name decls =
      foldl (<>) (Left (UnknownType typeName))
        <$> mapM
          ( \case
              (AST.TypeDecl n _ _) | n == name -> return (Right (T.AlgebraicType n))
              (AST.TypeAliasDecl n t _) | n == name -> (T.TypeAlias n <$>) <$> typify t
              _ -> return (Left (UnknownType typeName))
          )
          decls

valueConstraints :: Text -> Text -> State CompilerState (Either ConstraintError (T.Type, [T.Constraint]))
valueConstraints modu valName = do
  def <- valueDef modu valName
  case def of
    Right expr -> Right <$> evalStateT (constrain expr) (initialEnv modu)
    Left err   -> return (Left err)

valueDef :: Text -> Text -> State CompilerState (Either ConstraintError AST.Expr)
valueDef modu valName = do
  mDef <- moduleDef modu
  return (mDef >>= findDef valName)
  where
    findDef name decls =
      maybe
        (Left (UnknownVal name))
        Right
        ( findMaybe
            ( \case
                (AST.ValueDecl n expr _) | n == name -> Just expr
                _ -> Nothing
            )
            decls
        )

-- TODO: Cache module definitions here
moduleDef :: Text -> State CompilerState (Either ConstraintError [AST.Declaration])
moduleDef modu = do
  first ParseError <$> P.parse modu
