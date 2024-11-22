module MNML.Constrain
    ( ConstraintError
    , valueConstraints
    ) where

import           Control.Monad       (foldM)
import           Control.Monad.State (State, StateT, gets, lift, modify,
                                      runStateT)
import           Data.Map            (Map, (!?))
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import           Data.Text           (Text)
import           Lens.Micro          (Lens', lens, over, set)
import           MNML                (CompilerState (..),
                                      QualifiedValueReference, varIdPlusPlus)
import qualified MNML.AST            as AST
import qualified MNML.Parse          as P
import qualified MNML.Type           as T

type Bindings = Map Text T.Type

type PendingType = (QualifiedValueReference, T.Type, AST.SpanAnnotation)

data ConstraintError
  = UnknownConstructor Text -- AST.SourceSpan
  | UnknownType Text -- AST.SourceSpan
  | ParseError P.ParseError
  deriving (Eq, Show)

data ConstrainEnv
  = ConstrainEnv
      { _bindings     :: Bindings
      , _module       :: Text
      , _pendingTypes :: [PendingType]
      , _errors       :: [ConstraintError]
      }

type Constrain a = StateT ConstrainEnv (State CompilerState) a

initialEnv :: Text -> ConstrainEnv
initialEnv modu =
  ConstrainEnv
    { _bindings = Map.empty
    , _module = modu
    , _pendingTypes = []
    , _errors = []
    }

bindings :: Lens' ConstrainEnv Bindings
bindings = lens _bindings (\us bin -> us {_bindings = bin})

pendingTypes :: Lens' ConstrainEnv [PendingType]
pendingTypes = lens _pendingTypes (\us pt -> us {_pendingTypes = pt})

addError :: ConstraintError -> Constrain ()
addError err = modify (\s -> s {_errors = err : _errors s})

giveUp :: ConstraintError -> Text -> Constrain (T.Type, [T.Constraint])
giveUp err name = addError err >> (,[]) <$> freshTypeVar name []

constrain :: AST.Expr AST.SpanAnnotation -> Constrain (T.Type, [T.Constraint])
constrain (AST.EVar name spanA) = do
  lookupRes <- gets ((!? name) . _bindings)
  case lookupRes of
    -- The presence of a var doesn't inform it's type
    Just t -> return (t, [])
    Nothing -> do
      newTVar <- freshTypeVar name []
      modu <- gets _module
      modify (over pendingTypes (((modu, name), newTVar, spanA) :))
      return (newTVar, [])
constrain (AST.EConstructor name _spanA) = do
  modu <- gets _module
  expectedTypeRes <- constructorType modu name
  case expectedTypeRes of
    Left err           -> giveUp err name
    Right expectedType -> return (expectedType, [])
constrain (AST.ELit lit _spanA) =
  (,[]) <$> litType lit
constrain (AST.ELambda args body spanA) = do
  (argVars, bodyType, bodyConstraints) <- withNewScope $ do
    argVars <- mapM declareVar args
    (bt, bc) <- constrain body
    return (argVars, bt, bc)
  retType <- freshTypeVar "fun" []
  return (retType, T.CEqual spanA retType (T.Fun argVars bodyType) : bodyConstraints)
constrain (AST.EApp funExpr argExprs spanA) = do
  (funType, fc) <- constrain funExpr
  argResults <- mapM constrain argExprs
  let argTypes = map fst argResults
      argConstraints = concatMap snd argResults
  retType <- freshTypeVar "ret" []
  return (retType, T.CEqual spanA (T.Fun argTypes retType) funType : fc ++ argConstraints)
constrain (AST.ECase subj branches spanA) = do
  (subjType, subjConstraints) <- constrain subj
  branchRes <- mapM constrainBranch branches
  retType <- freshTypeVar "ret" []
  let (patternRes, clauseRes) = unzip branchRes
      -- The subject type will need to match every pattern
      patternConstraints = map (T.CEqual spanA subjType . fst) patternRes
      patternSubConstraints = concatMap snd patternRes
      -- Every clause type must match the return type of the case expression
      clauseConstraints = map (T.CEqual spanA retType . fst) clauseRes
      clauseSubConstraints = concatMap snd clauseRes
  return
    ( retType
    , concat
        [ patternConstraints
        , clauseConstraints
        , subjConstraints
        , patternSubConstraints
        , clauseSubConstraints
        ]
    )
  where
    constrainBranch (bPattern, bExpr) = withNewScope $ do
      patternConstraints <- constrainPattern bPattern
      clauseConstraints <- constrain bExpr
      return (patternConstraints, clauseConstraints)
constrain (AST.EBinary _op left right spanA) = do
  (lType, lConstraints) <- constrain left
  (rType, rConstraints) <- constrain right
  retVar <- freshTypeVar "ret" [T.Numeric]
  let lConstraint = T.CEqual spanA retVar lType
      rConstraint = T.CEqual spanA retVar rType
  return (retVar, lConstraint : rConstraint : lConstraints ++ rConstraints)
constrain (AST.ERecord fields spanA) = do
  fieldResults <- mapM (constrain . snd) fields
  let typedFields = zip (map fst fields) (map fst fieldResults)
      fieldConstraints = concatMap snd fieldResults
  retType <- freshTypeVar "ret" []
  return (retType, T.CEqual spanA retType (T.Record (Map.fromList typedFields)) : fieldConstraints)
constrain (AST.EList elems spanA) = do
  elemResults <- mapM constrain elems
  elemType <- freshTypeVar "elem" []
  retType <- freshTypeVar "ret" []
  let consistencyConstraints = map (T.CEqual spanA elemType . fst) elemResults
      elemConstraints = concatMap snd elemResults
  return
    (retType, T.CEqual spanA (T.List elemType) retType : consistencyConstraints ++ elemConstraints)

-- Create constraints based on patterns
constrainPattern :: AST.Pattern AST.SpanAnnotation -> Constrain (T.Type, [T.Constraint])
constrainPattern (AST.PVar t _) = (,[]) <$> declareVar t
constrainPattern (AST.PDiscard _) = (,[]) <$> freshTypeVar "_" []
constrainPattern (AST.PConstructor name argPatterns spanA) = do
  modu <- gets _module
  funTypeRes <- constructorType modu name
  case funTypeRes of
    Left err -> giveUp err name
    Right funType@(T.Fun _ _) -> do
      retType <- freshTypeVar name []
      argCons <- mapM constrainPattern argPatterns
      let argTypes = map fst argCons
          argSubCons = concatMap snd argCons
      return (retType, T.CEqual spanA (T.Fun argTypes retType) funType : argSubCons)
    Right retType -> return (retType, [])
constrainPattern (AST.PRecord fieldSpec spanA) = do
  (fieldTypes, fieldConstraints) <- foldM foldRecord ([], []) fieldSpec
  retType <- freshTypeVar "record" []
  partialRecordType <- freshPartialRecord (Map.fromList fieldTypes)
  return (retType, T.CEqual spanA retType partialRecordType : fieldConstraints)
  where
    -- foldRecord ::
    --   ([(Text, T.Type)], [T.Constraint]) ->
    --   (Text, AST.Pattern ) ->
    --   Constrain ([(Text, T.Type)], [T.Constraint])
    foldRecord (fields, fieldCons) (fieldName, fieldPattern) = do
      (fieldType, fieldSubCons) <- constrainPattern fieldPattern
      return ((fieldName, fieldType) : fields, fieldSubCons ++ fieldCons)
constrainPattern (AST.PList elemPats spanA) = do
  elemCons <- mapM constrainPattern elemPats
  elemType <- freshTypeVar "ret" []
  let retCons = map (T.CEqual spanA elemType . fst) elemCons
      elemSubCons = concatMap snd elemCons
  return (elemType, retCons ++ elemSubCons)
constrainPattern (AST.PLiteral lit _) = (,[]) <$> litType lit

freshTypeVar :: Text -> [T.Trait] -> Constrain T.Type
freshTypeVar name traits = T.Var name (Set.fromList traits) <$> varIdPlusPlus

freshPartialRecord :: T.FieldSpec -> Constrain T.Type
freshPartialRecord fields = T.PartialRecord fields <$> varIdPlusPlus

-- Runs a function within its own scope (inheriting the existing scope)
withNewScope :: Constrain a -> Constrain a
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

litType :: AST.Literal AST.SpanAnnotation -> Constrain T.Type
-- For now, an "int" literal (e.g. "5") will be interpreted as "numeric type" (could be float)
litType (AST.LInt _ _)    = freshTypeVar "num" [T.Numeric]
litType (AST.LFloat _ _)  = return T.Float
litType (AST.LChar _ _)   = return T.Char
litType (AST.LString _ _) = return T.String

constructorType :: Text -> Text -> Constrain (Either ConstraintError T.Type)
constructorType modu conName = do
  mDef <- lift (P.moduleDef modu)
  case mDef of
    Left err    -> return (Left (ParseError err))
    Right decls -> findConType decls conName

findConType :: [AST.Declaration AST.SpanAnnotation] -> Text -> Constrain (Either ConstraintError T.Type)
findConType decls conName =
  foldl (<>) (Left (UnknownConstructor conName))
    <$> mapM
      ( \case
          (AST.TypeDecl tName constructors _) -> conTypeFromConstructors conName tName constructors
          _ -> return (Left (UnknownConstructor conName))
      )
      decls

conTypeFromConstructors ::
  Text -> Text -> [(Text, [AST.Type AST.SpanAnnotation])] -> Constrain (Either ConstraintError T.Type)
conTypeFromConstructors conName tName constructors = do
  foldl (<>) (Left (UnknownConstructor conName))
    <$> mapM
      ( \case
          (cName, argTypes) | cName == conName -> typifyConstructor argTypes tName
          _ -> return (Left (UnknownConstructor conName))
      )
      constructors

typifyConstructor :: [AST.Type AST.SpanAnnotation] -> Text -> Constrain (Either ConstraintError T.Type)
typifyConstructor [] tName = return (Right (T.AlgebraicType tName))
typifyConstructor argTypes tName = do
  typifiedTypes <- mapM typify argTypes
  return (T.Fun <$> sequence typifiedTypes <*> return (T.AlgebraicType tName))

typify :: AST.Type AST.SpanAnnotation -> Constrain (Either ConstraintError T.Type)
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
  return (T.Record . Map.fromList <$> sequence fieldTypes)
typify (AST.TVar name _) = Right <$> freshTypeVar name []

moduleNamedType :: Text -> Text -> Constrain (Either ConstraintError T.Type)
moduleNamedType modu typeName = do
  mDefRes <- lift (P.moduleDef modu)
  case mDefRes of
    Right mDef -> findDef typeName mDef
    Left _     -> return (Left (UnknownType typeName))
  where
    findDef :: Text -> [AST.Declaration AST.SpanAnnotation] -> Constrain (Either ConstraintError T.Type)
    findDef name decls =
      foldl (<>) (Left (UnknownType typeName))
        <$> mapM
          ( \case
              (AST.TypeDecl n _ _) | n == name -> return (Right (T.AlgebraicType n))
              (AST.TypeAliasDecl n t _) | n == name -> (T.TypeAlias n <$>) <$> typify t
              _ -> return (Left (UnknownType typeName))
          )
          decls

valueConstraints ::
  Text -> Text -> State CompilerState (Either [ConstraintError] (T.Type, [T.Constraint]))
valueConstraints modu valName = do
  res <$> runStateT (valueConstraints' modu valName) (initialEnv modu)
  where
    res :: ((T.Type, [T.Constraint]), ConstrainEnv) -> Either [ConstraintError] (T.Type, [T.Constraint])
    res (t, ConstrainEnv {_errors = []})   = Right t
    res (_, ConstrainEnv {_errors = errs}) = Left errs

valueConstraints' :: Text -> Text -> Constrain (T.Type, [T.Constraint])
valueConstraints' modu valName = do
  def <- lift (P.valueDef modu valName)
  case def of
    Right expr -> do
      modify (\s -> s {_module = modu, _pendingTypes = []})
      res <- constrain expr
      pTypes <- gets _pendingTypes
      -- TODO: WARNING: This is a trick for circular references but *only works in the same module*
      modify (\s -> s {_bindings = Map.insert valName (fst res) (_bindings s)})
      foldM foldPendingType res pTypes
    Left err -> giveUp (ParseError err) valName
  where
    foldPendingType :: (T.Type, [T.Constraint]) -> PendingType -> Constrain (T.Type, [T.Constraint])
    foldPendingType (t, cs) ((modu', valName'), t', spanA) = do
      (t'', cs') <- valueConstraints' modu' valName'
      return (t, T.CEqual spanA t' t'' : cs' ++ cs)
